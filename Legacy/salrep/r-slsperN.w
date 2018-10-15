&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slsper.w

  Description: Sales by Period

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

DEF TEMP-TABLE tt-report LIKE report
    FIELD dec3 AS DEC FIELD date1 AS DATE.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR tyfdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR tytdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR lyfdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR lytdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

DEFINE TEMP-TABLE tt-cust-sales 
   FIELD  cust-no        AS CHARACTER       
   FIELD op-zero-ty      AS LOGICAL    
   FIELD data-string-ty  AS CHARACTER
   FIELD excel-string-ty AS CHARACTER
   FIELD dTotSales       AS DECIMAL
   FIELD op-zero-ly      AS LOGICAL 
   FIELD data-string-ly  AS CHARACTER
   FIELD excel-string-ly AS CHARACTER   
    .

ASSIGN cTextListToSelect = "Period 1,Period 2,Period 3,Period 4,Period 5,Period 6,Period 7,Period 8,Period 9,Period 10,Period 11,Period 12,TOTAL" 

       cFieldListToSelect = "per1,per2,per3,per4,per5,per6,per7,per8,per9,per10,per11,per12,tot" 

       cFieldLength = "13,13,13,13,13,13,13,13,13,13,13,13,14" 
       cFieldType = "i,i,i,i,i,i,i,i,i,i,i,i,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Period 1,Period 2,Period 3,Period 4,Period 5,Period 6,Period 7,Period 8,Period 9,Period 10,Period 11,Period 12,TOTAL"
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust-no end_cust-no as-of-date tb_last-year tb_prt-cust tb_tot-rnu ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
as-of-date tb_last-year tb_prt-cust tb_tot-rnu sl_avail sl_selected rd-dest ~
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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As Of Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-slsper.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 8.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 7.62.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_last-year AS LOGICAL INITIAL no 
     LABEL "Print Last Year Sales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-cust AS LOGICAL INITIAL yes 
     LABEL "Print Customers w/Zero Balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_tot-rnu AS LOGICAL INITIAL NO 
     LABEL "Sort by Total Revenue?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.57 COL 31 WIDGET-ID 6
     btnCustList AT ROW 1.62 COL 64 WIDGET-ID 8
     begin_cust-no AT ROW 2.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     as-of-date AT ROW 4 COL 28 COLON-ALIGNED HELP
          "Enter As Of Date"
     tb_last-year AT ROW 5.52 COL 29.8
     tb_prt-cust AT ROW 6.48 COL 29.8
     tb_tot-rnu AT ROW 7.48 COL 29.8 WIDGET-ID 58
     sl_avail AT ROW 9.52 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.52 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 9.52 COL 58.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.52 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.52 COL 39 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.57 COL 39 WIDGET-ID 40
     btn_down AT ROW 13.57 COL 39 WIDGET-ID 42
     rd-dest AT ROW 16.05 COL 6 NO-LABEL
     lv-ornt AT ROW 16.29 COL 31 NO-LABEL
     lines-per-page AT ROW 16.29 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 18.19 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 19.14 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.38 COL 30
     tb_excel AT ROW 21.86 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.86 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 22.67 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.76 COL 19
     btn-cancel AT ROW 24.76 COL 57
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.81 COL 58.4 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.81 COL 3.8 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.95 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 15.29 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.81.


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
         TITLE              = "Sales Analysis - Sales by Period"
         HEIGHT             = 26.19
         WIDTH              = 95.8
         MAX-HEIGHT         = 26.76
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 26.76
         VIRTUAL-WIDTH      = 95.8
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_last-year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_tot-rnu:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Analysis - Sales by Period */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales by Period */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As Of Date */
DO:
  assign {&self-name}.
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
  SESSION:SET-WAIT-STATE("general").
  RUN GetSelectionList.
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
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
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


&Scoped-define SELF-NAME tb_last-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_last-year C-Win
ON VALUE-CHANGED OF tb_last-year IN FRAME FRAME-A /* Print Last Year Sales? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust C-Win
ON VALUE-CHANGED OF tb_prt-cust IN FRAME FRAME-A /* Print Customers w/Zero Balance? */
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


&Scoped-define SELF-NAME tb_tot-rnu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-rnu C-Win
ON VALUE-CHANGED OF tb_tot-rnu IN FRAME FRAME-A /* sort by Total Revenue? */
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

  as-of-date = TODAY.
  RUN DisplaySelectionList.
  RUN enable_UI.

  RUN sys/inc/CustListForm.p ( "HR15",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust-no.
  END.

  {methods/nowait.i}

    RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR15',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR15""}

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
                            INPUT 'HR15',
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
                                  INPUT 'HR15').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no as-of-date tb_last-year 
          tb_prt-cust tb_tot-rnu sl_avail sl_selected rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         as-of-date tb_last-year tb_prt-cust tb_tot-rnu sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generate-data C-Win 
PROCEDURE generate-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-mode AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-from-date AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ip-to-date AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER v1 AS INT NO-UNDO.
  DEFINE OUTPUT PARAMETER op-zero AS LOG INIT YES NO-UNDO.
  DEFINE OUTPUT PARAMETER op-data-string AS CHAR FORMAT "X(198)" NO-UNDO.
  DEFINE OUTPUT PARAMETER op-excel-string AS CHAR FORMAT "X(198)" NO-UNDO.
  DEFINE OUTPUT PARAMETER op-amount AS DECIMAL NO-UNDO.

  DEF VAR v-amt     AS   DEC  EXTENT 14 NO-UNDO.
  DEF VAR v-slsp    LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.
  DEF VAR v-slsm    LIKE ar-invl.sman  EXTENT 1 NO-UNDO.
  DEF VAR v-amt1    AS   DEC NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  DEF VAR cDisplay AS cha NO-UNDO.
  DEF VAR cExcelDisplay AS cha NO-UNDO.
  DEF VAR hField AS HANDLE NO-UNDO.
  DEF VAR cTmpField AS CHA NO-UNDO.
  DEF VAR cVarValue AS cha NO-UNDO.
  DEF VAR cExcelVarValue AS cha NO-UNDO.
  DEF VAR cSelectedList AS cha NO-UNDO.
  DEF VAR cFieldName AS cha NO-UNDO.

  EMPTY TEMP-TABLE tt-report.
  cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

  FOR EACH ar-inv WHERE
      ar-inv.company  EQ cocode AND
      ar-inv.inv-date GE ip-from-date AND
      ar-inv.inv-date LE ip-to-date AND
      ar-inv.posted   EQ YES AND
      ar-inv.cust-no EQ cust.cust-no
      NO-LOCK,
      EACH ar-invl WHERE
           ar-invl.x-no EQ ar-inv.x-no AND
           (ar-invl.billable OR NOT ar-invl.misc)
        NO-LOCK:

      DO i = 1 TO 3:
        ASSIGN
         v-amt     = 0
         v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                       cust.sman ELSE ar-invl.sman[i].

        IF i NE 1 AND
           (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

        ASSIGN
         v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                        (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                     ELSE ar-invl.s-pct[i]
         v-amt1    = ar-invl.amt * v-slsp[1] / 100.

        IF v-amt1 EQ ? THEN v-amt1 = 0.

        CREATE tt-report.
        ASSIGN
         tt-report.dec3    = v-amt1
         tt-report.date1   = ar-inv.inv-date.
      END.
   END.

   FOR EACH ar-cash WHERE
       ar-cash.company    EQ cocode AND
       ar-cash.cust-no    EQ cust.cust-no AND
       ar-cash.check-date GE ip-from-date AND
       ar-cash.check-date LE ip-to-date AND
       ar-cash.posted     EQ YES
       NO-LOCK,
       EACH ar-cashl WHERE
            ar-cashl.c-no    EQ ar-cash.c-no AND
            ar-cashl.posted  EQ YES AND
            ar-cashl.memo    EQ YES AND
            CAN-FIND(FIRST account WHERE
                     account.company EQ ar-cashl.company AND
                     account.actnum  EQ ar-cashl.actnum AND
                     account.type    EQ "R")
       NO-LOCK:

     RELEASE ar-invl.

     RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

     IF AVAIL oe-retl THEN
     FIND FIRST ar-invl
         WHERE ar-invl.company EQ cocode
           AND ar-invl.cust-no EQ cust.cust-no
           AND ar-invl.inv-no  EQ ar-cashl.inv-no
           AND ar-invl.i-no    EQ oe-retl.i-no
           AND (ar-invl.billable OR NOT ar-invl.misc)
         NO-LOCK NO-ERROR.

     DO i = 1 TO 3:
       ASSIGN
        v-amt     = 0
        v-slsm[1] = IF (NOT AVAIL ar-invl)                OR
                       (ar-invl.sman[i] EQ "" AND i EQ 1) THEN
                      cust.sman ELSE ar-invl.sman[i].

       IF i NE 1 AND
           (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

       ASSIGN
        v-slsp[1] = IF (NOT AVAIL ar-invl)                OR
                       ar-invl.sman[i] EQ ""              OR
                       (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                    ELSE ar-invl.s-pct[i]
        v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                    v-slsp[1] / 100.

       IF v-amt1 EQ ? THEN v-amt1 = 0.

       CREATE tt-report.
       ASSIGN
        tt-report.dec3    = v-amt1
        tt-report.date1   = ar-cash.check-date.

       IF NOT AVAIL ar-invl THEN LEAVE.
     END.
   END.

   v-amt = 0.

   FOR EACH tt-report
       NO-LOCK:

       IF ip-mode EQ "THIS YEAR" THEN
          DO i = 1 TO 13:
             IF tt-report.date1 GE tyfdate[i] AND
                tt-report.date1 LE tytdate[i] THEN
                v-amt[i] = v-amt[i] + tt-report.dec3.
          END.
       ELSE
          DO i = 1 TO 13:
             IF tt-report.date1 GE lyfdate[i] AND
                tt-report.date1 LE lytdate[i] THEN
                v-amt[i] = v-amt[i] + tt-report.dec3.
          END.
   END.

   /*Total*/
   DO i = 1 TO 13:
      v-amt[14] = v-amt[14] + v-amt[i].
      IF v1 GE i THEN
      DO:
         IF v-amt[i] NE 0 THEN
            op-zero = NO.

         ASSIGN
            op-data-string = op-data-string + STRING(v-amt[i],"$->>>,>>>,>>9") + " ".

         IF tb_excel THEN
            op-excel-string = op-excel-string + '"' + STRING(v-amt[i],"$->>>,>>>,>>9") + '",'.
      END.
      ELSE LEAVE.
   END.

       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "per1"  THEN cVarValue = string(v-amt[1],"$->>>,>>>,>>9") .
                         WHEN "per2"  THEN cVarValue = string(v-amt[2],"$->>>,>>>,>>9").
                         WHEN "per3"  THEN cVarValue = STRING(v-amt[3],"$->>>,>>>,>>9").
                         WHEN "per4"  THEN cVarValue = STRING(v-amt[4],"$->>>,>>>,>>9") .
                         WHEN "per5"  THEN cVarValue = STRING(v-amt[5],"$->>>,>>>,>>9") .
                         WHEN "per6"  THEN cVarValue = STRING(v-amt[6],"$->>>,>>>,>>9") .
                         WHEN "per7"  THEN cVarValue = STRING(v-amt[7],"$->>>,>>>,>>9") .
                         WHEN "per8"  THEN cVarValue = STRING(v-amt[8],"$->>>,>>>,>>9") .
                         WHEN "per9"  THEN cVarValue = STRING(v-amt[9],"$->>>,>>>,>>9") .
                         WHEN "per10"  THEN cVarValue = STRING(v-amt[10],"$->>>,>>>,>>9") .
                         WHEN "per11"  THEN cVarValue = STRING(v-amt[11],"$->>>,>>>,>>9") .
                         WHEN "per12"  THEN cVarValue = STRING(v-amt[12],"$->>>,>>>,>>9") .
                         WHEN "tot"  THEN cVarValue = STRING(v-amt[14],"$->>>>,>>>,>>9") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

           ASSIGN op-data-string = cDisplay .
            IF tb_excel THEN DO:
                   op-excel-string = cExcelDisplay .
             END.
     op-amount = v-amt[14] .
  /* op-data-string = op-data-string + STRING(v-amt[14],"$->>>,>>>,>>9").

   IF tb_excel THEN
      op-excel-string = op-excel-string + '"' + STRING(v-amt[14],"$->>>,>>>,>>9") + '",'.*/

   DOWN.

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
SESSION:SET-WAIT-STATE ("general").
{sys/form/r-topw.f}

def var fcust as ch init "" NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.


DEF VAR v         AS   INT NO-UNDO.
DEF VAR v-ly      AS   INT NO-UNDO.
DEF VAR v-yr      LIKE period.yr NO-UNDO.
DEF VAR v-yr-ly   LIKE period.yr NO-UNDO.
DEF VAR v1        AS   INT NO-UNDO.
DEF VAR v1-ly     AS   INT NO-UNDO.
DEF VAR v2        AS   INT NO-UNDO.
DEF VAR i         AS   INT NO-UNDO.
DEF VAR lv-period-header AS CHAR NO-UNDO.
DEF VAR lv-period-excel-header AS CHAR NO-UNDO.
DEF VAR ly-period-header AS CHAR NO-UNDO.
DEF VAR ly-period-excel-header AS CHAR NO-UNDO.
DEF VAR as-of-date-ly AS DATE NO-UNDO.
DEF VAR op-zero-ty AS LOG NO-UNDO.
DEF VAR op-zero-ly AS LOG NO-UNDO.
DEF VAR cust-string AS CHAR FORMAT "X(40)" NO-UNDO.
DEF VAR ip-header AS CHAR NO-UNDO.
DEF VAR data-string-ty AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR data-string-ly AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR excel-string-ty AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR excel-string-ly AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR ip-mode AS CHAR NO-UNDO.

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
DEF VAR str-line AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE dTotSales AS DECIMAL NO-UNDO .
DEFINE VARIABLE dTotSales2 AS DECIMAL NO-UNDO .
/*{sys/form/r-top5DL3.f} */
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
{custom/statusMsg.i "'Processing...'"} 
  
EMPTY TEMP-TABLE tt-cust-sales .

FORM cust-string FORMAT "X(40)"
     WITH NO-BOX NO-LABELS FRAME custs DOWN STREAM-IO WIDTH 200.

FORM str-tit4 FORMAT "X(198)" SKIP
     str-tit5 FORMAT "X(198)" SKIP 
     data-string-ty FORMAT "X(198)"
     WITH NO-BOX NO-LABELS FRAME custx DOWN STREAM-IO WIDTH 200.

FORM ip-mode FORMAT "X(8)"
     WITH NO-BOX NO-LABELS FRAME mode DOWN STREAM-IO WIDTH 200.

 ASSIGN
  str-tit2 = c-win:title
  {sys/inc/ctrtext.i str-tit2 112}
  fcust = begin_cust-no
  tcust = end_cust-no
  lSelected  = tb_cust-list.

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

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.
 IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

FIND LAST period NO-LOCK
    WHERE period.company EQ cocode
      AND period.pst     LE as-of-date
      AND period.pend    GE as-of-date.

ASSIGN
 v    = period.pnum
 v-yr = period.yr.

FOR EACH period NO-LOCK
    WHERE period.company EQ cocode
      AND period.yr      EQ v-yr
    BY period.pnum DESC:

  v1 = period.pnum.
  LEAVE.
END.

ASSIGN
  i = 1
  lv-period-header = FILL(" ",5).

FOR EACH period WHERE
    period.company EQ cocode AND
    period.yr      EQ v-yr
    NO-LOCK
    BY period.pnum:

  ASSIGN
     tyfdate[i] = period.pst
     tytdate[i] = period.pend
     i = i + 1
     lv-period-header = lv-period-header
                      + "Period " + STRING(period.pnum)
                      + (IF period.pnum LE 8 THEN FILL(" ",6)
                         ELSE FILL(" ",5)).

   IF tb_excel THEN
      lv-period-excel-header = lv-period-excel-header
                            + '"' + "Period " + STRING(period.pnum) + '",'.
END.

ASSIGN
   lv-period-header = lv-period-header + "    TOTAL".

IF tb_excel THEN
   lv-period-excel-header = lv-period-excel-header
                          + '"' + "TOTAL" + '",'.

IF tb_last-year THEN
DO:
   as-of-date-ly = DATE(MONTH(as-of-date), DAY(as-of-date), YEAR(as-of-date) - 1).

   FIND LAST period WHERE
        period.company EQ cocode AND
        period.pst     LE as-of-date-ly AND
        period.pend    GE as-of-date-ly
        NO-LOCK NO-ERROR.

   ASSIGN
      v-ly    = period.pnum
      v-yr-ly = period.yr.

   FOR EACH period WHERE
       period.company EQ cocode AND
       period.yr      EQ v-yr-ly
       NO-LOCK
       BY period.pnum DESC:

       v1-ly = period.pnum.
       LEAVE.
   END.

   ASSIGN
      i = 1
      ly-period-header = FILL(" ",5).

   FOR EACH period WHERE
       period.company EQ cocode AND
       period.yr      EQ v-yr-ly
       NO-LOCK
       BY period.pnum:

       ASSIGN
          lyfdate[i] = period.pst
          lytdate[i] = period.pend
          i = i + 1
          ly-period-header = ly-period-header
                           + "Period " + STRING(period.pnum)
                           + (IF period.pnum LE 8 THEN FILL(" ",6)
                              ELSE FILL(" ",5)).

        IF tb_excel THEN
           ly-period-excel-header = ly-period-excel-header
                                  + '"' + "Period " + STRING(period.pnum) + '",'.
   END.

   ASSIGN
      ly-period-header = ly-period-header + "    TOTAL".

   IF tb_excel THEN
      ly-period-excel-header = ly-period-excel-header
                             + '"' + "TOTAL" + '",'.
END.


PUT SPACE(58) "Sales By Period" SKIP
    SPACE(58) "As of " as-of-date SKIP.

FOR each cust FIELDS(NAME cust-no sman) WHERE
    cust.company eq cocode 
    AND cust.cust-no GE fcust
    AND cust.cust-no LE tcust
    AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
    AND ttCustList.log-fld no-lock) else true)
    AND cust.active NE "I"
    no-lock:

    {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

    op-zero-ly = YES.

    RUN generate-data(INPUT "THIS YEAR",
                      INPUT tyfdate[1],
                      INPUT as-of-date,
                      INPUT v1,
                      OUTPUT op-zero-ty,
                      OUTPUT data-string-ty,
                      OUTPUT excel-string-ty,
                      OUTPUT dTotSales ).

    IF tb_last-year THEN
       RUN generate-data(INPUT "LAST YEAR",
                         INPUT lyfdate[1],
                         INPUT as-of-date-ly,
                         INPUT v1-ly,
                         OUTPUT op-zero-ly,
                         OUTPUT data-string-ly,
                         OUTPUT excel-string-ly,
                         OUTPUT dTotSales2).

     CREATE tt-cust-sales .
         ASSIGN
             tt-cust-sales.cust-no         = cust.cust-no 
             tt-cust-sales.op-zero-ty      = op-zero-ty
             tt-cust-sales.data-string-ty  = data-string-ty
             tt-cust-sales.excel-string-ty = excel-string-ty
             tt-cust-sales.dTotSales       = dTotSales
             tt-cust-sales.op-zero-ly      = op-zero-ly
             tt-cust-sales.data-string-ly  = data-string-ly
             tt-cust-sales.excel-string-ly = excel-string-ly .
         IF NOT tb_tot-rnu THEN
                tt-cust-sales.dTotSales = 0 .



   /* IF tb_prt-cust OR (NOT tb_prt-cust AND NOT(op-zero-ty AND op-zero-ly)) THEN
    DO:
       DISPLAY cust.NAME + "/" + cust.cust-no @ cust-string SKIP(1) WITH FRAME custs.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' cust.NAME + "/" + cust.cust-no '",'
              SKIP(1).

       DISPLAY "THIS YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "THIS YEAR" '",'
              SKIP(1)
              excelheader SKIP
              excel-string-ty SKIP(1).

       /*DISPLAY lv-period-header @ ip-header FORM "X(198)" SKIP data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.*/
       DISPLAY str-tit4 SKIP
           str-tit5 SKIP
           data-string-ty FORM "X(198)" skip(1) WITH FRAME custx .

       IF tb_last-year THEN
       DO:
          DISPLAY "LAST YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' "LAST YEAR" '",'
                 SKIP(1)
                 excelheader SKIP.

           /*DISPLAY ly-period-header @ ip-header FORM "X(198)" SKIP data-string-ly @ data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.*/
            DISPLAY str-tit4 SKIP
                str-tit5 SKIP
               data-string-ly @ data-string-ty FORM "X(198)" skip(1) WITH FRAME custx .

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  excel-string-ly SKIP(1).
       END.
    END.*/
end.

FOR EACH tt-cust-sales NO-LOCK BREAK BY tt-cust-sales.dTotSales DESC :
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode 
          AND cust.cust-no EQ tt-cust-sales.cust-no  NO-ERROR .

     IF tb_prt-cust OR (NOT tb_prt-cust AND NOT(tt-cust-sales.op-zero-ty AND tt-cust-sales.op-zero-ly)) THEN
    DO:
       DISPLAY cust.NAME + "/" + cust.cust-no @ cust-string SKIP(1) WITH FRAME custs.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' cust.NAME + "/" + cust.cust-no '",'
              SKIP(1).

       DISPLAY "THIS YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "THIS YEAR" '",'
              SKIP(1)
              excelheader SKIP
              tt-cust-sales.excel-string-ty  SKIP(1).
      
       DISPLAY str-tit4 SKIP
           str-tit5 SKIP
           tt-cust-sales.data-string-ty @ data-string-ty FORM "X(198)" skip(1) WITH FRAME custx .

       IF tb_last-year THEN
       DO:
          DISPLAY "LAST YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' "LAST YEAR" '",'
                 SKIP(1)
                 excelheader SKIP.
      
            DISPLAY str-tit4 SKIP
                str-tit5 SKIP
               tt-cust-sales.data-string-ly @ data-string-ty FORM "X(198)" skip(1) WITH FRAME custx .

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  tt-cust-sales.excel-string-ly SKIP(1).
       END.
    END.
END. /* for each tt-cust-sales*/

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

