&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
DEF VAR list-name AS cha NO-UNDO.
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

ASSIGN
 cocode = gcompany
 locode = gloc.


/*{sys/inc/custlistform.i ""IR6"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report FIELD qty AS DEC
                                     FIELD tt-date AS DATE
                                     FIELD rct-date AS DATE
                                     FIELD ship-date AS DATE
                                     FIELD cust-no AS cha.
DEF TEMP-TABLE tt-itemfg
    FIELD cust-no    AS CHAR
    FIELD i-no       AS CHAR 
    FIELD itemfg-row AS ROWID
    FIELD slsrep     AS CHAR
    INDEX i1 cust-no    i-no
    INDEX i2 itemfg-row.                                     

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

/*DEF TEMP-TABLE tt-report LIKE report
    FIELD q-onh  LIKE itemfg.q-onh
    FIELD q-shp  LIKE itemfg.q-onh
    FIELD q-wip  LIKE itemfg.q-onh
    FIELD po-no  LIKE oe-ord.po-no
    FIELD inv    AS   LOG
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD row-id AS ROWID
    INDEX row-id row-id.
*/
DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Cust #,P.O. NUMBER,ITEM #,CUST PART #,DESCRIPTION,JOB NUMBER,QTY ON-HAND,PALLETS,SELL PRICE," +
                           "TOTAL VALUE,REQ DATE,ORDER DATE,REC DATE,LAST SHIP DATE,REP"
       cFieldListToSelect = "cust,po,item,part,desc,job,qty-hand,pall,sell-pr," +
                            "tot-val,q-date,ord-date,rec-date,ship-date,rep"
       cFieldLength = "8,15,15,15,30,10,11,7,10," + "14,8,10,8,14,3"
       cFieldType = "c,c,c,c,c,c,i,c,i," + "i,c,c,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Cust #,P.O. NUMBER,ITEM #,DESCRIPTION,JOB NUMBER,QTY ON-HAND,PALLETS,SELL PRICE," +
                           "TOTAL VALUE,ORDER DATE,REC DATE,LAST SHIP DATE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 RECT-7 RECT-8 as-of-date ~
tb_cust-list btnCustList begin_cust end_cust begin_cust-po end_cust-po ~
begin_slm end_slm tb_inc-zer tb_inc-cust fi_days-old rd_ascdsc sl_avail ~
Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date tb_cust-list begin_cust ~
end_cust begin_cust-po end_cust-po begin_slm end_slm tb_inc-zer tb_inc-cust ~
fi_days-old lbl_srt rd_ascdsc sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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
DEFINE BUTTON btn-cancel /*AUTO-END-KEY*/
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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Older Than" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cusitm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_srt AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By Receipt Date?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_ascdsc AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ascending", "A",
"Descending", "D"
     SIZE 31.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-8
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
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     as-of-date AT ROW 2 COL 27 COLON-ALIGNED
     tb_cust-list AT ROW 3.05 COL 30.4 WIDGET-ID 6
     btnCustList AT ROW 3.1 COL 62.4 WIDGET-ID 8
     begin_cust AT ROW 4.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 4.14 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-po AT ROW 5.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer PO Number"
     end_cust-po AT ROW 5.1 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_slm AT ROW 6.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 6.05 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_inc-zer AT ROW 7.38 COL 15.2
     tb_inc-cust AT ROW 8.24 COL 56.2 RIGHT-ALIGNED
     fi_days-old AT ROW 8.95 COL 78 COLON-ALIGNED
     lbl_srt AT ROW 9.48 COL 3 COLON-ALIGNED NO-LABEL
     rd_ascdsc AT ROW 9.52 COL 29.6 NO-LABEL
     sl_avail AT ROW 11.71 COL 4.8 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.71 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 11.71 COL 60.2 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 12.71 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 13.71 COL 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.76 COL 40.8 WIDGET-ID 40
     btn_down AT ROW 15.76 COL 40.8 WIDGET-ID 42
     rd-dest AT ROW 18.29 COL 6 NO-LABEL
     lv-ornt AT ROW 18.29 COL 29 NO-LABEL
     lines-per-page AT ROW 18.29 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 19.71 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 20.67 COL 26 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.05 COL 28
     tb_excel AT ROW 23.05 COL 65.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.05 COL 87.4 RIGHT-ALIGNED
     fi_file AT ROW 24 COL 43.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.67 COL 18
     btn-cancel AT ROW 25.67 COL 61.4
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11 COL 60.2 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11 COL 5.6 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Only Show QOH that is..." VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 8 COL 65
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.33 COL 4.6
     RECT-41 AT ROW 7.43 COL 62
     RECT-7 AT ROW 17.1 COL 1
     RECT-8 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Finished Goods Sales Value By Customer By Receipt Date"
         HEIGHT             = 26.33
         WIDTH              = 95.6
         MAX-HEIGHT         = 34.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.29
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_srt IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_srt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ascdsc".

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

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-zer:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Finished Goods Sales Value By Customer By Receipt Date */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Sales Value By Customer By Receipt Date */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po C-Win
ON LEAVE OF begin_cust-po IN FRAME FRAME-A /* Beginning Customer PO# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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
  SESSION:SET-WAIT-STATE("general").
  ASSIGN {&displayed-objects}.
  RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END.
  RUN run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust
                            &END_cust=END_cust
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust
                             &END_cust=end_cust
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust
                                  &END_cust=end_cust
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN RUN output-to-port.
  END CASE. 

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
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po C-Win
ON LEAVE OF end_cust-po IN FRAME FRAME-A /* Ending Customer PO# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
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

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-zer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-zer C-Win
ON VALUE-CHANGED OF tb_inc-zer IN FRAME FRAME-A /* Include Zero Quantity On Hand? */
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

  RUN sys/inc/CustListForm.p ( "IR6",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    as-of-date:SCREEN-VALUE = STRING(TODAY).
    APPLY "entry" TO as-of-date.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR6',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR6""}

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
                            INPUT 'IR6',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qoh C-Win 
PROCEDURE calc-qoh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-qty AS DEC.

  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC                EXTENT 6.
  DEF VAR v-qohi        LIKE v-qohj.
  DEF VAR v-qty         AS   INT.
  DEF VAR v-qty1        LIKE v-qty.
  DEF VAR v-qtyc        LIKE v-qty.
  DEF VAR v-red         LIKE v-qty.
  DEF VAR v             AS   INT.
  DEF VAR v-val         AS   DEC                EXTENT 4.
  DEF VAR v-cst         AS   DEC                EXTENT 4.
  DEF VAR v-u-val       AS   DEC.
  DEF VAR v-u-cst       AS   DEC.
  DEF VAR v-date        AS   DATE.
  DEF VAR lv-tag        LIKE fg-rdtlh.tag NO-UNDO.
  DEF VAR ld-last       AS   DATE NO-UNDO.

  DEF BUFFER b-f-rc FOR fg-rcpth.
  DEF BUFFER b-f-rd FOR fg-rdtlh.

  ASSIGN
   vdat     = as-of-date
   v-curr   = YES
   v-q-or-v = YES.

  /*
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-report.key-06
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
    */

    IF fi_days-old NE 0 THEN DO:
      /*tt-fg-bin.qty = 0.*/
      ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.

      FOR EACH fg-rcpth
          WHERE fg-rcpth.company    EQ fg-bin.company
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.trans-date LE as-of-date 
          NO-LOCK USE-INDEX tran,

          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no       EQ fg-rcpth.r-no
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.cust-no    EQ fg-bin.cust-no
            AND fg-rdtlh.rita-code  EQ fg-rcpth.rita-code
          NO-LOCK

          BREAK BY fg-rdtlh.loc
                BY fg-rdtlh.loc-bin
                BY fg-rdtlh.tag
                BY fg-rdtlh.cust-no
                BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:

        /*IF FIRST(fg-bin.i-no) THEN
          ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.
        */

        {fg/rep/fg-aging.i fi_days-old}

        v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                 v-qohj[4] + v-qohj[5] + v-qohj[6].      

        /*if v-qohj[6] lt 0 then do:
            v-qty = v-qohj[6] * -1.

            do v = 5 to 1 by -1:
              if v-qohj[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.

              if v-qty le 0 then leave.
            end.

            if v-qty gt 0 then v-qohi[6] = v-qohi[6] - v-qty.
        END.*/

            RELEASE oe-ordl.
            IF fg-bin.job-no NE "" THEN
            FIND LAST oe-ordl
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.job-no  EQ fg-bin.job-no
                AND oe-ordl.job-no2 EQ fg-bin.job-no2
                AND oe-ordl.i-no    EQ fg-rcpth.i-no
              USE-INDEX job NO-LOCK NO-ERROR.

            IF NOT v-curr THEN
            ASSIGN
             v-qohj[1] = 0
             v-qohj[2] = 0
             v-qohj[3] = 0.

          IF fg-rcpth.rita-code EQ "C" THEN v-qohi = 0.

          ASSIGN
           v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
           v-qohi[1] = v-qohi[1] + v-qohj[1]
           v-qohi[2] = v-qohi[2] + v-qohj[2]
           v-qohi[3] = v-qohi[3] + v-qohj[3]
           v-qohi[4] = v-qohi[4] + v-qohj[4]
           v-qohi[5] = v-qohi[5] + v-qohj[5]
           v-qohi[6] = v-qohi[6] + v-qohj[6]
           v-qohj    = 0.

          IF AVAIL oe-ordl THEN
            ASSIGN
             v-u-cst  = oe-ordl.t-cost / oe-ordl.qty
             v-u-val  = oe-ordl.t-price / oe-ordl.qty.

          ELSE DO:
            IF itemfg.prod-uom EQ "EA" THEN
              v-u-cst = itemfg.total-std-cost.
            ELSE
              RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                   itemfg.total-std-cost, OUTPUT v-u-cst).

            IF itemfg.sell-uom EQ "EA" THEN
              v-u-val = itemfg.sell-price.
            ELSE
              RUN sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                     itemfg.sell-price, OUTPUT v-u-val).
          END.

          IF v-u-cst EQ ? THEN v-u-cst = 0.
          IF v-u-val EQ ? THEN v-u-val = 0.

          ASSIGN
           v-cst[1] = v-cst[1] + (v-qty * v-u-cst)
           v-val[1] = v-val[1] + (v-qty * v-u-val).

          IF v-qohi[6] LT 0 THEN DO:
            ASSIGN
             v-qty     = v-qohi[6] * -1
             v-qohi[6] = 0.

            DO v = 5 TO 1 BY -1:
              IF v-qohi[v] GT 0 THEN
                ASSIGN
                 v-red     = min(v-qty,v-qohi[v])
                 v-qohi[v] = v-qohi[v] - v-red
                 v-qty     = v-qty     - v-red.

              IF v-qty LE 0 THEN LEAVE.
            END.

            IF v-qty GT 0 THEN
              ASSIGN
               v-qohi   = 0
               v-cst[1] = 0
               v-val[1] = 0.
          END. 

          IF v-cst[1] LT 0 THEN v-cst[1] = 0.
          IF v-val[1] LT 0 THEN v-val[1] = 0.

          IF NOT v-q-or-v THEN DO:
            v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].

            DO v = 1 TO 5:
               v-qohi[v] = v-val[1] / v-qty * v-qohi[v].             
               IF v-qohi[v] EQ ? THEN v-qohi[v] = 0.
            END.
          END.
      END.

      IF v-qohi[1] LT 0 THEN DO:
        v-qty = v-qohi[1] * -1.

        DO v = 5 TO 2 BY -1:
          IF v-qohi[v] GT 0 THEN
            ASSIGN
             v-red     = MIN(v-qty,v-qohi[v])
             v-qohi[v] = v-qohi[v] - v-red
             v-qty     = v-qty     - v-red.

          IF v-qty LE 0 THEN LEAVE.
        END.
      END.

        op-qty /*tt-fg-bin.qty*/ = v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
    END.

    /*
  END. /* for each itemfg*/
     */
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
                                  INPUT 'IR6').


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
  DISPLAY as-of-date tb_cust-list begin_cust end_cust begin_cust-po end_cust-po 
          begin_slm end_slm tb_inc-zer tb_inc-cust fi_days-old lbl_srt rd_ascdsc 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-41 RECT-7 RECT-8 as-of-date tb_cust-list btnCustList begin_cust 
         end_cust begin_cust-po end_cust-po begin_slm end_slm tb_inc-zer 
         tb_inc-cust fi_days-old rd_ascdsc sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-date C-Win 
PROCEDURE first-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT-OUTPUT PARAM io-date AS DATE NO-UNDO.

  DEF BUFFER b-bin   FOR fg-bin.
  DEF BUFFER b-rcpth FOR fg-rcpth.
  DEF BUFFER b-rdtlh FOR fg-rdtlh.


  FIND b-bin WHERE ROWID(b-bin) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-bin THEN
  IF TRIM(b-bin.tag) NE "" THEN
  FOR EACH b-rdtlh NO-LOCK
      WHERE b-rdtlh.company   EQ b-bin.company
        AND b-rdtlh.tag       EQ b-bin.tag
        AND b-rdtlh.rita-code EQ "R"
      USE-INDEX tag,
      EACH b-rcpth NO-LOCK
      WHERE b-rcpth.r-no      EQ b-rdtlh.r-no
        AND b-rcpth.i-no      EQ b-bin.i-no
        AND b-rcpth.rita-code EQ b-rdtlh.rita-code
      USE-INDEX r-no
      BY b-rcpth.trans-date
      BY b-rdtlh.trans-time
      BY b-rcpth.r-no:
    LEAVE.
  END.

  ELSE
  IF TRIM(b-bin.job-no) NE "" THEN
  FOR EACH b-rcpth NO-LOCK
      WHERE b-rcpth.company   EQ b-bin.company
        AND b-rcpth.job-no    EQ b-bin.job-no
        AND b-rcpth.job-no2   EQ b-bin.job-no2
        AND b-rcpth.i-no      EQ b-bin.i-no
        AND b-rcpth.rita-code EQ "R"
      USE-INDEX job,
      EACH b-rdtlh NO-LOCK
      WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
        AND b-rdtlh.rita-code EQ b-rcpth.rita-code
      BY b-rcpth.trans-date
      BY b-rdtlh.trans-time
      BY b-rcpth.r-no:
    LEAVE.
  END.

  IF AVAIL b-rcpth THEN io-date = b-rcpth.trans-date.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*{sys/form/r-topw.f  "Hot Keys I-R-6"}*/

DEF VAR vdat        AS   DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR fcus        LIKE itemfg.cust-no NO-UNDO.
DEF VAR tcus        LIKE fcus INIT "zzzzzzzz" NO-UNDO.
DEF VAR fpo         LIKE oe-ordl.po-no NO-UNDO.
DEF VAR tpo         LIKE fpo INIT "zzzzzzzzzzzzzzz" NO-UNDO.
DEF VAR fsls        LIKE cust.sman NO-UNDO.
DEF VAR tsls        LIKE fsls INIT "zzz" NO-UNDO.
DEF VAR vzer        AS   LOG FORMAT "Y/N" INIT NO NO-UNDO.
DEF VAR vwhs        LIKE vzer NO-UNDO.
DEF VAR vpcp        LIKE vzer NO-UNDO.
DEF VAR vdue        LIKE vzer FORMAT "DueDate/OrderDate" NO-UNDO.

DEF VAR v-frst      AS   LOG NO-UNDO.
DEF VAR v-print     AS   LOG NO-UNDO.
DEF VAR v-bin       AS   LOG NO-UNDO.
DEF VAR v-po-no     LIKE oe-ordl.po-no NO-UNDO.
DEF VAR v-price     LIKE itemfg.sell-price NO-UNDO.
DEF VAR v-uom       LIKE itemfg.sell-uom NO-UNDO.
DEF VAR v-cas-cnt   LIKE itemfg.case-count NO-UNDO.
DEF VAR v-binqty    AS   DEC NO-UNDO.
DEF VAR v-ext       AS   DEC EXTENT 3 NO-UNDO.
DEF VAR v-qty       AS   DEC EXTENT 3 NO-UNDO.
DEF VAR v-date      AS   DATE NO-UNDO.
DEF VAR v-rct-date      AS   DATE NO-UNDO.
DEF VAR v-pallets   AS   DEC NO-UNDO.
DEF VAR v-frst-date AS   DATE NO-UNDO.
DEF VAR v-ship-date AS   DATE NO-UNDO.
DEF VAR lv-stat     AS   CHAR NO-UNDO.
DEF VAR v-date2        AS   DATE.
  DEF VAR v-date3        AS   DATE.
  DEF VAR vjob-no AS CHAR NO-UNDO.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
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
DEF VAR v-sales-rep AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.

SESSION:SET-WAIT-STATE("general").

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 vdat   = as-of-date
 fcus   = begin_cust
 tcus   = end_cust
 fpo    = begin_cust-po
 tpo    = END_cust-po
 fsls   = begin_slm
 tsls   = end_slm
 vzer   = tb_inc-zer
 vwhs   = tb_inc-cust
 lSelected    = tb_cust-list
        /*vpcp   = tb_cust-pt*/
        /*vdue   = rd_sort EQ "due Date"*/ .

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

        IF LOOKUP(ttRptSelected.TextList, "PALLETS,TOTAL VALUE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.
DISPLAY "" WITH FRAME r-top.
FOR EACH tt-report:
    DELETE tt-report.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
 END.

DEF VAR cSlsREp AS CHAR.

FOR EACH itemfg
        WHERE itemfg.company    EQ cocode
        AND itemfg.cust-no GE fcus
        AND itemfg.cust-no    LE tcus
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND itemfg.cust-po-no GE fpo
        AND itemfg.cust-po-no LE tpo
        NO-LOCK:
    RUN fg/fgSlsRep.p (INPUT itemfg.company,
                       INPUT itemfg.cust-no,
                       INPUT itemfg.part-no,
                       INPUT itemfg.i-no,
                       OUTPUT cSlsRep).
    IF  cSlsRep    GE fsls
          AND cSlsRep    LE tsls THEN DO:
       CREATE tt-itemfg.
       ASSIGN tt-itemfg.i-no = itemfg.i-no
              tt-itemfg.slsrep = cSlsRep
              tt-itemfg.cust-no = itemfg.cust-no
              tt-itemfg.itemfg-row = ROWID(itemfg).
    END.
END.

FOR EACH tt-itemfg, 
  FIRST itemfg
        WHERE ROWID(itemfg) EQ tt-itemfg.itemfg-row NO-LOCK,
        FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ itemfg.cust-no
        NO-LOCK
        BREAK BY itemfg.cust-no
              BY itemfg.i-no:

      STATUS DEFAULT "Processing Customer#/FG Item#: " +
                     TRIM(itemfg.cust-no) + "/" + TRIM(itemfg.i-no).

      v-bin = NO.

      FOR EACH fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ itemfg.i-no
            AND (vwhs OR (fg-bin.loc NE "CUST" AND fg-bin.cust-no EQ ""))
          NO-LOCK
          USE-INDEX i-no:

       IF fg-bin.tag NE "" THEN
       FOR EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.company    EQ fg-bin.company
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.cust-no    EQ fg-bin.cust-no
          USE-INDEX tag,
          FIRST fg-rcpth NO-LOCK
          WHERE fg-rcpth.r-no       EQ fg-rdtlh.r-no
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.rita-code  EQ fg-rdtlh.rita-code
            AND fg-rcpth.trans-date LE vdat 
          USE-INDEX r-no
          BREAK BY fg-rcpth.i-no
                BY fg-rcpth.job-no
                BY fg-rcpth.job-no2
                BY fg-rdtlh.loc
                BY fg-rdtlh.loc-bin
                BY fg-rdtlh.tag
                BY fg-rdtlh.cust-no
                BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:

        v-bin = YES.

        IF FIRST-OF(fg-rdtlh.tag) THEN v-frst-date = fg-rcpth.trans-date.

        IF INDEX("RATE",fg-rcpth.rita-code) NE 0 THEN v-binqty = v-binqty + fg-rdtlh.qty.
        ELSE IF fg-rcpth.rita-code EQ "C" THEN v-binqty = fg-rdtlh.qty.
        ELSE IF fg-rcpth.rita-code EQ "S" THEN v-binqty = v-binqty - fg-rdtlh.qty.

        /*if last-of(fg-bin.tag) then*/
        IF LAST-OF(fg-rdtlh.cust-no) THEN DO:
          ASSIGN
           v-qty[1]    = v-qty[1] + v-binqty
           v-binqty    = 0
           v-ship-date = ?.

        /*if last-of(fg-bin.job-no2) then do:*/
           FIND LAST oe-ordl
              WHERE oe-ordl.company   EQ cocode
                AND oe-ordl.i-no      EQ fg-bin.i-no
                AND (oe-ordl.ord-no   EQ fg-bin.ord-no OR
                     (oe-ordl.job-no  EQ fg-bin.job-no AND
                      oe-ordl.job-no2 EQ fg-bin.job-no2))
              USE-INDEX item NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl THEN DO:
            FIND FIRST oe-ord
                WHERE oe-ord.company EQ cocode
                  AND oe-ord.ord-no  EQ oe-ordl.ord-no
                NO-LOCK.
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                BY oe-rel.rel-date DESC:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
              IF lv-stat EQ "C" THEN DO:
                v-ship-date = oe-rel.rel-date.
                LEAVE.
              END.
            END.
          END.

          v-date = IF AVAIL oe-ordl THEN 
                     IF vdue THEN oe-ordl.req-date
                             ELSE oe-ord.ord-date
                   ELSE ?.

          IF fi_days-old > 0 THEN RUN calc-qoh (OUTPUT v-qty[1]).

          RUN first-date (ROWID(fg-bin), INPUT-OUTPUT v-frst-date).

          CREATE tt-report.
          ASSIGN
           tt-report.key-01 = IF v-date EQ ? THEN ""
                              ELSE STRING(YEAR(v-date),"9999") +
                                   string(MONTH(v-date),"99")  +
                                   string(DAY(v-date),"99")
           tt-report.qty    = v-qty[1]
           v-qty[1]         = 0
           tt-report.rec-id = RECID(fg-bin)
           tt-report.tt-date = v-date
           tt-report.cust-no = itemfg.cust-no
           tt-report.rct-date = v-frst-date
           tt-report.ship-date = v-ship-date
           tt-report.key-06    = IF AVAIL oe-ordl THEN STRING(oe-ordl.req-date) ELSE ""
           tt-report.key-07    = IF AVAIL oe-ord THEN  STRING(oe-ord.ord-date) ELSE "".
        END.
       END.

       ELSE
       FOR EACH fg-rcpth
          WHERE fg-rcpth.company      EQ fg-bin.company
            AND fg-rcpth.i-no         EQ fg-bin.i-no
            AND fg-rcpth.job-no       EQ fg-bin.job-no
            AND fg-rcpth.job-no2      EQ fg-bin.job-no2
            AND fg-rcpth.trans-date   LE vdat
          NO-LOCK USE-INDEX tran,
          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
            AND fg-rdtlh.loc          EQ fg-bin.loc
            AND fg-rdtlh.loc-bin      EQ fg-bin.loc-bin
            AND fg-rdtlh.tag          EQ fg-bin.tag
            AND fg-rdtlh.cust-no      EQ fg-bin.cust-no
            AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
          NO-LOCK
          BREAK BY fg-rcpth.i-no
                BY fg-rcpth.job-no
                BY fg-rcpth.job-no2
                BY fg-rdtlh.loc
                BY fg-rdtlh.loc-bin
                BY fg-rdtlh.tag
                BY fg-rdtlh.cust-no
                BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:

        v-bin = YES.

        IF FIRST-OF(fg-rdtlh.tag) THEN v-frst-date = fg-rcpth.trans-date.

        IF INDEX("RATE",fg-rcpth.rita-code) NE 0 THEN v-binqty = v-binqty + fg-rdtlh.qty.
        ELSE IF fg-rcpth.rita-code EQ "C" THEN v-binqty = fg-rdtlh.qty.
        ELSE IF fg-rcpth.rita-code EQ "S" THEN v-binqty = v-binqty - fg-rdtlh.qty.

        /*if last-of(fg-bin.tag) then*/
        IF LAST-OF(fg-rdtlh.cust-no) THEN DO:
          ASSIGN
           v-qty[1]    = v-qty[1] + v-binqty
           v-binqty    = 0
           v-ship-date = ?.

        /*if last-of(fg-bin.job-no2) then do:*/
           FIND LAST oe-ordl
              WHERE oe-ordl.company   EQ cocode
                AND oe-ordl.i-no      EQ fg-bin.i-no
                AND (oe-ordl.ord-no   EQ fg-bin.ord-no OR
                     (oe-ordl.job-no  EQ fg-bin.job-no AND
                      oe-ordl.job-no2 EQ fg-bin.job-no2))
              USE-INDEX item NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl THEN DO:
            FIND FIRST oe-ord
                WHERE oe-ord.company EQ cocode
                  AND oe-ord.ord-no  EQ oe-ordl.ord-no
                NO-LOCK.
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                BY oe-rel.rel-date DESC:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
              IF lv-stat EQ "C" THEN DO:
                v-ship-date = oe-rel.rel-date.
                LEAVE.
              END.
            END.
          END.

          v-date = IF AVAIL oe-ordl THEN 
                     IF vdue THEN oe-ordl.req-date
                             ELSE oe-ord.ord-date
                   ELSE ?.

          IF fi_days-old > 0 THEN RUN calc-qoh (OUTPUT v-qty[1]).

          RUN first-date (ROWID(fg-bin), INPUT-OUTPUT v-frst-date).

          CREATE tt-report.
          ASSIGN
           tt-report.key-01 = IF v-date EQ ? THEN ""
                              ELSE STRING(YEAR(v-date),"9999") +
                                   string(MONTH(v-date),"99")  +
                                   string(DAY(v-date),"99")
           tt-report.qty    = v-qty[1]
           v-qty[1]         = 0
           tt-report.rec-id = RECID(fg-bin)
           tt-report.tt-date = v-date
           tt-report.cust-no = itemfg.cust-no
           tt-report.rct-date = v-frst-date
           tt-report.ship-date = v-ship-date
           tt-report.key-06    = IF AVAIL oe-ordl THEN STRING(oe-ordl.req-date) ELSE ""
           tt-report.key-07    = IF AVAIL oe-ord THEN  STRING(oe-ord.ord-date) ELSE "" .                             .                 
        END.
       END.
      END.
END.

IF rd_ascdsc = "A" THEN {fgrep/r-custimN.i}
                   ELSE {fgrep/r-custimN.i DESC}

STATUS DEFAULT.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-by-item C-Win 
PROCEDURE run-report-by-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  old way by customer by item

{sys/form/r-topw.f  "Hot Keys I-R-6"}

def var vdat        as   date init today format "99/99/9999".
def var fcus        like itemfg.cust-no.
def var tcus        like fcus init "zzzzzzzz".
def var fpo         like oe-ordl.po-no.
def var tpo         like fpo init "zzzzzzzzzzzzzzz".
def var fsls        like cust.sman.
def var tsls        like fsls init "zzz".
def var vzer        as   log format "Y/N" init no.
def var vwhs        like vzer.
def var vpcp        like vzer.
def var vdue        like vzer format "DueDate/OrderDate" no-undo.

def var v-frst      as   log no-undo.
def var v-print     as   log no-undo.
def var v-bin       as   log no-undo.
def var v-po-no     like oe-ordl.po-no no-undo.
def var v-price     like itemfg.sell-price no-undo.
def var v-uom       like itemfg.sell-uom no-undo.
def var v-cas-cnt   like itemfg.case-count no-undo.
def var v-binqty    as   dec.
def var v-ext       as   dec extent 3.
def var v-qty       as   dec extent 3.
def var v-date      as   date no-undo.
def var v-rct-date      as   date no-undo.
def var v-pallets   as   dec no-undo.

DEF BUFFER b-fg-rcpth FOR fg-rcpth.

form header skip(1) with frame r-top.

form cust.cust-no column-label "CUSTOMER!   ID"
     v-po-no column-label " P.O.!NUMBER"
     itemfg.i-no column-label " ITEM!NUMBER"
     itemfg.part-no label "CUST PART #" format "x(15)"
     itemfg.i-name label "DESCRIPTION" format "x(20)"
     fg-bin.job-no column-label "  JOB!NUMBER"
     space(0) "-" space(0) fg-bin.job-no2 column-label "" format ">9"
     v-qty[1] column-label "QUANTITY!ON-HAND" format "->>>,>>>,>>9"
     v-pallets column-label "PALLETS" format "->>>>>9"
     v-price column-label "SELL!PRICE"  format ">>>,>>9.99"
     v-ext[1] column-label "TOTAL!VALUE"  format "->>>,>>>,>>9.99"
     v-date column-label "DATE" format "99/99/99"
     v-rct-date column-label "RECEIPT!DATE" format "99/99/99"
     with no-box frame itemx1 down STREAM-IO width 160.

form cust.cust-no column-label "CUSTOMER!   ID"
     v-po-no column-label " P.O.!NUMBER"
     itemfg.i-no column-label " ITEM!NUMBER"
     itemfg.i-name label "DESCRIPTION"   format "x(20)"
     fg-bin.job-no column-label "  JOB!NUMBER"
     space(0) "-" space(0) fg-bin.job-no2 column-label "" format ">9"
     v-qty[1] column-label "QUANTITY!ON-HAND" format "->>>,>>>,>>9"
     v-pallets column-label "PALLETS" format "->>>>>9"
     v-price column-label "SELL!PRICE"  format ">>>,>>9.99"
     v-ext[1] column-label "TOTAL!VALUE"  format "->>>,>>>,>>9.99"
     v-date column-label "DATE" format "99/99/99"
     v-rct-date column-label "RECEIPT!DATE" format "99/99/99"
    with no-box frame itemx2 down STREAM-IO width 142.

SESSION:SET-WAIT-STATE("general").

assign
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 vdat   = as-of-date
 fcus   = begin_cust
 tcus   = end_cust
 fpo    = begin_cust-po
 tpo    = END_cust-po
 fsls   = begin_slm
 tsls   = end_slm
 vzer   = tb_inc-zer
 vwhs   = tb_inc-cust
 vpcp   = tb_cust-pt
 vdue   = rd_sort EQ "due Date".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

VIEW frame r-top.

if td-show-parm then run show-param.



  for each itemfg
        where itemfg.company    eq cocode
          and itemfg.cust-no    ge fcus
          and itemfg.cust-no    le tcus
          and itemfg.cust-po-no ge fpo
          and itemfg.cust-po-no le tpo
        use-index customer no-lock,

        first cust
        where cust.company eq cocode
          and cust.cust-no eq itemfg.cust-no
          and cust.sman    ge fsls
          and cust.sman    le tsls
        no-lock

        break by itemfg.cust-no:

      if first-of(itemfg.cust-no) then
        assign
         v-frst  = yes
         v-print = no.

      v-bin = no.

      for each tt-report:
        delete tt-report.
      end.

      for each fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq itemfg.i-no
            and (vwhs or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq ""))
          use-index i-no,

          each fg-rcpth
          where fg-rcpth.company      eq cocode
            and fg-rcpth.i-no         eq itemfg.i-no
            and fg-rcpth.job-no       eq fg-bin.job-no
            and fg-rcpth.job-no2      eq fg-bin.job-no2
            and fg-rcpth.trans-date   le vdat
          no-lock use-index tran,

          each fg-rdtlh
          where fg-rdtlh.r-no         eq fg-rcpth.r-no
            and fg-rdtlh.loc          eq fg-bin.loc
            and fg-rdtlh.loc-bin      eq fg-bin.loc-bin
            and fg-rdtlh.tag          eq fg-bin.tag
            and fg-rdtlh.rita-code    eq fg-rcpth.rita-code
          no-lock

          break by fg-bin.i-no
                by fg-bin.job-no
                by fg-bin.job-no2
                by fg-bin.loc
                by fg-bin.loc-bin
                by fg-bin.tag
                by fg-rcpth.trans-date
                by fg-rcpth.r-no:

        v-bin = yes.        

        if index("RATE",fg-rcpth.rita-code) ne 0 then
          v-binqty = v-binqty + fg-rdtlh.qty.

        else
        if fg-rcpth.rita-code eq "C" then
          v-binqty = fg-rdtlh.qty.

        else
        if fg-rcpth.rita-code eq "S" then
          v-binqty = v-binqty - fg-rdtlh.qty.

        if last-of(fg-bin.tag) then
          assign
           v-qty[1] = v-qty[1] + v-binqty
           v-binqty = 0.

        if last-of(fg-bin.job-no2) then do:
           find last oe-ordl
              where oe-ordl.company   eq cocode
                and oe-ordl.i-no      eq fg-bin.i-no
                and (oe-ordl.ord-no   eq fg-bin.ord-no or
                     (oe-ordl.job-no  eq fg-bin.job-no and
                      oe-ordl.job-no2 eq fg-bin.job-no2))
              use-index item no-lock no-error.
          if avail oe-ordl then
          find first oe-ord
              where oe-ord.company eq cocode
                and oe-ord.ord-no  eq oe-ordl.ord-no
              no-lock.

          v-date = if avail oe-ordl then 
                     if vdue then oe-ordl.req-date
                             else oe-ord.ord-date
                   else ?.

          IF fi_days-old > 0 THEN RUN calc-qoh (OUTPUT v-qty[1]).

          create tt-report.
          assign
           tt-report.key-01 = if v-date eq ? then ""
                              else string(year(v-date),"9999") +
                                   string(month(v-date),"99")  +
                                   string(day(v-date),"99")
           tt-report.qty    = v-qty[1]
           v-qty[1]         = 0
           tt-report.rec-id = recid(fg-bin)
           tt-report.tt-date = v-date.      
           FOR each b-fg-rcpth
                   where b-fg-rcpth.company      eq cocode
                     and b-fg-rcpth.i-no         eq itemfg.i-no
                     AND b-fg-rcpth.rita-code = "R"
                     and b-fg-rcpth.job-no       eq fg-bin.job-no
                     and b-fg-rcpth.job-no2      eq fg-bin.job-no2 NO-LOCK
                     BY b-fg-rcpth.trans-date:
               tt-report.rct-date = b-fg-rcpth.trans-date.
               LEAVE.
           END.
        end.
      end.

      IF rd_ascdsc = "A" THEN /*ascending*/
      for each tt-report,
          first fg-bin where recid(fg-bin) eq tt-report.rec-id no-lock
          by tt-report.tt-date 
          by fg-bin.job-no     
          by fg-bin.job-no2:

        v-qty[1] = tt-report.qty.

        find last oe-ordl
            where oe-ordl.company   eq cocode
              and oe-ordl.i-no      eq fg-bin.i-no
              and (oe-ordl.ord-no   eq fg-bin.ord-no or
                   (oe-ordl.job-no  eq fg-bin.job-no and
                    oe-ordl.job-no2 eq fg-bin.job-no2))
            use-index item no-lock no-error.

        if avail oe-ordl THEN
            assign
             v-po-no   = oe-ordl.po-no
             v-price   = oe-ordl.price
             v-uom     = oe-ordl.pr-uom
             v-cas-cnt = oe-ordl.cas-cnt.
        else
            assign
             v-po-no   = itemfg.cust-po-no
             v-price   = itemfg.sell-price
             v-uom     = itemfg.sell-uom
             v-cas-cnt = itemfg.case-count.

        if v-uom eq "L" and avail oe-ordl then
            v-ext[1] = v-price / oe-ordl.qty * v-qty[1].

        else
          if v-uom eq "CS"  and
             v-cas-cnt ne 0 then
            v-ext[1] = (v-qty[1] * v-price) / v-cas-cnt.

        else do:
            v-ext[1] = v-qty[1] * v-price.
            find first uom
                where uom.uom  eq v-uom
                  and uom.mult ne 0
                no-lock no-error.
            if avail uom then v-ext[1] = v-ext[1] / uom.mult.
        end.

        if v-qty[1] ne 0 or vzer then do:
          v-pallets = v-qty[1] /*fg-bin.qty*/ /
               ((if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)).

          {sys/inc/roundup.i v-pallets}
          v-date = tt-report.tt-date.
          v-rct-date = tt-report.rct-date.
          if vpcp then do:
            display cust.cust-no        when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.part-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                with frame itemx1.
            down with frame itemx1.
          end.
          else do:
            display cust.cust-no        when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                with frame itemx2.
            down with frame itemx2.
          end.
        end.

        assign
         v-frst   = no
         v-qty[2] = v-qty[2] + v-qty[1]
         v-ext[2] = v-ext[2] + v-ext[1]
         v-qty[1] = 0
         v-ext[1] = 0
         v-print  = yes.

        delete tt-report.
      end.
      ELSE for each tt-report,
          first fg-bin where recid(fg-bin) eq tt-report.rec-id no-lock

          BREAK by tt-report.tt-date DESC /*key-01*/
          by fg-bin.job-no
          by fg-bin.job-no2:

        v-qty[1] = tt-report.qty.

        find last oe-ordl
            where oe-ordl.company   eq cocode
              and oe-ordl.i-no      eq fg-bin.i-no
              and (oe-ordl.ord-no   eq fg-bin.ord-no or
                   (oe-ordl.job-no  eq fg-bin.job-no and
                    oe-ordl.job-no2 eq fg-bin.job-no2))
            use-index item no-lock no-error.

        if avail oe-ordl then

            assign
             v-po-no   = oe-ordl.po-no
             v-price   = oe-ordl.price
             v-uom     = oe-ordl.pr-uom
             v-cas-cnt = oe-ordl.cas-cnt.
          else
            assign
             v-po-no   = itemfg.cust-po-no
             v-price   = itemfg.sell-price
             v-uom     = itemfg.sell-uom
             v-cas-cnt = itemfg.case-count.

          if v-uom eq "L" and avail oe-ordl then
            v-ext[1] = v-price / oe-ordl.qty * v-qty[1].

          else
          if v-uom eq "CS"  and
             v-cas-cnt ne 0 then
            v-ext[1] = (v-qty[1] * v-price) / v-cas-cnt.

          else do:
            v-ext[1] = v-qty[1] * v-price.
            find first uom
                where uom.uom  eq v-uom
                  and uom.mult ne 0
                no-lock no-error.
            if avail uom then v-ext[1] = v-ext[1] / uom.mult.
          end.

                if v-qty[1] ne 0 or vzer then do:
          v-pallets = fg-bin.qty /
               ((if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)).

          {sys/inc/roundup.i v-pallets}
          v-date = tt-report.tt-date.
          v-rct-date = tt-report.rct-date.
          if vpcp then do:
            display cust.cust-no        when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.part-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                with frame itemx1.
            down with frame itemx1.
          end.
          else do:
            display cust.cust-no        when v-frst
                    v-po-no
                    itemfg.i-no
                    itemfg.i-name
                    fg-bin.job-no
                    fg-bin.job-no2
                    v-qty[1]
                    v-pallets
                    v-price
                    v-ext[1]
                    v-date
                    v-rct-date
                with frame itemx2.
            down with frame itemx2.
          end.
        end.

        assign
         v-frst   = no
         v-qty[2] = v-qty[2] + v-qty[1]
         v-ext[2] = v-ext[2] + v-ext[1]
         v-qty[1] = 0
         v-ext[1] = 0
         v-print  = yes.

        delete tt-report.
      end. /* descending */

      if vzer and not v-bin then do:
        if vpcp then do:
          display cust.cust-no          when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.part-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx1.
          down with frame itemx1.
        end.

        else do:
          display cust.cust-no          when v-frst
                  ""                    @ v-po-no
                  itemfg.i-no
                  itemfg.i-name
                  0                     @ v-qty[1]
                  0                     @ v-pallets
                  itemfg.sell-price     @ v-price
                  0                     @ v-ext[1]
              with frame itemx2.
          down with frame itemx2.
        end.

        assign
         v-frst  = no
         v-print = yes.
      end.

      if last-of(itemfg.cust-no) then do:
        if v-print                 and
           (v-qty[2] ne 0 or vzer) then

          put "--------------" to 123 skip
              "Customer Total" at 85 v-ext[2] to 123 format "->>,>>>,>>9.99"
              skip(1).

        assign
         v-qty[3] = v-qty[3] + v-qty[2]
         v-ext[3] = v-ext[3] + v-ext[2]
         v-qty[2] = 0
         v-ext[2] = 0.
      end.

      if last(itemfg.cust-no)    and
         (v-qty[3] ne 0 or vzer) then

        put "--------------" to 123 skip
            "   Grand Total" at 85 v-ext[3] to 123 format "->>,>>>,>>9.99"
            skip(1).
    end.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

   SESSION:SET-WAIT-STATE("").
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
*/
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
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
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
       entry(i,parm-lbl-list) NE "" THEN DO:

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

