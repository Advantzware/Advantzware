&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-trnord.w

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

/*{sys/inc/custlistform.i ""IR11"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD qty LIKE fg-bin.qty.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Cust #,Item #,Cust PO #,Job #,Qty Ordered,Trans Date,C,Qty," +
                           "Qty on Hand,Bal Remaining,Selling Price,Total Value,Ship Date,Days of Last Ship,Rep"
       cFieldListToSelect = "cust,item,cust-po,job,qty-ord,tr-date,c,qty," +
                            "qty-hand,bal-rem,sell-pr,tot-val,ship-date,last-ship,rep"
       cFieldLength = "8,15,15,10,11,10,1,11," + "11,13,13,11,8,17,3"
       cFieldType = "c,c,c,c,i,c,c,i," + "i,i,i,i,c,i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Cust #,Item #,Cust PO #,Job #,Qty Ordered,Trans Date,C,Qty," +
                           "Qty on Hand,Bal Remaining,Selling Price,Total Value,Ship Date,Days of Last Ship" .

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
begin_cust-no end_cust-no begin_i-no end_i-no begin_cust-po end_cust-po ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_ord-no end_ord-no ~
begin_cat end_cat begin_slm end_slm tb_clo-ord tb_new-ord sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down lv-ornt lines-per-page ~
rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_i-no end_i-no begin_cust-po end_cust-po begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_ord-no end_ord-no begin_cat end_cat begin_slm ~
end_slm tb_clo-ord tb_new-ord sl_avail sl_selected lv-ornt lines-per-page ~
rd-dest lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job# on Order" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job# On Order" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-trnord.csv" 
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
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 7.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 11.19.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_clo-ord AS LOGICAL INITIAL yes 
     LABEL "Include Closed Orders?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_new-ord AS LOGICAL INITIAL yes 
     LABEL "Include New Orders?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 2.48 COL 30.6 WIDGET-ID 6
     btnCustList AT ROW 2.52 COL 62.6 WIDGET-ID 8
     begin_cust-no AT ROW 3.76 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.76 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_i-no AT ROW 4.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.71 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cust-po AT ROW 5.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer PO Number"
     end_cust-po AT ROW 5.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_job-no AT ROW 6.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.62 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.62 COL 82 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_ord-no AT ROW 7.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 7.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cat AT ROW 8.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 2
     end_cat AT ROW 8.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number" WIDGET-ID 4
     begin_slm AT ROW 9.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 6
     end_slm AT ROW 9.71 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 8
     tb_clo-ord AT ROW 10.86 COL 29.2
     tb_new-ord AT ROW 10.91 COL 58.8
     sl_avail AT ROW 13.05 COL 5 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.05 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 13.05 COL 60.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.05 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.05 COL 41 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.1 COL 41 WIDGET-ID 40
     btn_down AT ROW 17.1 COL 41 WIDGET-ID 42
     lv-ornt AT ROW 18.67 COL 31 NO-LABEL
     lines-per-page AT ROW 18.67 COL 84 COLON-ALIGNED
     rd-dest AT ROW 19.24 COL 4.6 NO-LABEL
     lv-font-no AT ROW 19.91 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21.43 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.76 COL 30.2
     tb_excel AT ROW 23.81 COL 72 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.81 COL 94 RIGHT-ALIGNED
     fi_file AT ROW 24.76 COL 50 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.43 COL 27.8
     btn-cancel AT ROW 26.43 COL 56.8
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.33 COL 5.8 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.52 COL 2.6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.33 COL 60.4 WIDGET-ID 44
     RECT-6 AT ROW 18.57 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 27.19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 27.19.


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
         TITLE              = "Finished Goods Transactions By Order"
         HEIGHT             = 27.19
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_clo-ord:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_new-ord:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Finished Goods Transactions By Order */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Transactions By Order */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po C-Win
ON LEAVE OF begin_cust-po IN FRAME FRAME-A /* Beginning Customer PO# */
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


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# on Order */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  SESSION:SET-WAIT-STATE("general").
   RUN GetSelectionList.
   FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.

  /* gdm - 10260906*/
  /*IF tb_Shipment 
    THEN RUN run-report2.
    ELSE*/ run run-report. 

   STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_cust-no 
                            &END_cust=END_cust-no 
                            &fax-subject=c-win:TITLE 
                            &fax-body=c-win:TITLE 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust-no 
                             &END_cust=END_cust-no 
                             &mail-subject=c-win:TITLE 
                             &mail-body=c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust-no 
                                  &END_cust=END_cust-no 
                                  &mail-subject=c-win:TITLE 
                                  &mail-body=c-win:TITLE 
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


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po C-Win
ON LEAVE OF end_cust-po IN FRAME FRAME-A /* Ending Customer PO# */
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# On Order */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME tb_clo-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_clo-ord C-Win
ON VALUE-CHANGED OF tb_clo-ord IN FRAME FRAME-A /* Include Closed Orders? */
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


&Scoped-define SELF-NAME tb_new-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_new-ord C-Win
ON VALUE-CHANGED OF tb_new-ord IN FRAME FRAME-A /* Include New Orders? */
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
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IR11",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust-no.
  END.

  /* gdm - 10260906 */
  /*IF tb_Shipment:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "yes"
    THEN ASSIGN lv-ornt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "L".
    ELSE ASSIGN lv-ornt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "P".*/

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR11',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR11""}

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
                            INPUT 'IR11',
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
                                  INPUT 'IR11').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_i-no end_i-no 
          begin_cust-po end_cust-po begin_job-no begin_job-no2 end_job-no 
          end_job-no2 begin_ord-no end_ord-no begin_cat end_cat begin_slm 
          end_slm tb_clo-ord tb_new-ord sl_avail sl_selected lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_i-no end_i-no begin_cust-po end_cust-po begin_job-no 
         begin_job-no2 end_job-no end_job-no2 begin_ord-no end_ord-no begin_cat 
         end_cat begin_slm end_slm tb_clo-ord tb_new-ord sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down lv-ornt lines-per-page 
         rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok 
         btn-cancel 
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- fg/rep/fg-trans.p 07/96 JLF */
/* finished goods transactions by order                                       */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var fcus        like itemfg.cust-no NO-UNDO.
def var tcus        like fcus               init "zzzzzzzz" NO-UNDO.
def var fitm        like itemfg.i-no NO-UNDO.
def var titm        like fitm               init "zzzzzzzzzzzzzzz" NO-UNDO.
def var fp-o        like oe-ordl.po-no NO-UNDO.
def var tp-o        like fp-o               init "zzzzzzzzzzzzzzz" NO-UNDO.
def var ford        like oe-ord.ord-no      format ">>>>>>" NO-UNDO.
def var tord        like ford               init 999999 NO-UNDO.
def var fjob        like oe-ordl.job-no NO-UNDO.
def var tjob        like fjob               init "zzzzzz" NO-UNDO.
def var fjob2       like oe-ordl.job-no2    format "99" NO-UNDO.
def var tjob2       like fjob2              init 99 NO-UNDO.
def var vdet        as   log   init yes    format "Detail/Summary" NO-UNDO.
def var vinc        as   log   init yes    format "Yes/No" NO-UNDO.
def var vinc1       as   log   init yes    format "Yes/No" NO-UNDO.
def var v-qty       as   INT NO-UNDO.
def var v-qop       as   INT NO-UNDO.
def var v-qoh       as   INT NO-UNDO.
def var v-bal       as   INT NO-UNDO.
def var v-val       as   DEC NO-UNDO.
def var v-job       as   char format "x(9)" NO-UNDO.
def var v-cus       like itemfg.cust-no NO-UNDO.
def var v-itm       like itemfg.i-no NO-UNDO.
def var v-price     like oe-ordl.price NO-UNDO.
DEF VAR v-fcat  LIKE itemfg.procat                 NO-UNDO.
DEF VAR v-tcat  LIKE itemfg.procat                 NO-UNDO.
DEF VAR v-fsman LIKE cust.sman                     NO-UNDO.
DEF VAR v-tsman LIKE cust.sman                     NO-UNDO.
def var v-printed   as   LOG NO-UNDO.
DEF VAR v-shpdate LIKE fg-rcpth.trans-date         NO-UNDO.
DEF VAR v-shp       AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR li-tqty AS INT NO-UNDO.
DEF VAR v-shpdays AS INT NO-UNDO.
DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
/*DEF VAR excelheader AS CHAR NO-UNDO.*/

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

DEF VAR v-sales-rep AS CHAR NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

form header
        "        "
        "               "
        "               "
        "         "
        "    Qty  "
        " Trans  "
        " "
        "          "
        "      Qty  "
        "  Balance "
        "  Selling"
        "              "                    skip

        "Cust #  "
        "Item #         "
        "Cust PO #      "
        "    Job #"
        "  Ordered"
        "  Date  "
        "C"
        "       Qty"
        "    On Hand"
        " Remaining"
        "    Price"
        "   Total Value"                    skip

        "--------"
        "---------------"
        "---------------"
        "---------"
        "---------"
        "--------"
        "-"
        "----------"
        "-----------"
        "----------"
        "---------"
        "--------------"                    SKIP
        skip(1)
        with no-box page-top STREAM-IO width 132 frame top.


ASSIGN 
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}

   fcus   = begin_cust-no
   tcus   = end_cust-no
   fitm   = begin_i-no
   titm   = end_i-no
   fp-o   = begin_cust-po
   tp-o   = END_cust-po
   fjob   = fill(" ",6 - length(trim(begin_job-no))) +
            trim(begin_job-no) + STRING(int(begin_job-no2),"99")
   tjob   = fill(" ",6 - length(trim(end_job-no)))   +
            trim(end_job-no)   + STRING(int(end_job-no2),"99")
   ford   = begin_ord-no
   tord   = end_ord-no
   /*vdet   = tb_detailed*/
   vinc   = tb_clo-ord
   vinc1  = tb_new-ord
   v-fcat  = begin_cat
   v-tcat  = end_cat
   v-fsman = begin_slm 
   v-tsman = end_slm .


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

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN  
   RUN show-param.

/*IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Cust #,Item #,Cust PO #,Job #,Qty Ordered,"
               + "Trans Date,C,Qty,Qty On Hand,Balance Remaining,Selling Price,Total Value".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.*/

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

/*DISPLAY WITH FRAME TOP.*/

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    EACH cust NO-LOCK WHERE cust.company        EQ cocode
                        AND cust.cust-no        EQ ttCustList.cust-no /*fcus
                        AND cust.cust-no        LE tcus*/
                        AND cust.sman           GE v-fsman  
                        AND cust.sman           LE v-tsman,
   EACH itemfg NO-LOCK WHERE itemfg.company     EQ cust.company
                         AND itemfg.cust-no     EQ cust.cust-no
                         AND itemfg.i-no        GE fitm
                         AND itemfg.i-no        LE titm 
                         AND itemfg.procat      GE v-fcat
                         AND itemfg.procat      LE v-tcat USE-INDEX customer,
   EACH oe-ordl NO-LOCK WHERE oe-ordl.company   EQ itemfg.company
                          AND oe-ordl.i-no      EQ itemfg.i-no
                          AND oe-ordl.cust-no   EQ cust.cust-no
                          AND (oe-ordl.opened   EQ YES OR vinc)      
                          AND oe-ordl.po-no     GE fp-o
                          AND oe-ordl.po-no     LE tp-o      
                          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99")) GE fjob
                          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99")) LE tjob
                          AND oe-ordl.ord-no    GE ford
                          AND oe-ordl.ord-no    LE tord USE-INDEX item,
   FIRST oe-ord NO-LOCK WHERE oe-ord.company        EQ oe-ordl.company
                          AND oe-ord.ord-no         EQ oe-ordl.ord-no
                     BREAK BY itemfg.cust-no
                           BY itemfg.i-no
                           BY oe-ordl.ord-no

   WITH FRAME main NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 132 DOWN:

    {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"} 

   IF FIRST-OF(itemfg.cust-no) THEN v-cus = itemfg.cust-no.

   IF FIRST-OF(itemfg.i-no) THEN v-itm = itemfg.i-no.

   IF FIRST-OF(oe-ordl.ord-no) THEN EMPTY TEMP-TABLE tt-report.


   v-sales-rep = "" .
   IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:
     FOR EACH cust-part WHERE cust-part.company = itemfg.company   
         AND cust-part.i-no = itemfg.i-no
         AND cust-part.cust-no EQ cust.cust-no
         NO-LOCK:

         IF cust-part.spare-char-1 NE "" THEN do:
             FIND FIRST sman WHERE sman.company = itemfg.company
                 AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
             IF AVAIL sman THEN v-sales-rep = sman.sman.
             LEAVE .
         END.
      END. /* end of cust-part */

      IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
          FIND FIRST sman WHERE sman.company = cust.company
              AND sman.sman = cust.sman NO-LOCK NO-ERROR.
          IF AVAIL sman THEN v-sales-rep = sman.sman.
      END.
   END.
   ELSE DO:
       FIND FIRST sman WHERE sman.company = cust.company
           AND sman.sman = cust.sman NO-LOCK NO-ERROR.
       IF AVAIL sman THEN v-sales-rep = sman.sman.
   END.

   ASSIGN
      v-job     = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                  TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")
      v-price   = oe-ordl.t-price / oe-ordl.qty * 1000
      v-printed = NO.

   IF TRIM(oe-ordl.job-no) NE "" THEN
   FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ oe-ordl.company
                               AND fg-rcpth.job-no  EQ oe-ordl.job-no
                               AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                               AND fg-rcpth.i-no    EQ oe-ordl.i-no USE-INDEX job,
      EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code USE-INDEX rm-rdtl
                         BREAK BY fg-rdtlh.loc
                               BY fg-rdtlh.loc-bin
                               BY fg-rdtlh.tag
                               BY fg-rcpth.trans-date
                               BY fg-rdtlh.trans-time
                               BY fg-rcpth.r-no:

      CREATE tt-report.
      ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                             STRING(MONTH(fg-rcpth.trans-date),"99")  +
                             STRING(DAY(fg-rcpth.trans-date),"99")
         tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
         tt-report.rec-id  = RECID(fg-rdtlh).
   END.

   FOR EACH oe-boll NO-LOCK WHERE oe-boll.company  EQ cocode
                              AND oe-boll.ord-no   EQ oe-ordl.ord-no
                              AND oe-boll.i-no     EQ oe-ordl.i-no
                              AND oe-boll.line     EQ oe-ordl.line
                              AND (oe-ordl.job-no  EQ ""             OR
                                  (oe-boll.job-no  EQ oe-ordl.job-no AND
                                   oe-boll.job-no2 EQ oe-ordl.job-no2))
                              AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.company EQ oe-boll.company
                                                           AND oe-bolh.b-no    EQ oe-boll.b-no
                                                           AND oe-bolh.posted  EQ YES) USE-INDEX ord-no
                              BREAK BY oe-boll.job-no
                                    BY oe-boll.job-no2
                                    BY oe-boll.loc
                                    BY oe-boll.loc-bin
                                    BY oe-boll.tag
                                    BY oe-boll.cust-no:

      IF LAST-OF(oe-boll.cust-no) THEN
         FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ oe-boll.company
                                     AND fg-rcpth.b-no    EQ oe-boll.b-no
                                     AND fg-rcpth.job-no  EQ oe-boll.job-no
                                     AND fg-rcpth.job-no2 EQ oe-boll.job-no2
                                     AND fg-rcpth.i-no    EQ oe-boll.i-no USE-INDEX b-no,
            EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                    AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                    AND fg-rdtlh.tag       EQ oe-boll.tag
                                    AND fg-rdtlh.loc       EQ oe-boll.loc
                                    AND fg-rdtlh.loc-bin   EQ oe-boll.loc-bin
                                    AND fg-rdtlh.cust-no   EQ oe-boll.cust-no USE-INDEX rm-rdtl
                                     BY fg-rcpth.trans-date
                                     BY fg-rdtlh.trans-time
                                     BY fg-rcpth.r-no:

            FIND FIRST tt-report WHERE tt-report.term-id = ""
                                   AND tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                                                           STRING(MONTH(fg-rcpth.trans-date),"99")  +
                                                           STRING(DAY(fg-rcpth.trans-date),"99")
                                   AND tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999") 
                                   AND tt-report.rec-id  = RECID(fg-rdtlh)NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-report THEN DO:
               CREATE tt-report.
               ASSIGN
                  tt-report.term-id = ""
                  tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                                      STRING(MONTH(fg-rcpth.trans-date),"99")  +
                                      STRING(DAY(fg-rcpth.trans-date),"99")
                  tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
                  tt-report.rec-id  = RECID(fg-rdtlh).
            END.

         END.
   END.

   IF LAST-OF(oe-ordl.ord-no) THEN DO:
      EMPTY TEMP-TABLE tt-fg-bin.

      FOR EACH tt-report WHERE tt-report.term-id EQ "",
         FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
         FIRST fg-rcpth WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no NO-LOCK
         BREAK BY tt-report.key-01
               BY tt-report.key-02:

         FIND FIRST tt-fg-bin WHERE tt-fg-bin.company EQ fg-rcpth.company
                                AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                                AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                                AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                                AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                                AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                                AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-fg-bin THEN DO:
            CREATE tt-fg-bin.
            ASSIGN
               tt-fg-bin.company = fg-rcpth.company
               tt-fg-bin.job-no  = fg-rcpth.job-no
               tt-fg-bin.job-no2 = fg-rcpth.job-no2
               tt-fg-bin.loc     = fg-rdtlh.loc
               tt-fg-bin.loc-bin = fg-rdtlh.loc-bin
               tt-fg-bin.tag     = fg-rdtlh.tag
               tt-fg-bin.cust-no = fg-rdtlh.cust-no.
         END.

         ASSIGN
            li-tqty = (fg-rdtlh.qty * IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 1)
            tt-fg-bin.qty = (IF fg-rcpth.rita-code EQ "C" THEN 0
                             ELSE tt-fg-bin.qty) + li-tqty.
         RELEASE tt-fg-bin.
         tt-report.qty = 0.

         FOR EACH tt-fg-bin:
            tt-report.qty = tt-report.qty + tt-fg-bin.qty.
         END.
      END.

      FOR EACH tt-report WHERE tt-report.term-id EQ "",
         FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
         FIRST fg-rcpth WHERE fg-rcpth.r-no   EQ fg-rdtlh.r-no 
                          AND fg-rcpth.rita-code NE "T" NO-LOCK
                     BREAK BY tt-report.key-01
                           BY tt-report.key-02:

         IF FIRST(tt-report.key-01) THEN
            ASSIGN
               v-qty = oe-ordl.qty
               v-bal = oe-ordl.qty               
               v-bal = v-bal + (fg-rdtlh.qty * IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 0).

           /* gdm - 10260906 */
             ASSIGN v-shpdate = ?
                    v-shpdays = 0
                    v-shp     = "".

             FIND LAST bf-fg-rcpth NO-LOCK
               WHERE bf-fg-rcpth.company   EQ cocode
                 AND bf-fg-rcpth.i-no      EQ v-itm
                 AND bf-fg-rcpth.rita-code EQ "S" NO-ERROR.
             IF AVAIL bf-fg-rcpth 
               THEN ASSIGN v-shpdate = bf-fg-rcpth.trans-date
                           v-shpdays = (TODAY - bf-fg-rcpth.trans-date).
             v-shp = IF v-shpdays EQ 0 THEN " " ELSE STRING(v-shpdays,">>,>>>,>>9").


         IF LAST(tt-report.key-01) OR vdet THEN DO:
            ASSIGN
               v-qoh = tt-report.qty
               v-val = round(v-qoh * (oe-ordl.t-price / oe-ordl.qty),2)
               v-job = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                       TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(v-cus,"x(8)") .
                         WHEN "item"   THEN cVarValue = string(v-itm,"x(15)").
                         WHEN "cust-po"   THEN cVarValue = STRING(oe-ordl.po-no,"x(15)").
                         WHEN "job"  THEN cVarValue = IF trim(v-job) NE "-00" THEN STRING(v-job,"x(10)") ELSE "" .
                         WHEN "qty-ord"   THEN cVarValue = IF v-qty NE ? AND v-qty NE 0 THEN STRING(v-qty,"->>,>>>,>>9") ELSE "" .
                         WHEN "tr-date"  THEN cVarValue = IF fg-rcpth.trans-date NE ? THEN STRING(fg-rcpth.trans-date,"99/99/9999") ELSE "" .
                         WHEN "c"   THEN cVarValue = IF fg-rcpth.rita-code NE ? THEN STRING(fg-rcpth.rita-code) ELSE "" .
                         WHEN "qty"  THEN cVarValue = IF fg-rdtlh.qty NE ? THEN STRING(fg-rdtlh.qty,"->>,>>>,>>9") ELSE "" .
                         WHEN "qty-hand"    THEN cVarValue = IF v-qoh NE ? THEN string(v-qoh,"->>,>>>,>>9") ELSE "" .
                         WHEN "bal-rem"   THEN cVarValue = IF v-bal NE ? AND v-bal NE 0 THEN string(v-bal,"->,>>>,>>9.99") ELSE "" .
                         WHEN "sell-pr"   THEN cVarValue = IF v-price NE ? THEN STRING(v-price,"->,>>>,>>9.99") ELSE "" .
                         WHEN "tot-val"  THEN cVarValue = IF v-val NE ? THEN STRING(v-val,"->>>,>>9.99") ELSE "" .
                         WHEN "ship-date"   THEN cVarValue = IF v-shpdate NE ? THEN STRING(v-shpdate,"99/99/99") ELSE "" .
                         WHEN "last-ship"  THEN cVarValue = STRING(v-shp) .
                         WHEN "rep"  THEN cVarValue = STRING(v-sales-rep) .

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

            /*DISPLAY 
               v-cus
               v-itm
               oe-ordl.po-no
               v-job                            FORMAT "x(9)"
               v-qty WHEN v-qty NE 0            FORMAT ">,>>>,>>9"
               fg-rcpth.trans-date WHEN vdet    FORMAT "99/99/99"
               fg-rcpth.rita-code WHEN vdet
               fg-rdtlh.qty WHEN vdet           FORMAT "->,>>>,>>9"
               v-qoh                            FORMAT "->>,>>>,>>9"
               v-bal WHEN v-bal GE 0            FORMAT ">>,>>>,>>9"
                   0 WHEN v-bal LT 0 @ v-bal
               v-price                          FORMAT ">>,>>9.99"
               v-val                            FORMAT "->>,>>>,>>9.99"
               WITH FRAME main.
            DOWN WITH FRAME main.

            IF tb_excel THEN 
               PUT STREAM excel UNFORMATTED
                  '"' v-cus                                         '",'
                  '"' v-itm                                         '",'
                  '"' oe-ordl.po-no                                 '",'
                  '"' FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                      TRIM(fg-rcpth.job-no) + "-" + STRING(fg-rcpth.job-no2,"99") '",'
                  '"' (IF v-qty NE 0 THEN v-qty ELSE 0)             '",'
                  '"' (IF vdet AND fg-rcpth.trans-date <> ? THEN
                        STRING(fg-rcpth.trans-date,"99/99/99")
                      ELSE "")    '",'
                  '"' (IF vdet THEN fg-rcpth.rita-code ELSE "")     '",'
                  '"' (IF vdet THEN STRING(fg-rdtlh.qty) ELSE "")           '",'
                  '"' v-qoh                                         '",'
                  '"' (IF v-bal GE 0 THEN v-bal ELSE 0)             '",'
                  '"' v-price                                       '",'
                  '"' v-val                                         '",'
                  SKIP.*/

            ASSIGN
               v-printed = YES
              /* v-cus     = ""
               v-itm     = ""*/ .
         END.

        IF LAST(tt-report.key-01) /*AND vdet*/ THEN PUT SKIP(1).
      END.

      IF NOT v-printed AND ((vinc1 AND INDEX("CZ",oe-ord.stat) EQ 0) OR
                            (vinc AND INDEX("CZ",oe-ord.stat) GT 0))    THEN DO:

          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(v-cus,"x(8)") .
                         WHEN "item"   THEN cVarValue = string(v-itm,"x(15)").
                         WHEN "cust-po"   THEN cVarValue = STRING(oe-ordl.po-no,"x(15)").
                         WHEN "job"  THEN cVarValue = IF trim(v-job) NE "-00" THEN STRING(v-job,"x(10)") ELSE "" .
                         WHEN "qty-ord"   THEN cVarValue = IF oe-ordl.qty NE ? THEN STRING(oe-ordl.qty,"->>,>>>,>>9") ELSE "" .
                         WHEN "tr-date"  THEN cVarValue = IF oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date,"99/99/9999") ELSE "" .
                         WHEN "c"   THEN cVarValue = /*STRING(fg-rcpth.rita-code)*/ "" .
                         WHEN "qty"  THEN cVarValue = /*STRING(fg-rdtlh.qty,">>>,>>>,>>9")*/ "" .
                         WHEN "qty-hand"    THEN cVarValue = /*string(v-qoh,"->>,>>>,>>9")*/ "" .
                         WHEN "bal-rem"   THEN cVarValue = /*string(v-bal,"->,>>>,>>9.99")*/ "".
                         WHEN "sell-pr"   THEN cVarValue = IF v-price NE ? THEN STRING(v-price,"->,>>>,>>9.99") ELSE "".
                         WHEN "tot-val"  THEN cVarValue = /*STRING(v-val,"->>>,>>9.99")*/ "" .
                         WHEN "ship-date"   THEN cVarValue = /*IF v-shpdate NE ? THEN STRING(v-shpdate,"99/99/99") ELSE*/ "" .
                         WHEN "last-ship"  THEN cVarValue = /*STRING(v-shp)*/ "" .
                         WHEN "rep"  THEN cVarValue = STRING(v-sales-rep) .

                    END CASE.

                    cExcelVarValue = cVarValue.

                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.

            PUT SKIP(1).

            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
         /*DISPLAY 
            v-cus
            v-itm
            oe-ordl.po-no
            v-job
            oe-ordl.qty       @ v-qty
            oe-ord.ord-date   @ fg-rcpth.trans-date
            v-price
         WITH FRAME main.

         PUT SKIP(1).

         IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED
               '"' v-cus                                         '",'
               '"' v-itm                                         '",'
               '"' oe-ordl.po-no                                 '",'
               '"' v-job                                         '",'
               '"' oe-ordl.qty                                   '",'
               '"' oe-ord.ord-date                               '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' v-price                                       '",'
               SKIP.*/
         ASSIGN
           /* v-cus = ""
            v-itm = ""*/
            v-qoh = 0.
      END.
   END.
END. /* each cust */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report2 C-Win 
PROCEDURE run-report2 :
/*{sys/form/r-topw.f}*/

DEF VAR fcus    LIKE itemfg.cust-no                NO-UNDO.
DEF VAR tcus    LIKE fcus INIT "zzzzzzzz"          NO-UNDO.
DEF VAR fitm    LIKE itemfg.i-no                   NO-UNDO.
DEF VAR titm    LIKE fitm INIT "zzzzzzzzzzzzzzz"   NO-UNDO.
DEF VAR fp-o    LIKE oe-ordl.po-no                 NO-UNDO.
DEF VAR tp-o    LIKE fp-o INIT "zzzzzzzzzzzzzzz"   NO-UNDO.
DEF VAR ford    LIKE oe-ord.ord-no FORMAT ">>>>>>" NO-UNDO.
DEF VAR tord    LIKE ford INIT 999999              NO-UNDO.
DEF VAR fjob    LIKE oe-ordl.job-no                NO-UNDO.
DEF VAR tjob    LIKE fjob INIT "zzzzzz"            NO-UNDO.
DEF VAR fjob2   LIKE oe-ordl.job-no2  FORMAT  "99" NO-UNDO.
DEF VAR tjob2   LIKE fjob2 INIT 99                 NO-UNDO.
DEF VAR v-cus   LIKE itemfg.cust-no                NO-UNDO.
DEF VAR v-itm   LIKE itemfg.i-no                   NO-UNDO.
DEF VAR v-price LIKE oe-ordl.price                 NO-UNDO.
DEF VAR v-fcat  LIKE itemfg.procat                 NO-UNDO.
DEF VAR v-tcat  LIKE itemfg.procat                 NO-UNDO.
DEF VAR v-fsman LIKE cust.sman                     NO-UNDO.
DEF VAR v-tsman LIKE cust.sman                     NO-UNDO.
DEF VAR v-shpdate LIKE fg-rcpth.trans-date         NO-UNDO.

DEF VAR vdet      AS LOG INIT yes FORMAT "Detail/Summary" NO-UNDO.
DEF VAR vinc      AS LOG INIT yes FORMAT "Yes/No"         NO-UNDO.
DEF VAR vinc1     AS LOG INIT yes FORMAT "Yes/No"         NO-UNDO.
DEF VAR v-printed AS LOG                                  NO-UNDO.

DEF VAR v-qty     AS INT NO-UNDO.
DEF VAR v-qop     AS INT NO-UNDO.
DEF VAR v-qoh     AS INT NO-UNDO.
DEF VAR v-bal     AS INT NO-UNDO.
DEF VAR v-shpdays AS INT NO-UNDO.
DEF VAR li-tqty   AS INT NO-UNDO.

DEF VAR v-val     AS DEC NO-UNDO.

DEF VAR v-job       AS CHAR FORMAT "x(9)"  NO-UNDO.
DEF VAR v-shp       AS CHAR FORMAT "x(10)" NO-UNDO.
/*DEF VAR excelheader AS CHAR                NO-UNDO.*/

DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
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


FORM HEADER 
        "        "
        "               "
        "               "
        "         "
        "    Qty  "
        " Trans  "
        " "
        "          "
        "      Qty  "
        "  Balance "
        "  Selling"
        "              "               SKIP

        "Cust #  "
        "Item #         "
        "Cust PO #      "
        "    Job #"
        "  Ordered"
        "  Date  "
        "C"
        "       Qty"
        "    On Hand"
        " Remaining"
        "    Price"
        "   Total Value"                    
        " Ship Date" 
        " Days of Last Ship"           SKIP

        "--------"
        "---------------"
        "---------------"
        "---------"
        "---------"
        "--------"
        "-"
        "----------"
        "-----------"
        "----------"
        "---------"
        "--------------"                    
        "----------"                      
        "------------------"           SKIP
        SKIP(1)
        WITH NO-BOX PAGE-TOP STREAM-IO WIDTH 180 FRAME TOP.

ASSIGN 
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}

   fcus   = begin_cust-no
   tcus   = end_cust-no
   fitm   = begin_i-no
   titm   = end_i-no
   fp-o   = begin_cust-po
   tp-o   = END_cust-po
   fjob   = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
            TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
   tjob   = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
            TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
   ford   = begin_ord-no
   tord   = end_ord-no
   /*vdet   = tb_detailed*/
   vinc   = tb_clo-ord
   vinc1  = tb_new-ord
   v-fcat  = begin_cat
   v-tcat  = end_cat
   v-fsman = begin_slm 
   v-tsman = end_slm 
   .

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

        IF LOOKUP(ttRptSelected.TextList, "Daily Sq Ft/M,Amount1,PTD Sq Ft/M,Amount2,YTD Sq Ft/M,Amount3") <> 0    THEN
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

IF td-show-parm THEN  
   RUN show-param.

/*IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Cust #,Item #,Cust PO #,Job #,Qty Ordered,"
               + "Trans Date,C,Qty,Qty On Hand,Balance Remaining,Selling Price,Total Value,"
               + "Last Ship Date,Days Since Last Shipment".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.*/

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

DISPLAY WITH FRAME TOP.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    EACH cust NO-LOCK WHERE cust.company        EQ cocode
                        AND cust.cust-no        EQ ttCustList.cust-no /*fcus
                        AND cust.cust-no        LE tcus*/
                        AND cust.sman           GE v-fsman  
                        AND cust.sman           LE v-tsman,
   EACH itemfg NO-LOCK WHERE itemfg.company     EQ cust.company
                         AND itemfg.cust-no     EQ cust.cust-no
                         AND itemfg.i-no        GE fitm
                         AND itemfg.i-no        LE titm 
                         AND itemfg.procat      GE v-fcat
                         AND itemfg.procat      LE v-tcat USE-INDEX customer,
   EACH oe-ordl NO-LOCK WHERE oe-ordl.company   EQ itemfg.company
                          AND oe-ordl.i-no      EQ itemfg.i-no
                          AND oe-ordl.cust-no   EQ cust.cust-no
                          AND (oe-ordl.opened   EQ YES OR vinc)      
                          AND oe-ordl.po-no     GE fp-o
                          AND oe-ordl.po-no     LE tp-o      
                          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99")) GE fjob
                          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99")) LE tjob
                          AND oe-ordl.ord-no    GE ford
                          AND oe-ordl.ord-no    LE tord USE-INDEX item,
   FIRST oe-ord NO-LOCK WHERE oe-ord.company        EQ oe-ordl.company
                          AND oe-ord.ord-no         EQ oe-ordl.ord-no
                     BREAK BY itemfg.cust-no
                           BY itemfg.i-no
                           BY oe-ordl.ord-no

   WITH FRAME main NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 180 DOWN:

    {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"} 

   IF FIRST-OF(itemfg.cust-no) THEN v-cus = itemfg.cust-no.

   IF FIRST-OF(itemfg.i-no) THEN v-itm = itemfg.i-no.

   IF FIRST-OF(oe-ordl.ord-no) THEN EMPTY TEMP-TABLE tt-report.

   ASSIGN
      v-job     = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                  TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")
      v-price   = oe-ordl.t-price / oe-ordl.qty * 1000
      v-printed = NO.

   IF TRIM(oe-ordl.job-no) NE "" THEN
   FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ oe-ordl.company
                               AND fg-rcpth.job-no  EQ oe-ordl.job-no
                               AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                               AND fg-rcpth.i-no    EQ oe-ordl.i-no USE-INDEX job,
      EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code USE-INDEX rm-rdtl
                         BREAK BY fg-rdtlh.loc
                               BY fg-rdtlh.loc-bin
                               BY fg-rdtlh.tag
                               BY fg-rcpth.trans-date
                               BY fg-rdtlh.trans-time
                               BY fg-rcpth.r-no:

      CREATE tt-report.
      ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                             STRING(MONTH(fg-rcpth.trans-date),"99")  +
                             STRING(DAY(fg-rcpth.trans-date),"99")
         tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
         tt-report.rec-id  = RECID(fg-rdtlh).
   END.

   FOR EACH oe-boll NO-LOCK WHERE oe-boll.company  EQ cocode
                              AND oe-boll.ord-no   EQ oe-ordl.ord-no
                              AND oe-boll.i-no     EQ oe-ordl.i-no
                              AND oe-boll.line     EQ oe-ordl.line
                              AND (oe-ordl.job-no  EQ ""             OR
                                  (oe-boll.job-no  EQ oe-ordl.job-no AND
                                   oe-boll.job-no2 EQ oe-ordl.job-no2))
                              AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.company EQ oe-boll.company
                                                           AND oe-bolh.b-no    EQ oe-boll.b-no
                                                           AND oe-bolh.posted  EQ YES) USE-INDEX ord-no
                              BREAK BY oe-boll.job-no
                                    BY oe-boll.job-no2
                                    BY oe-boll.loc
                                    BY oe-boll.loc-bin
                                    BY oe-boll.tag
                                    BY oe-boll.cust-no:

      IF LAST-OF(oe-boll.cust-no) THEN
         FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ oe-boll.company
                                     AND fg-rcpth.b-no    EQ oe-boll.b-no
                                     AND fg-rcpth.job-no  EQ oe-boll.job-no
                                     AND fg-rcpth.job-no2 EQ oe-boll.job-no2
                                     AND fg-rcpth.i-no    EQ oe-boll.i-no USE-INDEX b-no,
            EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                    AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                    AND fg-rdtlh.tag       EQ oe-boll.tag
                                    AND fg-rdtlh.loc       EQ oe-boll.loc
                                    AND fg-rdtlh.loc-bin   EQ oe-boll.loc-bin
                                    AND fg-rdtlh.cust-no   EQ oe-boll.cust-no USE-INDEX rm-rdtl
                                     BY fg-rcpth.trans-date
                                     BY fg-rdtlh.trans-time
                                     BY fg-rcpth.r-no:

            FIND FIRST tt-report WHERE tt-report.term-id = ""
                                   AND tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                                                           STRING(MONTH(fg-rcpth.trans-date),"99")  +
                                                           STRING(DAY(fg-rcpth.trans-date),"99")
                                   AND tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999") 
                                   AND tt-report.rec-id  = RECID(fg-rdtlh)NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-report THEN DO:
               CREATE tt-report.
               ASSIGN
                  tt-report.term-id = ""
                  tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                                      STRING(MONTH(fg-rcpth.trans-date),"99")  +
                                      STRING(DAY(fg-rcpth.trans-date),"99")
                  tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
                  tt-report.rec-id  = RECID(fg-rdtlh).
            END.

         END.
   END.

   IF LAST-OF(oe-ordl.ord-no) THEN DO:
      EMPTY TEMP-TABLE tt-fg-bin.

      FOR EACH tt-report WHERE tt-report.term-id EQ "",
         FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
         FIRST fg-rcpth WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no NO-LOCK
         BREAK BY tt-report.key-01
               BY tt-report.key-02:

         FIND FIRST tt-fg-bin WHERE tt-fg-bin.company EQ fg-rcpth.company
                                AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                                AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                                AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                                AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                                AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                                AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-fg-bin THEN DO:
            CREATE tt-fg-bin.
            ASSIGN
               tt-fg-bin.company = fg-rcpth.company
               tt-fg-bin.job-no  = fg-rcpth.job-no
               tt-fg-bin.job-no2 = fg-rcpth.job-no2
               tt-fg-bin.loc     = fg-rdtlh.loc
               tt-fg-bin.loc-bin = fg-rdtlh.loc-bin
               tt-fg-bin.tag     = fg-rdtlh.tag
               tt-fg-bin.cust-no = fg-rdtlh.cust-no.
         END.

         ASSIGN
            li-tqty = (fg-rdtlh.qty * IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 1)
            tt-fg-bin.qty = (IF fg-rcpth.rita-code EQ "C" THEN 0
                             ELSE tt-fg-bin.qty) + li-tqty.
         RELEASE tt-fg-bin.
         tt-report.qty = 0.

         FOR EACH tt-fg-bin:
            tt-report.qty = tt-report.qty + tt-fg-bin.qty.
         END.
      END.

      FOR EACH tt-report WHERE tt-report.term-id EQ "",
         FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
         FIRST fg-rcpth WHERE fg-rcpth.r-no   EQ fg-rdtlh.r-no 
                          AND fg-rcpth.rita-code NE "T" NO-LOCK
                     BREAK BY tt-report.key-01
                           BY tt-report.key-02:

         IF FIRST(tt-report.key-01) THEN
            ASSIGN
               v-qty = oe-ordl.qty
               v-bal = oe-ordl.qty               
               v-bal = v-bal + (fg-rdtlh.qty * IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 0).

         IF LAST(tt-report.key-01) OR vdet THEN DO:
            ASSIGN
               v-qoh = tt-report.qty
               v-val = round(v-qoh * (oe-ordl.t-price / oe-ordl.qty),2).

            /* gdm - 10260906 */
             ASSIGN v-shpdate = ?
                    v-shpdays = 0
                    v-shp     = ""
                    v-job     = "".

             FIND LAST bf-fg-rcpth NO-LOCK
               WHERE bf-fg-rcpth.company   EQ cocode
                 AND bf-fg-rcpth.i-no      EQ v-itm
                 AND bf-fg-rcpth.rita-code EQ "S" NO-ERROR.
             IF AVAIL bf-fg-rcpth 
               THEN ASSIGN v-shpdate = bf-fg-rcpth.trans-date
                           v-shpdays = (TODAY - bf-fg-rcpth.trans-date).

             ASSIGN v-job = FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                            TRIM(fg-rcpth.job-no) + "-" + 
                            STRING(fg-rcpth.job-no2,"99")
                    v-job = TRIM(v-job)
                    v-job = IF v-job = "-00" THEN " " ELSE v-job
                    v-shp = IF v-shpdays EQ 0 THEN " " ELSE STRING(v-shpdays,">>,>>>,>>9").

            /* gdm - 10260906 */

            DISPLAY 
               v-cus
               v-itm
               oe-ordl.po-no
               v-job                            FORMAT "x(9)"
               v-qty WHEN v-qty NE 0            FORMAT ">,>>>,>>9"
               fg-rcpth.trans-date WHEN vdet    FORMAT "99/99/99"
               fg-rcpth.rita-code WHEN vdet
               fg-rdtlh.qty WHEN vdet           FORMAT "->,>>>,>>9"
               v-qoh                            FORMAT "->>,>>>,>>9"
               v-bal WHEN v-bal GE 0            FORMAT ">>,>>>,>>9"
                   0 WHEN v-bal LT 0 @ v-bal
               v-price                          FORMAT ">>,>>9.99"
               v-val                            FORMAT "->>,>>>,>>9.99"
               v-shpdate                        FORMAT "99/99/99"
               v-shp                            FORMAT "x(10)"
               WITH FRAME main.
            DOWN WITH FRAME main.

            IF tb_excel THEN 
               PUT STREAM excel UNFORMATTED
                  '"' v-cus                                         '",'
                  '"' v-itm                                         '",'
                  '"' oe-ordl.po-no                                 '",'
                  '"' FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                      TRIM(fg-rcpth.job-no) + "-" + STRING(fg-rcpth.job-no2,"99") '",'
                  '"' (IF v-qty NE 0 THEN v-qty ELSE 0)             '",'
                  '"' (IF vdet AND fg-rcpth.trans-date <> ? THEN
                        STRING(fg-rcpth.trans-date,"99/99/99")
                      ELSE "")    '",'
                  '"' (IF vdet THEN fg-rcpth.rita-code ELSE "")     '",'
                  '"' (IF vdet THEN STRING(fg-rdtlh.qty) ELSE "")           '",'
                  '"' v-qoh                                         '",'
                  '"' (IF v-bal GE 0 THEN v-bal ELSE 0)             '",'
                  '"' v-price                                       '",'
                  '"' v-val                                         '",'
                  '"' v-shpdate                                     '",'
                  '"' v-shpdays                                     '",'
                  SKIP.

            ASSIGN
               v-printed = YES
               v-cus     = ""
               v-itm     = "".
         END.

        IF LAST(tt-report.key-01) AND vdet THEN PUT SKIP(1).
      END.

      IF NOT v-printed AND ((vinc1 AND INDEX("CZ",oe-ord.stat) EQ 0) OR
                            (vinc AND INDEX("CZ",oe-ord.stat) GT 0))    THEN DO:

        ASSIGN v-shpdate = ?
               v-shpdays = 0
               v-shp     = ""
               v-job     = "".

        FIND LAST bf-fg-rcpth NO-LOCK
          WHERE bf-fg-rcpth.company   EQ cocode
            AND bf-fg-rcpth.i-no      EQ v-itm
            AND bf-fg-rcpth.rita-code EQ "S" NO-ERROR.
        IF AVAIL bf-fg-rcpth 
          THEN ASSIGN v-shpdate = bf-fg-rcpth.trans-date
                      v-shpdays = (TODAY - bf-fg-rcpth.trans-date).

        ASSIGN v-job = TRIM(v-job)
               v-job = IF v-job = "-00" THEN " " ELSE v-job
               v-shp = IF v-shpdays EQ 0 THEN " " ELSE STRING(v-shpdays,">>,>>>,>>9").

         DISPLAY 
            v-cus
            v-itm
            oe-ordl.po-no
            v-job
            oe-ordl.qty       @ v-qty
            oe-ord.ord-date   @ fg-rcpth.trans-date
            v-price
            v-shpdate 
            v-shp FORMAT "x(10)"
         WITH FRAME main.

         PUT SKIP(1).

         IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED
               '"' v-cus                                         '",'
               '"' v-itm                                         '",'
               '"' oe-ordl.po-no                                 '",'
               '"' v-job                                         '",'
               '"' oe-ordl.qty                                   '",'
               '"' oe-ord.ord-date                               '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' ""                                            '",'
               '"' v-price                                       '",'
               '"' v-shpdate                                     '",'
               '"' v-shpdays                                     '",'
               SKIP.
         ASSIGN
            v-cus = ""
            v-itm = ""
            v-qoh = 0.
      END.
   END.
END. /* each cust */

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

