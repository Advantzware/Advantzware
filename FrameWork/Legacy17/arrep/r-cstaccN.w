&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:

  Created:

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


/*{sys/inc/custlistform.i ""AR10"" }*/

{sys/ref/CustList.i NEW}

DEF TEMP-TABLE tt-report FIELD actnum LIKE account.actnum
                         FIELD cust-no LIKE cust.cust-no
                         FIELD inv-no LIKE ar-inv.inv-no
                         FIELD jrnl LIKE gltrans.jrnl
                         FIELD tr-date LIKE ar-ledger.tr-date
                         FIELD tr-num LIKE ar-ledger.tr-num
                         FIELD amt AS DEC
                         INDEX detail actnum cust-no inv-no jrnl.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

ASSIGN cTextListToSelect = "GL Acct#,Description,Customer,Inv#,Journal,Run#,Date,Amount"
       cFieldListToSelect = "act,dscr,cust,inv,jrnl,run,date,amt"
       cFieldLength = "25,30,8,6,8,6,10,14"
       cFieldType = "c,c,c,i,c,i,c,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "GL Acct#,Description,Customer,Inv#,Journal,Run#,Date,Amount"  .

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
begin_cust end_cust begin_date end_date begin_acct end_acct sl_avail ~
Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_date end_date begin_acct end_acct sl_avail sl_selected rd-dest ~
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

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(20)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cstacc.csv" 
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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.43.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .95 NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 2.05 COL 36 WIDGET-ID 6
     btnCustList AT ROW 2.1 COL 68.4 WIDGET-ID 8
     begin_cust AT ROW 3.29 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 3.29 COL 63 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 4.57 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_date AT ROW 4.57 COL 63 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_acct AT ROW 5.86 COL 19 COLON-ALIGNED HELP
          "Enter Beginning GL Account Number"
     end_acct AT ROW 5.86 COL 63 COLON-ALIGNED HELP
          "Enter Ending GL Account Number"
     sl_avail AT ROW 8.33 COL 4.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 8.33 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 8.33 COL 59.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.33 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.33 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11.38 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 12.38 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 15 COL 6 NO-LABEL
     lv-ornt AT ROW 15.24 COL 30 NO-LABEL
     lines-per-page AT ROW 15.24 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 16.91 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 17.86 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.81 COL 29
     tb_excel AT ROW 19.86 COL 49 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.86 COL 51
     fi_file AT ROW 20.81 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.38 COL 19
     btn-cancel AT ROW 22.38 COL 58
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 7.62 COL 59.8 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 7.62 COL 5.2 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.29 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 13.81 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 23.24.


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
         TITLE              = "AR Accounts by Customer"
         HEIGHT             = 23.48
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
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* AR Accounts by Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AR Accounts by Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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
    ASSIGN {&displayed-objects}.
  END.

  RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust,
                        INPUT END_cust).
 END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 
  SESSION:SET-WAIT-STATE ("").

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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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



&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
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

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "AR10",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'AR10',
                          INPUT NO,
                          OUTPUT glCustListActive).

  {sys/inc/chblankcust.i ""AR10""}
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
                            INPUT 'AR10',
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
                                  INPUT 'AR10').


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
  DISPLAY tb_cust-list begin_cust end_cust begin_date end_date begin_acct 
          end_acct sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust end_cust begin_date 
         end_date begin_acct end_acct sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*     /*     CREATE-TEST-FILE*/                           */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
{custom/out2file.i}

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
/*{sys/form/r-topw.f}   */

DEF VAR lv-jrnl LIKE gltrans.jrnl NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR lv-amt LIKE tt-report.amt EXTENT 3 NO-UNDO.
DEF VAR ld-tax-rate AS DEC  EXTENT 4 NO-UNDO.
def var v-ttl-tax  as decimal no-undo.
def var v-ttl-rate as decimal no-undo.

DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEF VAR fcust AS CHAR NO-UNDO.
DEF VAR tcust AS CHAR NO-UNDO.

FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
     account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)"
     tt-report.cust-no     COLUMN-LABEL "Customer"      
     /*cust.name             COLUMN-LABEL "Name"        FORMAT "x(20)"*/
     tt-report.inv-no      COLUMN-LABEL "Inv#"
     tt-report.jrnl        COLUMN-LABEL "Journal"
     tt-report.tr-num      COLUMN-LABEL "Run#"
     tt-report.tr-date     COLUMN-LABEL "Date"
     tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"

    WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


SESSION:SET-WAIT-STATE ("general").

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}. 
assign
     fcust = begin_cust
     tcust = END_cust 
     lSelected  = tb_cust-list .

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


        IF LOOKUP(ttRptSelected.TextList, "Amount") <> 0    THEN
            ASSIGN
            str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

 END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    /*excelHeader = 'GL Acct#,Description,Customer,Inv#,Journal,Run#,Date,Amount'.*/
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
END.

IF td-show-parm THEN RUN show-param.

FOR EACH tt-report:
  DELETE tt-report.
END.

DISPLAY "" WITH FRAME r-top.
FOR EACH ar-ledger
    WHERE ar-ledger.company EQ cocode
      /*AND ar-ledger.cust-no GE begin_cust
      AND ar-ledger.cust-no LE end_cust*/
      AND ar-ledger.cust-no GE fcust
      AND ar-ledger.cust-no LE tcust
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-ledger.cust-no
      AND ttCustList.log-fld no-lock) else true)
      AND ar-ledger.cust-no NE ""
      AND ar-ledger.tr-date GE begin_date
      AND ar-ledger.tr-date LE end_date
    NO-LOCK:

    {custom/statusMsg.i " 'Processing Customer#  '  + string(ar-ledger.cust-no) "}

  IF ar-ledger.ref-num BEGINS "INV# " THEN DO:
    FIND FIRST ar-inv
        WHERE ar-inv.company EQ ar-ledger.company
          AND ar-inv.inv-no  EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
        NO-LOCK NO-ERROR.

    IF AVAIL ar-inv THEN DO:
      lv-jrnl = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt
                THEN "ARINV" ELSE "OEINV".

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ar-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ar-ctrl.receivables
       tt-report.amt    = (IF lv-jrnl EQ "ARINV"
                           THEN ar-inv.net ELSE ar-inv.gross) * -1.

      IF ar-inv.tax-amt NE 0 THEN DO:
        RELEASE stax.
        IF ar-inv.tax-code NE "" THEN
        FIND FIRST stax
            WHERE stax.company   EQ ar-inv.company
              AND stax.tax-group EQ ar-inv.tax-code
            NO-LOCK NO-ERROR.

        IF AVAIL stax THEN DO:
          assign v-ttl-tax  = 0
                 v-ttl-rate = 0.


          DO li = 1 TO extent(stax.tax-rate1): 

            if stax.tax-rate1[li] = 0 then next.

            ld-tax-rate[li] = stax.tax-rate1[li].

            IF stax.accum-tax AND li GT 1 THEN
           DO lj = 1 TO li - 1: 

              ld-tax-rate[li] = ld-tax-rate[li] +
                                (ld-tax-rate[li] * (stax.tax-rate[lj] / 100)).
            END.
            v-ttl-rate = v-ttl-rate + ld-tax-rate[li].
          END.

          DO li = 1 TO extent(stax.tax-rate1):
            if stax.tax-rate1[li] = 0 then next.
            ASSIGN ld-tax-rate[li] = ROUND(ld-tax-rate[li] / v-ttl-rate *
                                           ar-inv.tax-amt,2)
                   v-ttl-tax = v-ttl-tax + ld-tax-rate[li].
          END.


          IF ar-inv.tax-amt NE v-ttl-tax THEN
            ld-tax-rate[1] = ld-tax-rate[1] +
                             (ar-inv.tax-amt - v-ttl-tax).

          DO li = 1 TO extent(stax.tax-rate1):
            if stax.tax-rate1[li] = 0 then next.
            CREATE tt-report.
            ASSIGN 
             tt-report.inv-no = ar-inv.inv-no
             tt-report.jrnl   = lv-jrnl
             tt-report.actnum = stax.tax-acc1[li]
             tt-report.amt    = ld-tax-rate[li].
          END. /* 1 to 3 */
        END. /* avail stax */

        ELSE DO:
          CREATE tt-report.
          ASSIGN 
           tt-report.inv-no = ar-inv.inv-no
           tt-report.jrnl   = lv-jrnl
           tt-report.actnum = ar-ctrl.stax
           tt-report.amt    = ar-inv.tax-amt.
        END.
      END.

      IF ar-inv.f-bill THEN DO:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no = ar-inv.inv-no
         tt-report.jrnl   = lv-jrnl
         tt-report.actnum = ar-ctrl.freight
         tt-report.amt    = ar-inv.freight.
      END.

      FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no NO-LOCK:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ar-inv.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ar-invl.actnum
         tt-report.amt     = ar-invl.amt.
      END.
    END.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "Memo#" THEN DO:    
    FOR EACH ar-cash
        WHERE ar-cash.company  EQ ar-ledger.company
          AND ar-cash.posted   EQ YES
          AND ar-cash.memo     EQ YES
          AND ar-cash.cust-no  EQ ar-ledger.cust-no
          AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,8))
        NO-LOCK,

        EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK:   /* task 12111307 */
      /* gdm - 09240903 */
      lv-jrnl = IF ar-cashl.amt-paid - ar-cashl.amt-disc GT 0
                THEN "DBMEM" ELSE "CRMEM".

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-ctrl.receivables
       tt-report.amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc) * -1
/*        tt-report.jrnl    = IF tt-report.amt < 0        */
/*                              THEN "DBMEM" ELSE "CRMEM" */
                                 .

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-cashl.actnum
       tt-report.amt     = ar-cashl.amt-paid - ar-cashl.amt-disc
/*        tt-report.jrnl    = IF tt-report.amt < 0        */
/*                              THEN "DBMEM" ELSE "CRMEM" */
                                 .
    END.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "CHK# " THEN DO:
    lv-jrnl = "CASHR".

    FOR EACH ar-cash
        WHERE ar-cash.company  EQ ar-ledger.company
          AND ar-cash.posted   EQ YES
          AND ar-cash.memo     EQ NO
          AND ar-cash.cust-no  EQ ar-ledger.cust-no
          AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
        NO-LOCK :

        {custom/statusMsg.i " 'Processing Customer#  '  + string(ar-cash.cust-no) "}

      /*  FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK: */
        for each ar-cashl where ar-cashl.c-no = ar-cash.c-no NO-LOCK
         break by ar-cashl.inv-no :

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-ctrl.receivables
       tt-report.amt     = ar-cashl.amt-paid + ar-cashl.amt-disc.

   /*   CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-cashl.actnum
       tt-report.amt     = ar-cashl.amt-paid * -1. */

      IF ar-cashl.amt-disc NE 0 THEN DO:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ar-cashl.inv-no
         tt-report.jrnl    = "CRDIS"
         tt-report.actnum  = ar-ctrl.discount
         tt-report.amt     = ar-cashl.amt-disc * -1.
      END.
    END.
  END.
END.
END.

FOR EACH tt-report
    WHERE tt-report.actnum GE begin_acct
      AND tt-report.actnum LE end_acct
      AND tt-report.actnum NE ""
      AND tt-report.amt    NE 0
    USE-INDEX detail
    BREAK BY tt-report.actnum
          BY tt-report.cust-no
          BY tt-report.inv-no
          BY tt-report.jrnl:

    {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-report.cust-no) "}

  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.actnum
      NO-LOCK NO-ERROR.

  FIND FIRST cust
      WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.cust-no
      NO-LOCK NO-ERROR.



/*  DISPLAY tt-report.actnum WHEN FIRST-OF(tt-report.actnum)
          account.dscr WHEN AVAIL account AND FIRST-OF(tt-report.actnum)
              "Not on File" WHEN NOT AVAIL account AND FIRST-OF(tt-report.actnum)
                @ account.dscr
          tt-report.cust-no WHEN FIRST-OF(tt-report.cust-no)
          /*cust.name WHEN AVAIL cust AND FIRST-OF(tt-report.cust-no)
              "Not on File" WHEN NOT AVAIL cust AND FIRST-OF(tt-report.cust-no)
                @ cust.name*/
          tt-report.inv-no
          tt-report.jrnl
          tt-report.tr-num
          tt-report.tr-date
          tt-report.amt
      WITH FRAME detail.
  DOWN WITH FRAME detail. 

  IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
       '"' (IF FIRST-OF(tt-report.actnum) THEN tt-report.actnum ELSE "") '",'
       '"' (IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN account.dscr
            ELSE IF NOT AVAIL account AND FIRST-OF(tt-report.actnum) THEN
                "Not on File"
            ELSE "")                                                     '",'
       '"'  (IF FIRST-OF(tt-report.cust-no) THEN tt-report.cust-no
             ELSE "")                                                    '",'
       '"' tt-report.inv-no                                              '",' 
       '"' tt-report.jrnl                                                '",'
       '"' tt-report.tr-num                                              '",'
       '"' (IF tt-report.tr-date NE ? THEN
            STRING(tt-report.tr-date,"99/99/9999") ELSE "")              '",'
       '"' STRING(tt-report.amt,'->>,>>>,>>9.99')                        '",'
      SKIP. */

      ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "act"     THEN cVarValue = IF FIRST-OF(tt-report.actnum) THEN tt-report.actnum ELSE "" .
                  WHEN "dscr"    THEN cVarValue = IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN account.dscr ELSE IF NOT AVAIL account AND FIRST-OF(tt-report.actnum) THEN "Not on File" ELSE "" .
                  WHEN "cust"    THEN cVarValue = IF FIRST-OF(tt-report.cust-no) THEN tt-report.cust-no ELSE "" .
                  WHEN "inv"     THEN cVarValue = STRING(tt-report.inv-no) .
                  WHEN "jrnl"    THEN cVarValue = STRING(tt-report.jrnl) .
                  WHEN "run"     THEN cVarValue = STRING(tt-report.tr-num) .
                  WHEN "date"    THEN cVarValue = IF tt-report.tr-date NE ? THEN STRING(tt-report.tr-date,"99/99/9999") ELSE ""  .
                  WHEN "amt"     THEN cVarValue = STRING(tt-report.amt,"->>,>>>,>>9.99")   .

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

  lv-amt[1] = lv-amt[1] + tt-report.amt.

  IF LAST-OF(tt-report.cust-no) THEN DO WITH FRAME detail:
  /*  PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "Customer" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[1]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' "Customer"                         '",'
         '"' "Totals"                           '",' 
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' STRING(lv-amt[1],'->>,>>>,>>9.99') '",'
        SKIP. */
    PUT SKIP str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "act"     THEN cVarValue = "" .
                  WHEN "dscr"    THEN cVarValue = "" .
                  WHEN "cust"    THEN cVarValue = "" .
                  WHEN "inv"     THEN cVarValue = "" .
                  WHEN "jrnl"    THEN cVarValue = "" .
                  WHEN "run"     THEN cVarValue = "" .
                  WHEN "date"    THEN cVarValue = ""  .
                  WHEN "amt"     THEN cVarValue = STRING(lv-amt[1],"->>,>>>,>>9.99")   .

             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
     END.
     PUT UNFORMATTED  "       Customer Totals" substring(cDisplay,23,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "  Customer Totals  " + substring(cExcelDisplay,3,300) SKIP.
     END.

    ASSIGN
     lv-amt[2] = lv-amt[2] + lv-amt[1]
     lv-amt[1] = 0.

    PUT SKIP(1).
  END.

  IF LAST-OF(tt-report.actnum) THEN DO WITH FRAME detail:
   /* PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Acct#" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[2]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' "Acct#"                            '",'
         '"' "Totals"                           '",' 
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' STRING(lv-amt[2],'->>,>>>,>>9.99') '",'
        SKIP. */
      PUT SKIP str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "act"     THEN cVarValue = "" .
                  WHEN "dscr"    THEN cVarValue = "" .
                  WHEN "cust"    THEN cVarValue = "" .
                  WHEN "inv"     THEN cVarValue = "" .
                  WHEN "jrnl"    THEN cVarValue = "" .
                  WHEN "run"     THEN cVarValue = "" .
                  WHEN "date"    THEN cVarValue = ""  .
                  WHEN "amt"     THEN cVarValue = STRING(lv-amt[2],"->>,>>>,>>9.99")   .

             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
     END.
     PUT UNFORMATTED  "       Acct# Totals" substring(cDisplay,20,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "  Acct# Totals  " + substring(cExcelDisplay,3,300) SKIP.
     END.

    ASSIGN
     lv-amt[3] = lv-amt[3] + lv-amt[2]
     lv-amt[2] = 0.

    PUT SKIP(3).
  END.

  IF LAST(tt-report.actnum) THEN DO WITH FRAME detail:
  /*  PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Grand" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[3]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' "Grand Totals"                     '",' 
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' ""                                 '",'
         '"' STRING(lv-amt[3],'->>,>>>,>>9.99') '",'
        SKIP. */
      PUT SKIP str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "act"     THEN cVarValue = "" .
                  WHEN "dscr"    THEN cVarValue = "" .
                  WHEN "cust"    THEN cVarValue = "" .
                  WHEN "inv"     THEN cVarValue = "" .
                  WHEN "jrnl"    THEN cVarValue = "" .
                  WHEN "run"     THEN cVarValue = "" .
                  WHEN "date"    THEN cVarValue = ""  .
                  WHEN "amt"     THEN cVarValue = STRING(lv-amt[3],"->>,>>>,>>9.99")   .

             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
     END.
     PUT UNFORMATTED  "       Grand Totals" substring(cDisplay,20,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "  Grand Totals  " + substring(cExcelDisplay,3,300) SKIP.
     END.
  END.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
  OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

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

