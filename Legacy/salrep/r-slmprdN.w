&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slmprd.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def TEMP-TABLE w-data no-undo
  field w-type      as   char
  field w-sman-no   as   char format "x(3)"
  field w-sqft      AS   DEC      extent 3   column-label "Sq Ft/M"
  field w-amt       AS   DEC      extent 3   column-label "Amount"
  field w-pmsf      AS   DEC      extent 3   column-label "$/MSF".

def TEMP-TABLE w-data1 NO-UNDO like w-data.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID.

DEF BUFFER b-tt-report FOR tt-report.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Rep,Salesrep Name,Daily Sq Ft/M,Daily Amount,Daily $/MSF,"
                           + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,"
                           + "YTD $/MSF"
       cFieldListToSelect = "rep,name,d-sqt,d-amt,d-msf," +
                            "ptd-sqt,ptd-amt,ptd-msf,ytd-sqt,ytd-amt," + "ytd-msf"
       cFieldLength = "3,20,13,13,11," + "13,13,10,13,13," + "10"
       cFieldType = "c,c,i,i,i," + "i,i,i,i,i," + "i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Rep,Salesrep Name,Daily Sq Ft/M,Daily Amount,Daily $/MSF,"
                           + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,"
                           + "YTD $/MSF" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 inv-date begin_slsmn end_slsmn ~
begin_i-no end_i-no tb_round tb_fin-chg sl_avail sl_selected Btn_Def ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file tb_batch btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS inv-date begin_slsmn end_slsmn begin_i-no ~
end_i-no tb_round tb_fin-chg sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file tb_batch 

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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)" 
     LABEL "Beginning FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-slmprd.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .95 NO-UNDO.

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
     SIZE 94 BY 9.95.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_round AS LOGICAL INITIAL no 
     LABEL "Round Amounts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

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
     inv-date AT ROW 2.62 COL 26 COLON-ALIGNED HELP
          "Enter Invoice Date"
     begin_slsmn AT ROW 3.81 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 2
     end_slsmn AT ROW 3.81 COL 68 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 4
     begin_i-no AT ROW 4.81 COL 26 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number"
     end_i-no AT ROW 4.81 COL 68 COLON-ALIGNED HELP
          "Enter Ending FG Item Number"
     tb_round AT ROW 6.29 COL 35
     tb_fin-chg AT ROW 7.48 COL 35
     sl_avail AT ROW 9.71 COL 4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 9.71 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 9.81 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 10.81 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.81 COL 39 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.86 COL 39 WIDGET-ID 40
     btn_down AT ROW 13.86 COL 39 WIDGET-ID 42
     rd-dest AT ROW 16.67 COL 6 NO-LABEL
     lv-ornt AT ROW 16.91 COL 30 NO-LABEL
     lines-per-page AT ROW 16.91 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 19.05 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 20 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.19 COL 30
     tb_excel AT ROW 22.29 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.29 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 23.1 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     tb_batch AT ROW 24.33 COL 6
     btn-ok AT ROW 25.95 COL 22
     btn-cancel AT ROW 25.95 COL 59
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9 COL 59.4 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.71 COL 4
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9 COL 4.8 WIDGET-ID 38
     RECT-6 AT ROW 15.52 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.91.


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
         TITLE              = "Sales Rep Production"
         HEIGHT             = 27.14
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
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_round:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Rep Production */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Rep Production */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning FG Item# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
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

  g_batch = tb_batch.

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.
  RUN GetSelectionList.
  run run-report. 

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
              {custom/asimail.i &TYPE = "Salesman"
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending FG Item# */
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


&Scoped-define SELF-NAME tb_round
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_round C-Win
ON VALUE-CHANGED OF tb_round IN FRAME FRAME-A /* Round Amounts? */
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
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

  assign
   inv-date = TODAY.
  IF g_batch THEN tb_batch = YES.
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO inv-date.
  END.

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:57 am */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:57 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:43:14 am */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY inv-date begin_slsmn end_slsmn begin_i-no end_i-no tb_round tb_fin-chg 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file tb_batch 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 inv-date begin_slsmn end_slsmn begin_i-no end_i-no 
         tb_round tb_fin-chg sl_avail sl_selected Btn_Def Btn_Add Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file tb_batch btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {BATCH/runbatch.i "salrep\s-slmprd.r"}

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

def var called as log no-undo.

called = NO.

def var fdate as date extent 3 no-undo.
/* [day] [period] [year] */
def var edate as date extent 3 no-undo.
def var tdate as date format "99/99/9999" no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" no-undo.
def var v-inc-fc as log init no no-undo.

def var v-sman-no   as   char format "x(3)" no-undo.
def var v-amt       AS   DEC no-undo.
def var v-sqft      AS   DEC no-undo.

def var v-tot-sqft  like v-sqft extent 6.
def var v-tot-amt   like v-amt  extent 6.
def var v-tot-pmsf  like v-amt  extent 6.

def var v-period as int no-undo.
def var v-year as int format "9999" no-undo.
def var v-qty like ar-invl.ship-qty format "->>>,>>9.99" no-undo.
def var v-exclude as log no-undo.
def var v-pct as dec format "99.99" no-undo.
def var v-misc as log no-undo.
def var v-leave as log no-undo.
def var v-fac as int.
def var head as char no-undo.
DEF VAR v-term LIKE tt-report.term-id NO-UNDO.
DEF VAR lv-i-no LIKE itemfg.i-no NO-UNDO.
DEF VAR lv-r-no LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type AS CHAR NO-UNDO.
DEF VAR ld-inv-pct  AS   DEC NO-UNDO.
DEF VAR lv-format-1 AS CHAR NO-UNDO.
DEF VAR lv-format-2 AS CHAR NO-UNDO.
DEF VAR lv-format-3 AS CHAR NO-UNDO.

DEF BUFFER b-ar-invl FOR ar-invl.
DEF VAR v-head-line AS CHAR NO-UNDO.
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

form w-data1.w-sman-no  column-label "No"
     sman.sname         column-label "Name" format "x(17)"
     w-data1.w-sqft[1]                      format "->>,>>9.999"
     w-data1.w-amt[1]                       format "->,>>>,>>9.99"
     w-data1.w-pmsf[1]                      format "->>,>>9.99"
     w-data1.w-sqft[2]                      format "->>,>>9.999"
     w-data1.w-amt[2]                       format "->,>>>,>>9.99"
     w-data1.w-pmsf[2]                      format "->>,>>9.99"
     w-data1.w-sqft[3]                      format "->>,>>9.999"
     w-data1.w-amt[3]                       format "->,>>>,>>9.99"
     w-data1.w-pmsf[3]                      format "->>,>>9.99"
     header skip(1)
     "    Salesrep"
     "---------------Daily----------------" at 23
     "-----------Period to Date-----------"
     "------------Year to Date------------"

    with no-box frame f-prod down STREAM-IO width 144.

assign
 fdate = today
 tdate = today
 edate[3] = TODAY
 head = if called then "SALES ANALYSIS - SALESREP PERFORMANCE REPORT"
                  else "SALES ANALYSIS - SALESREP PRODUCTION REPORT".

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 tdate        = inv-date
 fsman        = begin_slsmn
 tsman        = end_slsmn
 v-inc-fc     = tb_fin-chg.

IF tb_round THEN DO:
  DO WITH FRAME f-prod:
    ASSIGN
     w-data1.w-sqft[1]:FORMAT = "->>,>>>,>>9"
     w-data1.w-amt[1]:FORMAT  = "->>>>,>>>,>>9"
     w-data1.w-pmsf[1]:FORMAT = "->,>>>,>>9"
     w-data1.w-sqft[2]:FORMAT = "->>,>>>,>>9"
     w-data1.w-amt[2]:FORMAT  = "->>>>,>>>,>>9"
     w-data1.w-pmsf[2]:FORMAT = "->,>>>,>>9"
     w-data1.w-sqft[3]:FORMAT = "->>,>>>,>>9"
     w-data1.w-amt[3]:FORMAT  = "->>>>,>>>,>>9"
     w-data1.w-pmsf[3]:FORMAT = "->,>>>,>>9"
     lv-format-1 = "->>,>>>,>>9"
     lv-format-2 = "->>>>,>>>,>>9"
     lv-format-3 = "->,>>>,>>9".
  END.
END.
ELSE
   ASSIGN
     lv-format-1 = "->>,>>9.999"
     lv-format-2 = "->,>>>,>>9.99"
     lv-format-3 = "->>,>>9.99".

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

        IF LOOKUP(ttRptSelected.TextList, "Daily Sq Ft/M,Daily Amount,Daily $/MSF,PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,YTD $/MSF") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "No,Salesrep Name,Daily Sq Ft/M,Daily Amount,Daily $/MSF,"
              + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,"
              + "YTD $/MSF".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE w-data.
EMPTY TEMP-TABLE w-data1.

{salrep/r-slmprdN.i}

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

