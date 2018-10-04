&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

DEF TEMP-TABLE tt-report LIKE report.
DEF STREAM excel.

assign
 cocode = gcompany
 locode = gloc.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect =  "MACHINE,START DATE,DUE DATE,CUSTOMER,CUSTOMER PART #,JOB #,"
                          + "JOB QTY,BOARD,SHEET SIZE,VENDOR,BOARD RECEIVED,GL/ST,FG SHIPPED"

       cFieldListToSelect = "mach,st-date,due-date,cust,cust-part,job," +
                            "job-qty,board,sheet-size,vend,board-rec,gl-st,fg-ship"
       cFieldLength = "8,10,10,8,15,9," + "10,10,30,8,14,5,12"
       cFieldType = "c,c,c,c,c,c," + "i,c,c,c,i,c,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "MACHINE,START DATE,DUE DATE,CUSTOMER,CUSTOMER PART #,JOB #,"
                          + "JOB QTY,BOARD,SHEET SIZE,VENDOR,BOARD RECEIVED,GL/ST,FG SHIPPED" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 thru_date begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 rd_fg-rcpt ~
tb_no-show-underrun tb_mch-hrs tb_dept-nt rd_mch-ind sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS thru_date begin_mach end_mach begin_job-no ~
begin_job-no2 end_job-no end_job-no2 lbl_fg-rcpt rd_fg-rcpt ~
tb_no-show-underrun tb_mch-hrs tb_dept-nt lbl_mch-ind rd_mch-ind sl_avail ~
sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-bckmch.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_fg-rcpt AS CHARACTER FORMAT "X(256)":U INITIAL "Show FG Receipts or Shipments?" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_mch-ind AS CHARACTER FORMAT "X(256)":U INITIAL "Machine Industry?" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .95 NO-UNDO.

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

DEFINE VARIABLE thru_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Thru Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_fg-rcpt AS CHARACTER INITIAL "Shipments" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Receipts", "Receipts",
"Shipments", "Shipments"
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rd_mch-ind AS CHARACTER INITIAL "Foldware" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Foldware", "Foldware",
"Corrware", "Corrware",
"Both", "Both",
"All", "All"
     SIZE 53 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_dept-nt AS LOGICAL INITIAL no 
     LABEL "Print Department Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_mch-hrs AS LOGICAL INITIAL no 
     LABEL "Print Remaining Machine Hours?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_no-show-underrun AS LOGICAL INITIAL yes 
     LABEL "Don't Show If Receipts Within Underrun?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.6 BY 1 NO-UNDO.

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
     thru_date AT ROW 2.19 COL 26 COLON-ALIGNED
     begin_mach AT ROW 3.14 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.14 COL 66 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.1 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.1 COL 66 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.1 COL 78 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     lbl_fg-rcpt AT ROW 5.52 COL 16 COLON-ALIGNED NO-LABEL
     rd_fg-rcpt AT ROW 5.52 COL 51 NO-LABEL
     tb_no-show-underrun AT ROW 6.71 COL 5 WIDGET-ID 2
     tb_mch-hrs AT ROW 6.71 COL 51
     tb_dept-nt AT ROW 8.05 COL 51
     lbl_mch-ind AT ROW 9.38 COL 16 COLON-ALIGNED NO-LABEL
     rd_mch-ind AT ROW 9.38 COL 38 NO-LABEL
     sl_avail AT ROW 12 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.05 COL 40 WIDGET-ID 40
     btn_down AT ROW 16.05 COL 40 WIDGET-ID 42
     rd-dest AT ROW 18.52 COL 6 NO-LABEL
     lv-ornt AT ROW 18.52 COL 31 NO-LABEL
     lines-per-page AT ROW 18.52 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 19.95 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 20.91 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.1 COL 30
     tb_excel AT ROW 23.05 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.05 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 23.86 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.14 COL 18
     btn-cancel AT ROW 26.14 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.29 COL 4.8 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.29 COL 59.4 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.81 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 17.62 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 27.


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
         TITLE              = "Job Backlog by Machine"
         HEIGHT             = 27.24
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
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_fg-rcpt IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_fg-rcpt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_fg-rcpt".

/* SETTINGS FOR FILL-IN lbl_mch-ind IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_mch-ind:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_mch-ind".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_fg-rcpt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_mch-ind:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_dept-nt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_mch-hrs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_no-show-underrun:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       thru_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Backlog by Machine */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Backlog by Machine */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
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


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
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
  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
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


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
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


&Scoped-define SELF-NAME rd_fg-rcpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fg-rcpt C-Win
ON VALUE-CHANGED OF rd_fg-rcpt IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_mch-ind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_mch-ind C-Win
ON VALUE-CHANGED OF rd_mch-ind IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_dept-nt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dept-nt C-Win
ON VALUE-CHANGED OF tb_dept-nt IN FRAME FRAME-A /* Print Department Notes? */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME tb_mch-hrs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mch-hrs C-Win
ON VALUE-CHANGED OF tb_mch-hrs IN FRAME FRAME-A /* Print Remaining Machine Hours? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_no-show-underrun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_no-show-underrun C-Win
ON VALUE-CHANGED OF tb_no-show-underrun IN FRAME FRAME-A /* Don't Show If Receipts Within Underrun? */
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


&Scoped-define SELF-NAME thru_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL thru_date C-Win
ON LEAVE OF thru_date IN FRAME FRAME-A /* Thru Date */
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

  THRU_date = TODAY.   

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CEMENU"
      no-lock no-error.
  rd_mch-ind = if avail sys-ctrl               and
               sys-ctrl.char-fld eq "Corrware" then "Corrware"
                                               ELSE "Foldware".
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
     RUN DisplaySelectionList2.
    APPLY "entry" TO thru_date.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY thru_date begin_mach end_mach begin_job-no begin_job-no2 end_job-no 
          end_job-no2 lbl_fg-rcpt rd_fg-rcpt tb_no-show-underrun tb_mch-hrs 
          tb_dept-nt lbl_mch-ind rd_mch-ind sl_avail sl_selected rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 thru_date begin_mach end_mach begin_job-no begin_job-no2 
         end_job-no end_job-no2 rd_fg-rcpt tb_no-show-underrun tb_mch-hrs 
         tb_dept-nt rd_mch-ind sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/* ----------------------------------------------- jc/rep/jc-summ.p 07/98 JLF */
/* Job Cost Summary Report                                                    */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3w.f}*/

def buffer b-jh for job-hdr.

def var v-fdate as   date format "99/99/9999" init TODAY NO-UNDO.
def var v-fmach like job-mch.m-code NO-UNDO.
def var v-tmach like v-fmach                  init "zzzzzz" NO-UNDO.
def var v-fjob  like job.job-no NO-UNDO.
def var v-tjob  like v-fjob                   init "zzzzzz" NO-UNDO.
def var v-fjob2 like job.job-no2 NO-UNDO.
def var v-tjob2 like v-fjob2                  init 99 NO-UNDO.
def var v-ship  as   log format "Ship/Rec"    init YES NO-UNDO.
def var v-remm  as   log format "Y/N"         init NO NO-UNDO.
def var v-ddat  as   log format "Due/Start"   init YES NO-UNDO.
def var v-note  as   log format "Y/N"         init NO NO-UNDO.
def var v-indus as   char format "!" NO-UNDO.

def var v-up     like eb.num-up NO-UNDO.
def var v-on     like v-up NO-UNDO.

def var v-hdr     as   char format "x(131)" extent 3 NO-UNDO.
def var v-mach    like job-mch.m-code format "x(8)" NO-UNDO.
def var v-date    as   date format "99/99/99" NO-UNDO.
def var v-due-date    as   date format "99/99/99" NO-UNDO.
def var v-job     as   char format "x(9)" NO-UNDO.
def var v-sheet   as   char format "x(19)" NO-UNDO.
def var v-gl      as   char format "x(3)" NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v-pct     as   DEC NO-UNDO.
def var v-mat-qty as   dec format "->>>,>>>,>>9" NO-UNDO.
def var v-fg-qty  like v-mat-qty NO-UNDO.
def var v-rem-hr  like job-mch.run-hr extent 6 NO-UNDO.
def var v-m-list  as   char format "x(61)" NO-UNDO.
def var v-factor  as   DEC NO-UNDO.
def var v-first   as   log extent 2 NO-UNDO.

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

/*form header
     skip
     "Machine:"
     v-mach
     skip(1)

    with frame r-top.

form header v-hdr skip(1) with frame r-top.*/

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-fdate   = thru_date
  v-fmach   = begin_mach
  v-tmach   = END_mach

  v-fjob    = fill(" ",6 - length(trim(begin_job-no))) +
               trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob    = fill(" ",6 - length(trim(end_job-no)))   +
               trim(end_job-no)   + string(int(end_job-no2),"99") 

  v-hdr[1]  = "START/" + fill(" ",102) + "BOARD"
  v-hdr[2]  = "DUE DATE  CUSTOMER CUSTOMER PART #     JOB #    JOB QTY "   +
                "BOARD           SHEET SIZE          VENDOR       RECEIVED " +
                "GL/ST " + (if v-ship then " FG SHIPPED" else "FG RECEIVED")
  v-hdr[3]  = fill("-",131)

  v-ship    = rd_fg-rcpt EQ "Shipments"   
  v-remm    = tb_mch-hrs 
  /*v-ddat    = rd_due-start EQ "Due Date"   */
  v-note    = tb_dept-nt 
  v-indus   = SUBSTR(rd_mch-ind,1,1). 

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

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "MACHINE,START/DUE DATE,CUSTOMER,CUSTOMER PART #,JOB #,"
              + "JOB QTY,BOARD,SHEET SIZE,VENDOR,BOARD RECEIVED,GL/ST,"
              + "FG SHIPPED".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

    for each tt-report:
      delete tt-report.
    end.

    for each job
        where job.company eq cocode
          and job.stat    lt "C"
    {jc/rep/jc-backN.i}

    for each job
        where job.company eq cocode
          and job.stat    gt "C"
          and job.stat    lt "Z"
    {jc/rep/jc-backN.i}

    for each job
        where job.company eq cocode
          and job.stat    gt "Z"
    {jc/rep/jc-backN.i}

    for each tt-report where tt-report.term-id eq "",
        first job-hdr where recid(job-hdr) eq tt-report.rec-id no-lock
        break by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03

        transaction:

      if first-of(tt-report.key-01) then do:
        v-mach = tt-report.key-01.
        IF first(tt-report.key-01) THEN display "" with frame r-top.
        ELSE page.
      end.

      for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job-hdr.job
            and (job-mat.frm    eq job-hdr.frm or
                 tt-report.key-04  eq "SET")
          use-index seq-idx no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq job-mat.i-no
            and item.mat-type eq "B"
          no-lock:

        for each mat-act
            where mat-act.company eq cocode
              and mat-act.job     eq job-mat.job
              and mat-act.s-num   eq job-mat.frm
              and mat-act.b-num   eq job-mat.blank-no
              and mat-act.i-no    eq job-mat.i-no
            use-index job no-lock:

          run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                 job-mat.len, job-mat.wid, item.s-dep,
                                 mat-act.qty, output v-qty).

          v-mat-qty = v-mat-qty + v-qty.
        end.

        assign
         v-pct = 1
         v-up  = 1
         v-on  = 1.

        find est where est.company EQ job-hdr.company
                   AND est.est-no  EQ job-hdr.est-no
                 no-lock no-error.

        if avail est then do:
          run sys/inc/numup.p (est.company, est.est-no, job-mat.frm, output v-up).

          find first ef
              where ef.company   EQ est.company
                AND ef.est-no    EQ est.est-no
                and ef.form-no eq job-mat.frm
              no-lock no-error.

          IF AVAIL ef THEN DO:
            RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
            v-on = v-up * v-on.

            find first eb
                where eb.company    EQ ef.company
                  AND eb.est-no     EQ eb.est-no
                  and eb.form-no    EQ ef.form-no
                  and eb.blank-no   NE 0
                no-lock no-error.
          end.

          if est.est-type eq 3 then do:
            v-qty = 0.

            for each b-jh FIELDS(qty)
                where b-jh.company eq job-hdr.company
                  and b-jh.job     eq job-hdr.job
                  and b-jh.job-no  eq job-hdr.job-no
                  and b-jh.job-no2 eq job-hdr.job-no2
                no-lock:

              v-qty = v-qty + b-jh.qty.
            end.

            v-pct = job-hdr.qty / v-qty.
          end.

          else
          if est.est-type eq 4 or est.est-type eq 8 then
            v-pct = job-hdr.sq-in / 100.
        end.

        v-mat-qty = round(v-mat-qty * v-pct * v-on,0).

        leave.
      end.

      if not v-ship then
      for each fg-rcpth FIELDS(r-no rita-code)
          where fg-rcpth.company   eq cocode
            and fg-rcpth.i-no      eq job-hdr.i-no
            and fg-rcpth.job-no    eq job-hdr.job-no
            and fg-rcpth.job-no2   eq job-hdr.job-no2
            and fg-rcpth.rita-code eq "R"
          no-lock,

          each fg-rdtlh FIELDS(qty)
          where fg-rdtlh.r-no      eq fg-rcpth.r-no
            and fg-rdtlh.rita-code eq fg-rcpth.rita-code
          no-lock:

        v-fg-qty = v-fg-qty + fg-rdtlh.qty.  
      end.

      else
      if job-hdr.ord-no ne 0 then
      for each ar-inv
          where ar-inv.company eq cocode
            and ar-inv.ord-no  eq job-hdr.ord-no
            and ar-inv.posted  eq yes
          use-index ord-no no-lock,

          each ar-invl
          where ar-invl.x-no    eq ar-inv.x-no
            and ar-invl.i-no    eq job-hdr.i-no
            and ar-invl.job-no  eq job-hdr.job-no
            and ar-invl.job-no2 eq job-hdr.job-no2
          no-lock:

        v-qty = ar-invl.inv-qty.

        if tt-report.key-04 eq "SET" and avail eb then do:
          {ce/set-qty.i v-qty eb}
        end.

        v-fg-qty = v-fg-qty + v-qty.
      end.

      assign
       v-job   = fill(" ",6 - length(trim(job-hdr.job-no))) +
                 trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
       v-date  = date(int(substr(tt-report.key-02,5,2)),
                      int(substr(tt-report.key-02,7,2)),
                      int(substr(tt-report.key-02,1,4)))
       v-due-date  = date(int(substr(tt-report.key-05,5,2)),
                      int(substr(tt-report.key-05,7,2)),
                      int(substr(tt-report.key-05,1,4)))
       v-sheet = if avail job-mat then
                   "W: " + trim(string(job-mat.wid,">>9.99<<")) + " " +
                   "L: " + trim(string(job-mat.len,">>9.99<<"))
                 else ""
       v-pct   = 0.

      find first job-mch
          where job-mch.company   eq cocode
            and job-mch.job       eq job-hdr.job
            and job-mch.job-no    eq job-hdr.job-no
            and job-mch.job-no2   eq job-hdr.job-no2
            and job-mch.frm       eq job-hdr.frm
            and (job-mch.blank-no eq job-hdr.blank-no or
                 job-hdr.blank-no eq 0)
            and job-mch.dept      eq "GL"
          use-index job no-lock no-error.
      v-gl = if avail job-mch then "  X" else "".

      release oe-ord.
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
            and oe-ordl.job-no  eq job-hdr.job-no
            and oe-ordl.job-no2 eq job-hdr.job-no2
          no-lock no-error.
      if avail oe-ordl then do:
        find first oe-ord of oe-ordl no-lock no-error.
        v-pct = oe-ordl.under-pct / 100.
      end.

      release eb.
      if avail est then
      find first eb
          where eb.company    EQ est.company
            AND eb.est-no     EQ est.est-no
            and eb.form-no    EQ job-hdr.frm
            and (est.est-type EQ 1                                       or
                 est.est-type EQ 5                                       or
                 (eb.blank-no EQ job-hdr.blank-no and
                  (est.est-type EQ 3 OR
                   est.est-type EQ 4 OR
                   est.est-type EQ 8))                                   or
                 (eb.blank-no eq 0                and
                  (est.est-type eq 2 or est.est-type eq 6)))
          no-lock no-error.

      if (tb_no-show-underrun AND
         v-fg-qty lt job-hdr.qty - (job-hdr.qty * v-pct)) OR
         tb_no-show-underrun = NO then do:

       /* display v-date
                space(2)
                tt-report.key-03
                job-hdr.i-no
                eb.part-no          when avail eb
                                    @ job-hdr.i-no
                v-job
                job-hdr.qty
                job-mat.i-no        when avail job-mat
                v-sheet
                oe-ordl.vend-no     when avail oe-ordl
                v-mat-qty
                v-gl
                space(2)
                v-fg-qty

            with frame det STREAM-IO width 132 no-labels no-box down.*/



       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mach"    THEN cVarValue = string(v-mach,"x(8)") .
                         WHEN "st-date"   THEN cVarValue = IF v-date <> ? THEN string(v-date,"99/99/9999") ELSE "".
                         WHEN "due-date"   THEN cVarValue = IF v-due-date <> ? THEN string(v-due-date,"99/99/9999") ELSE "".
                         WHEN "cust"   THEN cVarValue = STRING(tt-report.key-03,"x(8)").
                         WHEN "cust-part"  THEN cVarValue = IF AVAIL eb THEN  STRING(eb.part-no,"x(15)") ELSE STRING(job-hdr.i-no,"x(15)") .
                         WHEN "job"   THEN cVarValue = STRING(v-job) .
                         WHEN "job-qty"  THEN cVarValue = STRING(job-hdr.qty,">>,>>>,>>9") .
                         WHEN "board"   THEN cVarValue = IF AVAIL job-mat THEN  STRING(job-mat.i-no,"x(10)") ELSE "" .
                         WHEN "sheet-size"  THEN cVarValue = STRING(v-sheet,"x(30)") .
                         WHEN "vend"    THEN cVarValue = IF AVAIL oe-ordl THEN  string(oe-ordl.vend-no,"x(8)") ELSE "" .
                         WHEN "board-rec"   THEN cVarValue = string(v-mat-qty,"->,>>>,>>>,>>9").
                         WHEN "gl-st"   THEN cVarValue = STRING(v-gl,"x(5)").
                         WHEN "fg-ship"  THEN cVarValue = STRING(v-fg-qty,"->>>,>>>,>>9") .


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
        DO:
           IF first-of(tt-report.key-01) THEN
              PUT STREAM excel UNFORMATTED
                  SKIP(1).

           PUT STREAM excel UNFORMATTED
              '"' IF first-of(tt-report.key-01) THEN
                     v-mach ELSE ""                      '",'
              '"' IF v-date NE ? THEN STRING(v-date)
                  ELSE ""                                '",'
              '"' tt-report.key-03                       '",'
              '"' IF AVAIL eb THEN eb.part-no
                  ELSE job-hdr.i-no                      '",'
              '"' v-job                                  '",'
              '"' STRING(job-hdr.qty,">>,>>>,>>9")       '",'
              '"' IF AVAIL job-mat THEN job-mat.i-no
                  ELSE ""                                '",'
              '"' v-sheet                                '",'
              '"' IF AVAIL oe-ordl THEN oe-ordl.vend-no
                  ELSE ""                                '",'
              '"' STRING(v-mat-qty,"->>>,>>>,>>9")       '",'
              '"' v-gl                                   '",'
              '"' STRING(v-fg-qty,"->>>,>>>,>>9")        '",'
              SKIP.
        END.*/

        v-first[1] = yes.

        if v-note then do:
          find est where est.company EQ job-hdr.company
                     AND est.est-no  EQ job-hdr.est-no
                   no-lock no-error.

          if avail est then
          do i = 1 to 2:
            for each est-inst
                where est-inst.company  EQ est.company
                  AND est-inst.est-no   EQ est.est-no
                  and (est-inst.line-no EQ job-hdr.frm or est-inst.line-no eq 0)
                  and ((est-inst.dept   EQ "NT" and i eq 1) or
                       (est-inst.dept   NE "NT" and i eq 2))
                no-lock,
                first dept where dept.code eq est-inst.dept no-lock
                break by dept.fc:

              v-first[2] = yes.  

              if v-first[1] then
              do j = 1 to 3:
                if est-inst.inst[j] ne "" then do:
                  put skip(1)
                      "    Instructions: ".
                  v-first[1] = no.    
                  leave.
                end.
              end.

              do j = 1 to 3:
                if est-inst.inst[j] ne "" then do:
                  if v-first[2] then do:
                    put est-inst.dept at 19.
                    v-first[2] = no.
                  end.

                  put est-inst.inst[j] at 22 skip.
                end.      
              end.  
            end.    
          end.

          if not v-first[1] then put skip(1).
        end.

        if v-remm then
        for first job
            where job.company eq cocode
              and job.job     eq job-hdr.job     
              and job.job-no  eq job-hdr.job-no
              and job.job-no2 eq job-hdr.job-no2
            no-lock,

            each job-mch
            where job-mch.company   eq cocode
              and job-mch.job       eq job.job
              and job-mch.job-no    eq job.job-no
              and job-mch.job-no2   eq job.job-no2
              and job-hdr.frm       eq job-mch.frm
              and (job-mch.blank-no eq job-hdr.blank-no or
                   job-mch.blank-no eq 0)
            use-index job no-lock,

            first dept where dept.code eq job-mch.dept no-lock  

            break by dept.fc
                  by job-mch.m-code
                  by job-mch.pass:

           assign
            v-factor    = if job-mch.blank-no gt 0 then 1
                          else (job-hdr.sq-in / 100)
            v-rem-hr[1] = v-rem-hr[1] + (job-mch.mr-hr  * v-factor)
            v-rem-hr[2] = v-rem-hr[2] + (job-mch.run-hr * v-factor).

           for each mch-act
               where mch-act.company   eq cocode
                 and mch-act.m-code    eq job-mch.m-code
                 and mch-act.job-no    eq job-mch.job-no
                 and mch-act.job-no2   eq job-mch.job-no2
                 and mch-act.frm       eq job-mch.frm
                 and (mch-act.blank-no eq job-mch.blank-no or
                      job-mch.blank-no eq 0)
                 and mch-act.pass      eq job-mch.pass
               use-index operation no-lock,

               first job-code
               where job-code.code eq mch-act.code
                 and (job-code.cat eq "MR" or job-code.cat eq "RUN")
               no-lock

               break by job-code.cat
                     by mch-act.complete:

             i = lookup(job-code.cat,"MR,RUN").

             v-rem-hr[i] = v-rem-hr[i] - (mch-act.hours * v-factor).

             if last-of(mch-act.complete) and
                mch-act.complete          then v-rem-hr[i] = 0.
           end.

           if v-rem-hr[1] lt 0 then v-rem-hr[1] = 0.
           if v-rem-hr[2] lt 0 then v-rem-hr[2] = 0.

           if last-of(job-mch.m-code) then do:

              if v-rem-hr[1] gt 0 or v-rem-hr[2] gt 0 then
                 v-m-list = v-m-list + trim(job-mch.m-code) + ",".

              if job-mch.m-code eq v-mach then
                 assign
                    v-rem-hr[3] = v-rem-hr[1]
                    v-rem-hr[4] = v-rem-hr[2].

              assign
                 v-rem-hr[1] = 0
                 v-rem-hr[2] = 0.
           end.

           if last(dept.fc) then do:
              if v-m-list ne "" then do:
                 if substr(v-m-list,length(trim(v-m-list)),1) eq "," then
                    substr(v-m-list,length(trim(v-m-list)),1) = "".

                 v-m-list = "    Incomplete Machines: " + v-m-list.

                 if v-first[1] then put skip(1).

                 put v-m-list
                     "Remaining Hours for "
                     v-mach
                     "  Run: "
                     v-rem-hr[4]
                     "  MR: "
                     v-rem-hr[3]
                     "  Tot: "
                     v-rem-hr[3] + v-rem-hr[4]
                     skip(1).
              end.

              assign
               v-rem-hr[5] = v-rem-hr[5] + v-rem-hr[3]
               v-rem-hr[6] = v-rem-hr[6] + v-rem-hr[4]

               v-rem-hr[3] = 0
               v-rem-hr[4] = 0.
           end.      
        end.
      end.

      v-m-list = "".

      if last-of(tt-report.key-02) then do:
        if v-rem-hr[5] gt 0 or v-rem-hr[6] gt 0 then
          put skip(1)
              v-m-list
              "Remaining Hours for "
              v-date
              "  Run: "
              v-rem-hr[6]
              "  MR: "
              v-rem-hr[5]
              "  Tot: "
              v-rem-hr[5] + v-rem-hr[6]
              skip(2).

        assign
         v-rem-hr[5] = 0
         v-rem-hr[6] = 0.
      end.

      assign
       v-mat-qty = 0
       v-fg-qty  = 0.
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
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

