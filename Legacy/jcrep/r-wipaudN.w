
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-wipaud.w

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
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

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

 DEFINE TEMP-TABLE work-aud NO-UNDO
   FIELD tran-date AS DATE
   FIELD procat LIKE item.procat
   FIELD job-no LIKE job-mat.job-no
   FIELD job-no2 LIKE job-mat.job-no2
   FIELD frm LIKE misc-act.frm
   FIELD blank-no LIKE misc-act.blank-no
   FIELD i-no LIKE mat-act.i-no
   FIELD m-code LIKE mach.m-code
   FIELD dscr LIKE item.i-dscr
   FIELD qty AS DECIMAL FORMAT ">>>>>>9.99-"
   FIELD waste LIKE mch-act.waste
   FIELD code LIKE mch-act.code
   FIELD hours LIKE mch-act.hours
   FIELD complete LIKE mch-act.complete
   FIELD tran-time LIKE mch-act.op-time.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy AS LOG NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cFieldLength AS cha NO-UNDO.
DEFINE VARIABLE cFieldType AS cha NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha NO-UNDO.
  

ASSIGN cTextListToSelect = "Trans Type,Trans Date,Job No.,S,B,Item Number,"
                                            + "Description,Qty Posted,Wst Qty,Mch Hrs,"
                                            + "Mach Code,Job Code,C,Trans Time"
       cFieldListToSelect = "trns-typ,trns-dt,job-no,frm,blnk,i-no," +
                                        "dscr,qty-pstd,wst-qty,mch-hrs," +
                                        "mch-cd,job-cd,vc,trns-tym"
       cFieldLength = "10,10,10,1,1,15," + "30,11,7,7," + "11,11,3,10" 
       cFieldType = "c,c,c,c,c,c," + "c,i,i,i," + "c,c,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  =  "Trans Type,Trans Date,Job No.,S,B,Item Number,"
                                            + "Description,Qty Posted,Wst Qty,Mch Hrs,"
                                            + "Mach Code,Job Code,C"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 tb_wip rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 tb_wip rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected

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
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

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
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipaud.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

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
"To Email", 5
     SIZE 23 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_wip AS LOGICAL INITIAL NO 
     LABEL "Show Only Open WIP?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.43 COL 28 COLON-ALIGNED NO-LABELS
     rd_jstat AT ROW 2.43 COL 43 NO-LABELS
     begin_job-no AT ROW 3.81 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 3.81 COL 37 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 3.81 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 3.81 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     tb_wip AT ROW 5.48 COL 62 RIGHT-ALIGNED
     sl_avail AT ROW 7.67 COL 3.6 NO-LABELS WIDGET-ID 26
     Btn_Def AT ROW 7.67 COL 39.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 7.67 COL 59 NO-LABELS WIDGET-ID 28
     Btn_Add AT ROW 8.67 COL 39.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 9.67 COL 39.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 10.71 COL 39.6 WIDGET-ID 40
     btn_down AT ROW 11.71 COL 39.6 WIDGET-ID 42
     rd-dest AT ROW 14.52 COL 5 NO-LABELS
     lv-ornt AT ROW 14.76 COL 32 NO-LABELS
     lines-per-page AT ROW 14.76 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 16.19 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.14 COL 29 COLON-ALIGNED NO-LABELS
     td-show-parm AT ROW 18.33 COL 31
     tb_excel AT ROW 19.29 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.29 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 20.1 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.67 COL 18
     btn-cancel AT ROW 21.67 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 6.95 COL 4.4 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.57 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 6.95 COL 59 WIDGET-ID 44
     RECT-6 AT ROW 13.1 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.19.


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
         TITLE              = "W.I.P. Job Audit Trail"
         HEIGHT             = 22.71
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
                                                                        */
ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_wip IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_wip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* W.I.P. Job Audit Trail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* W.I.P. Job Audit Trail */
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
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
       
  RUN GetSelectionList.
  RUN run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 5 THEN
       DO:
          DEFINE VARIABLE lv-tmp AS CHARACTER INIT "-0" NO-UNDO.
          
          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
       END.

  END CASE. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
  DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
  DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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

&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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
    DEFINE VARIABLE char-val AS cha NO-UNDO.

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
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

&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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


&Scoped-define SELF-NAME tb_wip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_wip C-Win
ON VALUE-CHANGED OF tb_wip IN FRAME FRAME-A /* Show Only Open WIP? */
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

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2 .
    APPLY "entry" TO begin_job-no.
  END.

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
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  
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

  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
  DISPLAY lbl_jstat rd_jstat begin_job-no begin_job-no2 end_job-no end_job-no2 
          tb_wip rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no 
         end_job-no2 tb_wip rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-job-totals-proc C-Win 
PROCEDURE excel-job-totals-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-text      AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-brd-job AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-mch-job AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-wst-job AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-hrs-job AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-fg-job  AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-oth-job AS DECIMAL NO-UNDO.
   
   PUT STREAM excel UNFORMATTED
       SKIP(1).
  
   RUN excel-spaces-proc(INPUT 5).
  
   PUT STREAM excel UNFORMATTED
       '"' ip-text                           '",'.
  
   PUT STREAM excel UNFORMATTED
       '"' "BOARD TOTALS - "                 '",'
       '"' STRING(ip-v-brd-job,">>>>>>>>9-") '",'
       SKIP.
  
   RUN excel-spaces-proc(INPUT 6).
  
   PUT STREAM excel UNFORMATTED
       '"' "MACHINE TOTALS - "               '",'
       '"' STRING(ip-v-mch-job,">>>>>>>>9-") '",'
       '"' STRING(ip-v-wst-job,">>>>>>9-")   '",'
       '"' STRING(ip-v-hrs-job,">>>>9.99-")  '",'
       SKIP.
  
   RUN excel-spaces-proc(INPUT 6).
  
   PUT STREAM excel UNFORMATTED
       '"' "FINISHED GOODS TOTALS:"          '",'
       '"' STRING(ip-v-fg-job,">>>>>>>>9-")  '",'
       SKIP.
  
   RUN excel-spaces-proc(INPUT 6).
  
   PUT STREAM excel UNFORMATTED
       '"' "OTHER MATERIAL TOTALS:"          '",'
       '"' STRING(ip-v-oth-job,">>>>>>>>9-") '",'
       SKIP(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-spaces-proc C-Win 
PROCEDURE excel-spaces-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-spaces AS INTEGER NO-UNDO.
  
   DEFINE VARIABLE viLoop AS INTEGER NO-UNDO.

   DO viLoop = 1 TO ip-spaces:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.
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
 DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ jc/rep/wip-aud.p  8/94 gb */
/* WIP Job Audit Listing Report                                               */
/* ---------------------------------------------------------------------------*/

/*{sys/form/r-topw.f}*/

DEFINE VARIABLE v-job-no LIKE job.job-no EXTENT 2 INIT ["","zzzzzz"] NO-UNDO.
DEFINE VARIABLE v-job-no2 LIKE job.job-no2 EXTENT 2 INIT [00,99] NO-UNDO.
DEFINE VARIABLE v-stat AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-only-opn AS LOGICAL FORMAT "Y/N" NO-UNDO.
DEFINE VARIABLE v-brd-job AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-brd-tot AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-oth-job AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-oth-tot AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-mch-job AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-mch-tot AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-fg-job AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-fg-tot AS INTEGER FORMAT ">>>>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-hrs-job AS DECIMAL FORMAT ">>>>9.99-" NO-UNDO.
DEFINE VARIABLE v-hrs-tot AS DECIMAL FORMAT ">>>>9.99-" NO-UNDO.
DEFINE VARIABLE v-wst-job AS INTEGER FORMAT ">>>>>>9-" NO-UNDO.
DEFINE VARIABLE v-wst-tot AS INTEGER FORMAT ">>>>>>9-" NO-UNDO.


DEFINE VARIABLE hdr-tit AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdr-tit2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdr-tit3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.
DEFINE VARIABLE str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line AS cha FORM "x(300)" NO-UNDO.

DEFINE VARIABLE cCustomerName AS cha FORM "x(25)" NO-UNDO.
DEFINE VARIABLE cPrepDscr AS cha FORM "x(25)" NO-UNDO.
{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat        = SUBSTR(rd_jstat,1,1)
  v-job-no[1]   = FILL(" ",6 - length(TRIM(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-job-no[2]   = FILL(" ",6 - length(TRIM(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99") 
  v-only-opn    = tb_wip
     
   .


DEFINE VARIABLE cslist AS cha NO-UNDO.
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

        IF LOOKUP(ttRptSelected.TextList, "Qty Posted,Wst Qty,Mch Hrs") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "Trans Type,Trans Date,Job Number,S,B,Item Number,"
              + "Description,Quantity Posted,Waste Qty,Mach Hours,"
              + "Mach Code,Job Code,C". */
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.


    FOR EACH job
          WHERE job.company EQ cocode
            AND job.job-no  GE SUBSTR(v-job-no[1],1,6)
            AND job.job-no  LE SUBSTR(v-job-no[2],1,6)
            AND fill(" ",6 - length(TRIM(job.job-no))) +
                trim(job.job-no) + string(int(job.job-no2),"99") GE v-job-no[1] 
            AND fill(" ",6 - length(TRIM(job.job-no))) +
                trim(job.job-no) + string(int(job.job-no2),"99") LE v-job-no[2]
            AND (v-stat  EQ "A"  OR
                 (v-stat EQ "O" AND job.opened) OR
                 (v-stat EQ "C" AND NOT job.opened))
          NO-LOCK:

         FOR EACH work-aud:
            DELETE work-aud.
         END.

         FOR EACH mch-act
             WHERE mch-act.company EQ cocode
               AND mch-act.job     EQ job.job
             USE-INDEX job NO-LOCK:
            IF v-only-opn AND NOT mch-act.opn THEN NEXT.

            FIND mach
                WHERE mach.company EQ cocode
                  AND mach.loc     EQ locode
                  AND mach.m-code  EQ mch-act.m-code
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mach THEN NEXT.

            CREATE work-aud.
            ASSIGN work-aud.job-no = mch-act.job-no
                   work-aud.tran-date = mch-act.op-date
                   work-aud.procat = "HRS"
                   work-aud.job-no2 = mch-act.job-no2
                   work-aud.frm = mch-act.frm
                   work-aud.blank-no = mch-act.blank-no
                   work-aud.qty = mch-act.qty
                   work-aud.waste = mch-act.waste
                   work-aud.hours = mch-act.hours
                   work-aud.m-code = mch-act.m-code
                   work-aud.dscr = mach.m-dscr
                   work-aud.code = mch-act.code
                   work-aud.complete = mch-act.complete
                   work-aud.tran-time = mch-act.op-time.
            

         END.

         FOR EACH mat-act
             WHERE mat-act.company EQ cocode
               AND mat-act.job     EQ job.job
             USE-INDEX job NO-LOCK:
            IF v-only-opn AND NOT mat-act.opn THEN NEXT. 

            FIND item
                WHERE item.company EQ cocode
                  AND item.i-no    EQ mat-act.i-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE item THEN NEXT.

            CREATE work-aud.
            ASSIGN work-aud.job-no = mat-act.job-no
                   work-aud.job-no2 = mat-act.job-no2
                   work-aud.procat = item.procat
                   work-aud.tran-date = mat-act.mat-date
                   work-aud.frm = mat-act.s-num
                   work-aud.blank-no = mat-act.b-num
                   work-aud.i-no = mat-act.i-no
                   work-aud.dscr = item.i-dscr
                   work-aud.qty = mat-act.qty
                   work-aud.tran-time = mat-act.mat-time.
         END.

         FOR EACH fg-act
             WHERE fg-act.company EQ cocode
               AND fg-act.job     EQ job.job
             USE-INDEX job-idx NO-LOCK:
            IF v-only-opn AND NOT fg-act.opn THEN NEXT.
     
            FIND itemfg
                WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ fg-act.i-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg THEN NEXT.
     
            CREATE work-aud.
            ASSIGN work-aud.job-no = fg-act.job-no
                   work-aud.job-no2 = fg-act.job-no2
                   work-aud.procat = "F.G."
                   work-aud.tran-date = fg-act.fg-date
                   work-aud.frm = fg-act.s-num
                   work-aud.blank-no = fg-act.b-num
                   work-aud.i-no = fg-act.i-no
                   work-aud.dscr = itemfg.i-name
                   work-aud.qty = fg-act.qty
                   work-aud.tran-time = fg-act.fg-time.
         END.

         FOR EACH misc-act
             WHERE misc-act.company EQ cocode
               AND misc-act.job     EQ job.job
             USE-INDEX misc-idx NO-LOCK:
            IF v-only-opn AND NOT misc-act.opn THEN NEXT.

            CREATE work-aud.
            ASSIGN work-aud.job-no = misc-act.job-no
                   work-aud.job-no2 = misc-act.job-no2
                   work-aud.frm = misc-act.frm
                   work-aud.blank-no = misc-act.blank-no
                   work-aud.tran-date = misc-act.misc-date
                   work-aud.dscr = misc-act.dscr
                   work-aud.tran-time = misc-act.misc-time.
            IF misc-act.ml THEN
               ASSIGN work-aud.qty = misc-act.cost
                      work-aud.procat = "MSC-M".
            ELSE
               ASSIGN work-aud.qty = misc-act.cost
                      work-aud.procat = "MSC-H"
                      work-aud.m-code = misc-act.m-code.
         END.

         ASSIGN v-brd-job = 0
                v-mch-job = 0
                v-fg-job  = 0
                v-oth-job = 0
                v-wst-job = 0
                v-hrs-job = 0.

         FOR EACH work-aud BREAK BY tran-date:

            IF work-aud.procat = "HRS" OR work-aud.procat = "MSC-H" THEN DO:
               
                ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING(work-aud.procat) .
                                    WHEN "trns-dt"      THEN cVarValue =  STRING(work-aud.tran-date) .
                                    WHEN "job-no"           THEN cVarValue =  STRING(work-aud.job-no + "-" + STRING(work-aud.job-no2,"99")) .
                                    WHEN "frm"              THEN cVarValue =  STRING(work-aud.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(work-aud.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  STRING(work-aud.dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(work-aud.qty,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  STRING(work-aud.waste,">>>>9-").
                                    WHEN "mch-hrs"          THEN cVarValue =  STRING(work-aud.hours,">>9.99-").
                                    WHEN "mch-cd"           THEN cVarValue =  work-aud.m-code   .
                                    WHEN "job-cd"           THEN cVarValue =  work-aud.code        .
                                    WHEN "vc"               THEN cVarValue =  STRING(work-aud.complete)    .
                                    WHEN "trns-tym"         THEN cVarValue =  STRING(work-aud.tran-time,"hh:mmam")    .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.
                       
                       PUT UNFORMATTED cDisplay SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  cExcelDisplay SKIP.
                        END.


               ASSIGN v-mch-job = v-mch-job + work-aud.qty
                      v-wst-job = v-wst-job + work-aud.waste
                      v-hrs-job = v-hrs-job + work-aud.hours
                      v-mch-tot = v-mch-tot + work-aud.qty
                      v-wst-tot = v-wst-tot + work-aud.waste
                      v-hrs-tot = v-hrs-tot + work-aud.hours.
            END.
            ELSE
            IF work-aud.procat = "F.G." THEN DO:
               

                ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING(work-aud.procat) .
                                    WHEN "trns-dt"      THEN cVarValue =  STRING(work-aud.tran-date) .
                                    WHEN "job-no"           THEN cVarValue =  STRING(work-aud.job-no + "-" + STRING(work-aud.job-no2,"99")) .
                                    WHEN "frm"              THEN cVarValue =  STRING(work-aud.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(work-aud.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  work-aud.i-no .
                                    WHEN "dscr"             THEN cVarValue =  STRING(work-aud.dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(work-aud.qty,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "" .
                                    WHEN "mch-cd"           THEN cVarValue =  ""  .
                                    WHEN "job-cd"           THEN cVarValue =  ""       .
                                    WHEN "vc"               THEN cVarValue =  ""    .
                                    WHEN "trns-tym"         THEN cVarValue =  STRING(work-aud.tran-time,"hh:mmam")    .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.
                       
                       PUT UNFORMATTED cDisplay SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  cExcelDisplay SKIP.
                        END.

               ASSIGN v-fg-job = v-fg-job + work-aud.qty
                      v-fg-tot = v-fg-tot + work-aud.qty.
            END.
            ELSE DO:
              

                ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING(work-aud.procat) .
                                    WHEN "trns-dt"      THEN cVarValue =  STRING(work-aud.tran-date) .
                                    WHEN "job-no"           THEN cVarValue =  STRING(work-aud.job-no + "-" + STRING(work-aud.job-no2,"99")) .
                                    WHEN "frm"              THEN cVarValue =  STRING(work-aud.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(work-aud.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  work-aud.i-no .
                                    WHEN "dscr"             THEN cVarValue =  STRING(work-aud.dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(work-aud.qty,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "" .
                                    WHEN "mch-cd"           THEN cVarValue =  ""  .
                                    WHEN "job-cd"           THEN cVarValue =  ""       .
                                    WHEN "vc"               THEN cVarValue =  ""    .
                                    WHEN "trns-tym"         THEN cVarValue =  STRING(work-aud.tran-time,"hh:mmam")    .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.
                       
                       PUT UNFORMATTED cDisplay SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  cExcelDisplay SKIP.
                        END.

               IF work-aud.procat NE "MSC-M" THEN DO:
                  FIND item
                      WHERE item.company EQ cocode
                        AND item.i-no    EQ work-aud.i-no
                      NO-LOCK.

                  IF item.mat-type = "B" THEN
                     ASSIGN v-brd-job = v-brd-job + work-aud.qty
                            v-brd-tot = v-brd-tot + work-aud.qty.
                  ELSE
                     ASSIGN v-oth-job = v-oth-job + work-aud.qty
                            v-oth-tot = v-oth-tot + work-aud.qty.
               END.
            END.
            IF LAST-OF(work-aud.tran-date) THEN
            DO:
              /* put skip(1) "JOB TOTALS - " at 20 job.job-no
                   space(0) "-" space(0) job.job-no2 format "99"
                   "         BOARD TOTALS: " at 56 v-brd-job skip
                   "       MACHINE TOTALS: " at 56 v-mch-job " " v-wst-job " "
                   v-hrs-job skip
                   "FINISHED GOODS TOTALS: " at 56 v-fg-job skip
                   "OTHER MATERIAL TOTALS: " at 56 v-oth-job skip(2).

               IF tb_excel THEN
                  RUN excel-job-totals-proc(INPUT "JOB TOTALS - " + job.job-no +
                                            "-" + STRING(job.job-no2,"99"),
                                            INPUT v-brd-job, INPUT v-mch-job,
                                            INPUT v-wst-job, INPUT v-hrs-job,
                                            INPUT v-fg-job, INPUT v-oth-job). */

               PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-brd-job,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "".
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "JOB TOTALS - " + job.job-no + "-" + string(job.job-no2,"99") + "         BOARD TOTALS: "
                           SUBSTRING(cDisplay,45,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " JOB TOTALS    " job.job-no + "-" + string(job.job-no2,"99") + "         BOARD TOTALS: " SUBSTRING(cExcelDisplay,3,300) SKIP.
                       END. 

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-mch-job,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  STRING(v-wst-job,">>>>9-").
                                    WHEN "mch-hrs"          THEN cVarValue =  STRING(v-hrs-job,">>9.99-").
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "MACHINE TOTALS : " SUBSTRING(cDisplay,17,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " MACHINE TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-fg-job,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue = "" .
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "FINISHED GOODS TOTALS : " SUBSTRING(cDisplay,24,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " FINISHED GOODS TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-oth-job,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "" .
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "OTHER MATERIAL TOTALS : " SUBSTRING(cDisplay,24,300) SKIP(2).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.
            END.
         END.
      END.
     
      PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-brd-tot,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "".
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "REPORT TOTALS  "  "             BOARD TOTALS: "
                           SUBSTRING(cDisplay,43,300) SKIP(1).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " REPORT TOTALS    "   "         BOARD TOTALS: " SUBSTRING(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-mch-tot,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  STRING(v-wst-tot,">>>>9-").
                                    WHEN "mch-hrs"          THEN cVarValue =  STRING(v-hrs-tot,">>9.99-").
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "MACHINE TOTALS : " SUBSTRING(cDisplay,17,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " MACHINE TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-fg-tot,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue = "" .
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "FINISHED GOODS TOTALS : " SUBSTRING(cDisplay,24,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " FINISHED GOODS TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  "" .
                                    WHEN "trns-dt"      THEN cVarValue =  "" .
                                    WHEN "job-no"           THEN cVarValue =  "" .
                                    WHEN "frm"              THEN cVarValue =  "" .
                                    WHEN "blnk"             THEN cVarValue =  "" .
                                    WHEN "i-no"             THEN cVarValue =  "" .
                                    WHEN "dscr"             THEN cVarValue =  "" .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(v-oth-tot,">>>>>>9.99-")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .
                                    WHEN "mch-hrs"          THEN cVarValue =  "" .
                                    WHEN "mch-cd"           THEN cVarValue =  "" .
                                    WHEN "job-cd"           THEN cVarValue =  "" .
                                    WHEN "vc"               THEN cVarValue =  "" .
                                    WHEN "trns-tym"         THEN cVarValue =  "" .
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "OTHER MATERIAL TOTALS : " SUBSTRING(cDisplay,24,300) SKIP(1).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.
  
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
