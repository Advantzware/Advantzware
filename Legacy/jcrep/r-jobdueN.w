&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-jobdue.w

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
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "CUSTOMER,JOB#,S,B,DIE#,Plate#,DUE DATE,COMPLETION DATE,STYLE," +
                           "QTY GLUING,SHEETED,PRINTED,DIE CUT,GLUED,GLUE HRS,ORD MFG DATE,RELEASE DATE," +
                           "ORDER #,ORDER DATE,FG ITEM#,PO#,PRIMARY CONTACT,CSR,SALES REP,SALES REP NAME,CONTACT NAME," +            
                           "FG CATEGORY,SHIP TO,WAREHOUSE"
       cFieldListToSelect = "cust,job,frm,blnk,die,palt,due-dt,comp-dt,styl," +
                            "qty-glu,sht,prntd,die-cut,glue,glu-hrs,mfg-date,rel-date," +
                            "order-no,ord-date,fg-item,po-no,pri-contact,csr,sales-rep,rep-name,contact-name," +
                            "fg-cat,shipto,loc"
       cFieldLength = "8,10,1,1,20,15,10,15,7," + "13,7,7,7,5,14,11,12," + "7,10,15,15,25,8,9,20,30," + "11,8,9" 
       cFieldType = "c,c,c,c,c,c,c,c,c," + "i,i,i,i,i,i,c,c," + "c,c,c,c,c,c,c,c,c," + "c,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  =  "CUSTOMER,JOB#,S,B,DIE#,Plate#,DUE DATE,COMPLETION DATE,STYLE," +
                                               "QTY GLUING,SHEETED,PRINTED,DIE CUT,GLUED,GLUE HRS"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 thru_date fi_st-cust ~
fi_end-cust begin_job-no begin_job-no2 end_job-no end_job-no2 tb_fold ~
tb_corr sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest lv-ornt lv-font-no lines-per-page td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS thru_date fi_st-cust fi_end-cust ~
begin_job-no begin_job-no2 end_job-no end_job-no2 tb_fold tb_corr sl_avail ~
sl_selected rd-dest lv-ornt lv-font-no lines-per-page lv-font-name ~
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

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_end-cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-bckmch.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE fi_st-cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no 
     LABEL "Folding" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

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
     fi_st-cust AT ROW 3.14 COL 26 COLON-ALIGNED WIDGET-ID 4
     fi_end-cust AT ROW 3.14 COL 66 COLON-ALIGNED WIDGET-ID 6
     begin_job-no AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.1 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.1 COL 66 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.1 COL 78 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     tb_fold AT ROW 6 COL 42 WIDGET-ID 10
     tb_corr AT ROW 6.95 COL 42 WIDGET-ID 12
     sl_avail AT ROW 9.52 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.52 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 9.52 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.52 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.52 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.57 COL 40 WIDGET-ID 40
     btn_down AT ROW 13.57 COL 40 WIDGET-ID 42
     rd-dest AT ROW 16.33 COL 6 NO-LABEL
     lv-ornt AT ROW 16.33 COL 31 NO-LABEL
     lv-font-no AT ROW 17.76 COL 34 COLON-ALIGNED
     lines-per-page AT ROW 17.76 COL 70 COLON-ALIGNED
     lv-font-name AT ROW 18.71 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.91 COL 30
     tb_excel AT ROW 20.86 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.86 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 21.67 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.71 COL 18
     btn-cancel AT ROW 23.71 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.81 COL 4.8 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.38 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Industry?" VIEW-AS TEXT
          SIZE 10 BY 1.19 AT ROW 6.24 COL 31 WIDGET-ID 8
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.81 COL 59.4 WIDGET-ID 44
     RECT-6 AT ROW 15.14 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95 BY 24.91.


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
         TITLE              = "Jobs Due Tracking"
         HEIGHT             = 25.14
         WIDTH              = 95.6
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
       fi_end-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_st-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fold:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Jobs Due Tracking */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Jobs Due Tracking */
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

  RUN GetSelectionList.
  run run-report.
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

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


&Scoped-define SELF-NAME fi_end-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_end-cust C-Win
ON LEAVE OF fi_end-cust IN FRAME FRAME-A /* Ending Customer */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME fi_st-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_st-cust C-Win
ON LEAVE OF fi_st-cust IN FRAME FRAME-A /* Beginning Customer */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding */
DO:
  ASSIGN {&self-name}.
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

  THRU_date = TODAY.   

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CEMENU"
      no-lock no-error.
  IF AVAIL sys-ctrl THEN
    ASSIGN
     tb_fold = INDEX(" FB",SUBSTR(sys-ctrl.char-fld,1,1)) GT 0
     tb_corr = INDEX(" CB",SUBSTR(sys-ctrl.char-fld,1,1)) GT 0.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2 .
    APPLY "entry" TO thru_date.
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
  DISPLAY thru_date fi_st-cust fi_end-cust begin_job-no begin_job-no2 end_job-no 
          end_job-no2 tb_fold tb_corr sl_avail sl_selected rd-dest lv-ornt 
          lv-font-no lines-per-page lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 thru_date fi_st-cust fi_end-cust begin_job-no 
         begin_job-no2 end_job-no end_job-no2 tb_fold tb_corr sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lv-font-no lines-per-page td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
DEF BUFFER b-job-mch FOR job-mch.
DEF BUFFER b-est FOR est.

def var v-fdate as   date format "99/99/9999" init TODAY NO-UNDO.
def var v-fjob  like job.job-no NO-UNDO.
def var v-tjob  like v-fjob                   init "zzzzzz" NO-UNDO.
def var v-fjob2 like job.job-no2 NO-UNDO.
def var v-tjob2 like v-fjob2                  init 99 NO-UNDO.
def var v-indus as   char format "x" NO-UNDO.
DEF VAR v-fcust AS   CHAR NO-UNDO.
DEF VAR v-tcust AS   CHAR NO-UNDO.
DEF VAR v-run-end-date AS DATE NO-UNDO.

def var v-up     like eb.num-up NO-UNDO.
def var v-on     like v-up NO-UNDO.

def var v-hdr     as   char format "x(150)" extent 3 NO-UNDO.
def var v-mach    like job-mch.m-code format "x(8)" NO-UNDO.
def var v-date    as   date format "99/99/99" NO-UNDO.
def var v-job     as   char format "x(9)" NO-UNDO.
def var v-sheet   as   char format "x(19)" NO-UNDO.
def var v-gl      as   char format "x(5)" NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v-pct     as   DEC NO-UNDO.
def var v-mat-qty as   dec format "->>>,>>>,>>9" NO-UNDO.
def var v-fg-qty  like v-mat-qty NO-UNDO.
def var v-rem-hr  like job-mch.run-hr extent 6 NO-UNDO.
def var v-m-list  as   char format "x(61)" NO-UNDO.
def var v-factor  as   DEC NO-UNDO.
def var v-first   as   log extent 2 NO-UNDO.
DEF VAR v-style AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-plate  AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-die    AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-logqty    AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-comdate AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-glue   AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-logsheet  AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-glhr   AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.
DEF VAR v-print  AS LOG FORMAT "Yes/No" INIT NO NO-UNDO.

def var v-rs      as   char format "x(7)" NO-UNDO.
def var v-dc      as   char format "x(7)" NO-UNDO.
def var v-pr      as   char format "x(7)" NO-UNDO.
DEF VAR v-die-no  LIKE eb.die-no NO-UNDO.
DEF VAR v-rel-date AS DATE NO-UNDO .
DEFINE VARIABLE cShipTo AS CHARACTER NO-UNDO .
DEF VAR lv-stat AS CHAR NO-UNDO.
def var v-stat  as   char format "!" init "A".

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

DEF VAR cCustomerName AS cha FORM "x(25)" NO-UNDO.
DEF VAR cPrepDscr AS cha FORM "x(25)" NO-UNDO.
DEFINE VARIABLE cSalesRep AS CHARACTER NO-UNDO .
{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.
DEFINE VARIABLE cPrim-Contact AS CHARACTER NO-UNDO .
{custom/statusMsg.i " 'Processing...   '"}

/*form header v-hdr skip with frame r-top WIDTH 230.*/

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 113}

  v-fdate   = thru_date
  v-fcust   = fi_st-cust
  v-tcust   = fi_end-cust

  v-fjob    = fill(" ",6 - length(trim(begin_job-no))) +
               trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob    = fill(" ",6 - length(trim(end_job-no)))   +
               trim(end_job-no)   + string(int(end_job-no2),"99") 

   .

  ASSIGN
  /*v-hdr[3]  = fill("-",150)*/
  v-indus   = IF tb_fold THEN
                 IF tb_corr THEN "B" ELSE "F"
              ELSE IF tb_corr THEN "C" 
                 ELSE "" .

        /*  ASSIGN
              v-style   = tb_style 
              v-plate   = tb_plate 
              v-die     = tb_die 
              v-logqty  = tb_qty
              v-comdate = tb_comdate 
              v-glue    = tb_glue 
              v-logsheet  = tb_sheet 
              v-glhr    = tb_glhr
              v-print   = tb_print. */

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
          PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
END. 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}


if td-show-parm then run show-param.


SESSION:SET-WAIT-STATE ("general").

    for each tt-report:
      delete tt-report.
    end.

    IF TRIM(SUBSTRING(v-fjob, 1, 6)) GT "" THEN DO:    

      for each job
          where job.company eq cocode
      {jc/rep/jc-back3.i}
    END.
    ELSE DO:

      FOR EACH job-hdr NO-LOCK 
          WHERE job-hdr.company EQ cocode
        {jc/rep/jc-back4.i}

    END.

    for each tt-report where tt-report.term-id eq "",
        first job-mch where recid(job-mch) eq tt-report.rec-id NO-LOCK,
         EACH job-hdr NO-LOCK 
        where job-hdr.company   eq cocode
          and job-hdr.job       eq job-mch.job
          and job-hdr.job-no    eq job-mch.job-no
          and job-hdr.job-no2   eq job-mch.job-no2
          and job-hdr.frm       eq job-mch.frm
          and (job-hdr.blank-no eq job-mch.blank-no or
              job-mch.blank-no eq 0)       
        break by tt-report.key-01
              by tt-report.key-02
              BY job-hdr.job-no
              BY job-hdr.job-no2
              BY job-hdr.frm
              BY job-hdr.blank-no

        transaction:

        {custom/statusMsg.i " 'Processing Job#  '  + job-hdr.job-no "}

     IF first(tt-report.key-01) THEN display "" with frame r-top.

      IF NOT first(tt-report.key-01) AND FIRST-OF(tt-report.key-01) THEN DO:
         PUT "" SKIP.
         IF tb_excel THEN 
         EXPORT STREAM excel "".

      END.

      IF FIRST-OF(job-hdr.blank-no) THEN DO:
          ASSIGN v-gl = " "
                 v-rs = " "
                 v-pr = " "
                 v-dc = " "
                 v-die-no = "".
      END.

      for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job-hdr.job
            and (job-mat.frm    eq job-hdr.frm or
                 tt-report.key-03  eq "SET")
          use-index seq-idx no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq job-mat.i-no
            and item.mat-type eq "B"
          no-lock:

           {custom/statusMsg.i " 'Processing Job#  '  + job-mat.job-no "}

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

            /*v-mat-qty = v-mat-qty + v-qty.*/
          end.

          assign
           v-pct = 1
           v-up  = 1
           v-on  = 1.

          find b-est where b-est.company EQ job-hdr.company
                     AND b-est.est-no  EQ job-hdr.est-no
                   no-lock no-error.

          if avail b-est then do:
            run sys/inc/numup.p (b-est.company, b-est.est-no, job-mat.frm, output v-up).

            find first ef
                where ef.company   EQ b-est.company
                  AND ef.est-no    EQ b-est.est-no
                  and ef.form-no eq job-mat.frm
                no-lock no-error.

            IF AVAIL ef THEN DO:
              RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
              /*v-on = v-up * v-on.*/

              find first eb
                  where eb.company    EQ ef.company
                    AND eb.est-no     EQ eb.est-no
                    and eb.form-no    EQ ef.form-no
                    and eb.blank-no   NE 0
                  no-lock no-error.
            end.

            if b-est.est-type eq 3 then do:
              v-qty = 0.

              for each b-jh FIELDS(qty)
                  where b-jh.company eq job-hdr.company
                    and b-jh.job     eq job-hdr.job
                    and b-jh.job-no  eq job-hdr.job-no
                    and b-jh.job-no2 eq job-hdr.job-no2
                  no-lock:

                v-qty = v-qty + b-jh.qty.
              end.

              /*v-pct = job-hdr.qty / v-qty.*/
            end.

            /*else
            if est.est-type eq 4 or est.est-type eq 8 then */
              /*v-pct = job-hdr.sq-in / 100*/.
          end.

          leave.
      end.

      if job-hdr.ord-no ne 0 then
      for each ar-inv FIELDS(x-no)
          where ar-inv.company eq cocode
            and ar-inv.ord-no  eq job-hdr.ord-no
            and ar-inv.posted  eq yes
          use-index ord-no no-lock,

          each ar-invl FIELDS(inv-qty)
          where ar-invl.x-no    eq ar-inv.x-no
            and ar-invl.i-no    eq job-hdr.i-no
            and ar-invl.job-no  eq job-hdr.job-no
            and ar-invl.job-no2 eq job-hdr.job-no2
          no-lock:

        v-qty = ar-invl.inv-qty.

        if tt-report.key-03 eq "SET" and avail eb then do:
          {ce/set-qty.i v-qty eb}
        end.

        v-fg-qty = v-fg-qty + v-qty.
      end.  

      assign
       v-job   = fill(" ",6 - length(trim(job-hdr.job-no))) +
                 trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
       v-date  = date(int(substr(tt-report.key-01,5,2)),
                      int(substr(tt-report.key-01,7,2)),
                      int(substr(tt-report.key-01,1,4)))
       v-pct   = 0.

      IF AVAIL job-mch AND job-mch.dept  eq "GL" AND job-mch.run-complete then 
         ASSIGN v-gl = "  X".
      IF AVAIL job-mch AND (job-mch.dept  eq "RS" OR job-mch.dept  eq "AA") AND job-mch.run-complete then 
         ASSIGN v-rs = "  X".
      IF AVAIL job-mch AND job-mch.dept  eq "PR" AND job-mch.run-complete then 
         ASSIGN v-pr = "  X".
      IF AVAIL job-mch AND job-mch.dept  eq "DC" AND job-mch.run-complete then 
         ASSIGN v-dc = "  X".

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
        /*v-pct = oe-ordl.under-pct / 100.*/
      end.

      release eb.
      if avail b-est then
      find first eb
          where eb.company    EQ b-est.company
            AND eb.est-no     EQ b-est.est-no
            and eb.form-no    EQ job-hdr.frm
            and (b-est.est-type EQ 1                                       or
                 b-est.est-type EQ 5                                       or
                 (eb.blank-no EQ job-hdr.blank-no and
                  (b-est.est-type EQ 3 OR
                   b-est.est-type EQ 4 OR
                   b-est.est-type EQ 8))                                   or
                 (eb.blank-no eq 0                and
                  (b-est.est-type eq 2 or b-est.est-type eq 6)))
          no-lock no-error.

        IF AVAIL eb AND eb.die-no <> "" THEN 
           ASSIGN v-die-no = eb.die-no.

        IF LAST-OF(job-hdr.blank-no) THEN DO:

          FIND LAST b-job-mch WHERE
               b-job-mch.company EQ cocode AND
               b-job-mch.job-no  EQ job-hdr.job-no AND
               b-job-mch.job-no2 EQ job-hdr.job-no2
               USE-INDEX seq-idx
               NO-LOCK NO-ERROR.

          IF AVAIL b-job-mch THEN
          DO:
             v-run-end-date = b-job-mch.end-date.
             RELEASE b-job-mch.
          END.
          ELSE
             v-run-end-date = ?.
          ASSIGN v-rel-date = ? 
                 cShipTo    = "" .

             IF AVAIL oe-ordl THEN
                 FOR EACH oe-rel NO-LOCK
                 WHERE oe-rel.company EQ oe-ordl.company
                 AND oe-rel.ord-no  EQ oe-ordl.ord-no
                 AND oe-rel.i-no    EQ oe-ordl.i-no
                 AND oe-rel.line    EQ oe-ordl.line
                 BY oe-rel.rel-date DESC:
                 {oe/rel-stat.i v-stat}

                     v-rel-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
                     cShipTo = oe-rel.ship-id .
                        LEAVE.
                 END.
                ASSIGN cPrim-Contact = "" .
                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ cocode
                       AND cust.cust-no EQ tt-report.key-02 NO-ERROR .
                IF AVAIL cust THEN
                FOR EACH empalert NO-LOCK
                    WHERE  empalert.table_rec_key = cust.rec_key
                      AND  empalert.spare-char-1 EQ "Yes" , 
                    FIRST users WHERE users.user_id = empalert.user-id
                     NO-LOCK BY users.user_id  :
                    cPrim-Contact = users.user_name .
                    LEAVE .
                END.
                cSalesRep = "" .
                IF avail cust THEN do:
                    FIND FIRST sman NO-LOCK
                    WHERE sman.company EQ cocode
                    AND sman.sman    EQ cust.sman NO-ERROR.
                
                IF AVAIL sman THEN
                    cSalesRep = sman.sNAME .
                END.
             
             IF AVAIL oe-ordl THEN
              FIND FIRST itemfg NO-LOCK 
                  WHERE itemfg.company EQ cocode
                    AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR .


             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"        THEN cVarValue =  STRING(tt-report.key-02) .
                         WHEN "job"         THEN cVarValue =  v-job .
                         WHEN "frm"         THEN cVarValue =  STRING(job-hdr.frm) .
                         WHEN "blnk"        THEN cVarValue =  STRING(job-mch.blank-no) .
                         WHEN "die"         THEN cVarValue =  STRING(v-die-no) .
                         WHEN "palt"        THEN cVarValue =  IF AVAIL eb THEN STRING(eb.plate-no) ELSE "".
                         WHEN "due-dt"      THEN cVarValue =  IF v-date NE ? THEN STRING(v-date) ELSE ""  .
                         WHEN "comp-dt"     THEN cVarValue =  IF v-run-end-date NE ? THEN STRING(v-run-end-date) ELSE ""     .
                         WHEN "styl"        THEN cVarValue =  IF AVAIL eb THEN STRING(eb.style) ELSE "" .
                         WHEN "qty-glu"     THEN cVarValue =  IF job-mch.dept = "GL" THEN STRING(job-mch.run-qty,"->>>,>>>,>>9")  ELSE "".
                         WHEN "sht"         THEN cVarValue =  STRING(v-rs).
                         WHEN "prntd"       THEN cVarValue =  STRING(v-pr).
                         WHEN "die-cut"     THEN cVarValue =  STRING(v-dc).
                         WHEN "glue"        THEN cVarValue =  STRING(v-gl).
                         WHEN "glu-hrs"     THEN cVarValue =  IF job-mch.dept = "GL" THEN STRING(job-mch.run-hr,"->>>>>,>>9.99") ELSE ""      .
                         WHEN "mfg-date"     THEN cVarValue =  IF AVAIL oe-ordl THEN STRING(oe-ordl.prom-date,"99/99/9999") ELSE ""      .
                         WHEN "rel-date"     THEN cVarValue =  IF v-rel-date <> ? THEN STRING(v-rel-date,"99/99/9999") ELSE ""      .
                         WHEN "order-no"     THEN cVarValue =  IF AVAIL oe-ordl THEN STRING(oe-ordl.ord-no) ELSE "".
                         WHEN "ord-date"     THEN cVarValue =  IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date,"99/99/9999") ELSE ""      .
                         WHEN "fg-item"      THEN cVarValue =  IF AVAIL oe-ordl THEN STRING(oe-ordl.i-no,"x(15)") ELSE ""      .
                         WHEN "po-no"        THEN cVarValue =  IF AVAIL oe-ordl THEN STRING(oe-ordl.po-no,"x(15)") ELSE ""      .
                         WHEN "pri-contact"  THEN cVarValue =   STRING(cPrim-Contact,"x(25)")   .
                         WHEN "csr"          THEN cVarValue =   IF AVAILABLE oe-ord AND oe-ord.csrUser_id NE "" THEN STRING(oe-ord.csrUser_id,"x(8)") ELSE ""   .
                         WHEN "sales-rep"    THEN cVarValue =   IF AVAILABLE cust THEN STRING(cust.sman,"x(8)") ELSE ""   .
                         WHEN "rep-name"     THEN cVarValue =   STRING(cSalesRep,"x(20)")   .
                         WHEN "contact-name" THEN cVarValue =   IF AVAILABLE cust THEN STRING(cust.contact,"x(30)") ELSE ""   .
                         WHEN "fg-cat"       THEN cVarValue =   IF AVAIL oe-ordl AND avail itemfg THEN STRING(itemfg.procat,"x(11)") ELSE ""    .
                         WHEN "loc"       THEN cVarValue =   IF AVAIL oe-ordl AND avail itemfg THEN STRING(itemfg.def-loc,"x(9)") ELSE ""    .
                         WHEN "shipto"       THEN cVarValue =    STRING(cShipTo,"x(8)")   .

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
        END.
      ASSIGN
      v-first[1] = YES
      v-m-list = "".

    END. /* each tt-report */

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

