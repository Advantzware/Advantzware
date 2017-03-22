
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-wiplst.w

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

 def TEMP-TABLE work-edit NO-UNDO
   field job like job-mat.job
   field job-no like job-mat.job-no
   field job-no2 like job-mat.job-no2.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Trans Type,Trans Date,Job No.,S,B,Item Number,"
                                            + "Description,Qty Posted,Wst Qty,Mch Hrs,"
                                            + "Mach Code,Job Code,C"
       cFieldListToSelect = "trns-typ,trns-dt,job-no,frm,blnk,i-no," +
                                        "dscr,qty-pstd,wst-qty,mch-hrs," +
                                        "mch-cd,job-cd,vc"
       cFieldLength = "10,10,10,1,1,15," + "30,11,7,7," + "11,11,1" 
       cFieldType = "c,c,c,c,c,c," + "c,i,i,i," + "c,c,c"
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wiplst.csv" 
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
     SIZE 94 BY 9.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 4.05.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

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
     begin_job-no AT ROW 2.71 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 2.71 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 2.71 COL 62 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 2.71 COL 74 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     sl_avail AT ROW 5.95 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 5.95 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 5.95 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 6.95 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 7.95 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 9 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 10 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 12.71 COL 6 NO-LABEL
     lv-ornt AT ROW 12.95 COL 31 NO-LABEL
     lines-per-page AT ROW 12.95 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.1 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 15.91 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.05 COL 31
     tb_excel AT ROW 18.19 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.19 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 19 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.81 COL 18
     btn-cancel AT ROW 20.81 COL 56
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 5.24 COL 59.6 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.76 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 5.24 COL 5 WIDGET-ID 38
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "W.I.P. Posting Edit List"
         HEIGHT             = 21.81
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

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* W.I.P. Posting Edit List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* W.I.P. Posting Edit List */
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
  assign rd-dest.
       
  RUN GetSelectionList.
  run run-report. 

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
   DO WITH FRAME {&FRAME-NAME}:

    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    IF end_job-no:SCREEN-VALUE = "" THEN
        end_job-no:SCREEN-VALUE = "zzzzzz".
    IF end_job-no2:SCREEN-VALUE = "" THEN
        end_job-no2:SCREEN-VALUE = "99".
    APPLY "entry" TO begin_job-no .

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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-job-tot-excel C-Win 
PROCEDURE print-job-tot-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-v-brd-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-mch-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-wst-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-hrs-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-fg-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-oth-job AS DEC NO-UNDO.
   
   DEF VAR viCnt AS INT NO-UNDO.

   PUT STREAM excel UNFORMATTED SKIP(1).

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.
          
   PUT STREAM excel UNFORMATTED
       '"' ("JOB TOTALS - " + STRING(work-edit.job-no) + "-" +
            STRING(work-edit.job-no2))                         '",'
       '"' "BOARD TOTALS:"                                     '",'
       '"' ip-v-brd-job                                        '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "MACHINE TOTALS:"          '",'
       '"' ip-v-mch-job               '",'
       '"' ip-v-wst-job               '",'
       '"' ip-v-hrs-job               '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "FINISHED GOODS TOTALS:"   '",'
       '"' ip-v-fg-job                '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "OTHER MATERIALS TOTALS:"  '",'
       '"' ip-v-oth-job             '",'
       SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-rep-tot-excel C-Win 
PROCEDURE print-rep-tot-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-v-brd-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-mch-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-wst-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-hrs-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-fg-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-oth-job AS DEC NO-UNDO.
   
   DEF VAR viCnt AS INT NO-UNDO.

   PUT STREAM excel UNFORMATTED SKIP(1).

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.
          
   PUT STREAM excel UNFORMATTED
       '"' "REPORT TOTALS - "     '",'
       '"' "BOARD TOTALS:"        '",'
       '"' ip-v-brd-job           '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "MACHINE TOTALS:"          '",'
       '"' ip-v-mch-job               '",'
       '"' ip-v-wst-job               '",'
       '"' ip-v-hrs-job               '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "FINISHED GOODS TOTALS:"   '",'
       '"' ip-v-fg-job                '",'
       SKIP.

   DO viCnt = 1 TO 5:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                         '",'
       '"' "OTHER MATERIALS TOTALS:"  '",'
       '"' ip-v-oth-job             '",'
       SKIP(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------- pc/rep/wip-edit.p 8/94 gb */
/* WIP Edit Listing Report                                                    */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var v-job-no like job.job-no extent 2 initial [" ", "ZZZZZZ"] no-undo.
def var v-job-no2 like job.job-no2 extent 2 initial [00, 99] no-undo.
def var v-date as date extent 2 no-undo.
def var v-brd-job as int format ">>>>>>>>>9" no-undo.
def var v-brd-tot as int format ">>>>>>>>>9" no-undo.
def var v-oth-job as int format ">>>>>>>>>9" no-undo.
def var v-oth-tot as int format ">>>>>>>>>9" no-undo.
def var v-mch-job as int format ">>>>>>>>>9" no-undo.
def var v-mch-tot as int format ">>>>>>>>>9" no-undo.
def var v-fg-job as int format ">>>>>>>>>9" no-undo.
def var v-fg-tot as int format ">>>>>>>>>9" no-undo.
def var v-hrs-job as dec format ">>>>>9.99" no-undo.
def var v-hrs-tot as dec format ">>>>>9.99" no-undo.
def var v-wst-job as int format ">>>>>>>9" no-undo.
def var v-wst-tot as int format ">>>>>>>9" no-undo.

def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

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
{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.

/*FORM HEADER
     hdr-tit format "x(132)" skip
     hdr-tit2 format "x(132)" skip
     hdr-tit3 format "x(132)"

    WITH FRAME r-top.

form item.procat
     mat-act.mat-date FORMAT "99/99/99"
     work-edit.job-no space(0) "-" space(0)
     work-edit.job-no2 format "99"
     mat-act.s-num space(0) "/" space(0)
     mat-act.b-num
     mat-act.i-no
     item.i-name
     mat-act.qty format "->>>>>>>>>9"
     with frame edit-mat down no-attr-space no-box no-labels STREAM-IO width 132.

form item.procat
     mch-act.op-date FORMAT "99/99/99"
     work-edit.job-no space(0) "-" space(0)
     work-edit.job-no2 format "99"
     mch-act.frm format ">9" space(0) "/" space(0)
     mch-act.blank-no
     mch-act.i-no /*"               "*/
     mach.m-dscr "         "
     mch-act.qty format "->>>>>>>9.99"
     mch-act.waste format ">>>>>>>9"
     mch-act.hours format ">>>>>9.99"
     mch-act.m-code
     mch-act.code
     mch-act.complete
     with frame edit-mch down no-attr-space no-box no-labels STREAM-IO width 132.

form item.procat
     fg-act.fg-date  FORMAT "99/99/99"
     work-edit.job-no space(0) "-" space(0)
     work-edit.job-no2 format "99"
     fg-act.s-num space(0) "/" space(0)
     fg-act.b-num
     fg-act.i-no
     fg-act.i-name
     fg-act.qty format "->>>>>>>9.99"
     with frame edit-fg down no-attr-space no-box no-labels STREAM-IO width 132.
*/
assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

   v-job-no[1]   = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
   v-job-no[2]   = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99") 
 /*
 v-job-no[1]    = begin_job-no
 v-job-no[2]    = end_job-no
 v-job-no2[1]   = begin_rel
 v-job-no2[2]   = end_rel
   
       hdr-tit = "TRANS  TRANS      JOB                                      " +
             "                     QUANTITY     WASTE      MACH MACH   JOB     "
       hdr-tit2 = "TYPE    DATE      NUMBER  S/ B ITEM NUMBER     DESCRIPTION " +
             "                       POSTED       QTY     HOURS CODE   CODE  C "
       hdr-tit3 = fill("-", 131)*/.


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
 /* excelheader = "TRANS TYPE,TRANS DATE,JOB NUMBER,S,B,ITEM NUMBER,"
              + "DESCRIPTION,QUANTITY POSTED,WASTE QTY,MACH HOURS,MACH CODE,"
              + "JOB CODE,C". */
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.
   EMPTY TEMP-TABLE work-edit.

   for each mch-act where mch-act.company = cocode and
                          mch-act.opn = yes
                          use-index opn-idx
                          no-lock:
      if fill(" ",6 - length(trim(mch-act.job-no))) +
         trim(mch-act.job-no) + string(int(mch-act.job-no2),"99") < v-job-no[1] or 
         fill(" ",6 - length(trim(mch-act.job-no))) +
         trim(mch-act.job-no) + string(int(mch-act.job-no2),"99") > v-job-no[2] THEN next.

      find first work-edit where work-edit.job = mch-act.job no-error.

      if not available work-edit then
      do:
         create work-edit.
         assign work-edit.job-no = mch-act.job-no
                work-edit.job    = mch-act.job
                work-edit.job-no2 = mch-act.job-no2.
      end.
   end.
   for each mat-act where mat-act.company = cocode and
                          mat-act.opn = yes
                          use-index opn-idx
                          no-lock:

      if fill(" ",6 - length(trim(mat-act.job-no))) +
         trim(mat-act.job-no) + string(int(mat-act.job-no2),"99") < v-job-no[1] or 
         fill(" ",6 - length(trim(mat-act.job-no))) +
         trim(mat-act.job-no) + string(int(mat-act.job-no2),"99") > v-job-no[2] THEN next.

      find first work-edit where work-edit.job = mat-act.job no-error.

      if not available work-edit then
      do:
         create work-edit.
         assign work-edit.job-no = mat-act.job-no
                work-edit.job    = mat-act.job
                work-edit.job-no2 = mat-act.job-no2.
      end.
   end.
   for each fg-act where fg-act.company = cocode and
                         fg-act.opn = yes
                         use-index opn-idx
                         no-lock:

      if fill(" ",6 - length(trim(fg-act.job-no))) +
         trim(fg-act.job-no) + string(int(fg-act.job-no2),"99") < v-job-no[1] or 
         fill(" ",6 - length(trim(fg-act.job-no))) +
         trim(fg-act.job-no) + string(int(fg-act.job-no2),"99") > v-job-no[2] THEN next.

      find first work-edit where work-edit.job = fg-act.job no-error.

      if not available work-edit then
      do:
         create work-edit.
         assign work-edit.job-no = fg-act.job-no
                work-edit.job    = fg-act.job
                work-edit.job-no2 = fg-act.job-no2.
      end.
   end.

   for each misc-act where misc-act.company = cocode and
                          misc-act.opn = yes
                          use-index opn-idx
                          no-lock:

      if fill(" ",6 - length(trim(misc-act.job-no))) +
         trim(misc-act.job-no) + string(int(misc-act.job-no2),"99") < v-job-no[1] or 
         fill(" ",6 - length(trim(misc-act.job-no))) +
         trim(misc-act.job-no) + string(int(misc-act.job-no2),"99") > v-job-no[2] THEN next.

      find first work-edit where work-edit.job = misc-act.job no-error.

      if not available work-edit then
      do:
         create work-edit.
         assign work-edit.job-no = misc-act.job-no
                work-edit.job    = misc-act.job
                work-edit.job-no2 = misc-act.job-no2.
      end.
   end.

    
    for each work-edit break by work-edit.job-no
                               by work-edit.job-no2:
 
         assign v-brd-job = 0
                v-mch-job = 0
                v-fg-job  = 0
                v-oth-job = 0
                v-wst-job = 0
                v-hrs-job = 0.

         for each mat-act where mat-act.company = cocode and
                                mat-act.job = work-edit.job
                                use-index job
                                no-lock:
            if not mat-act.opn then next.

            find item where item.company = cocode and
                            item.i-no    = mat-act.i-no
                            no-lock no-error.
            if not available item then next.

          /*  display item.procat
                    mat-act.mat-date
                    work-edit.job-no
                    work-edit.job-no2
                    mat-act.s-num
                    mat-act.b-num
                    mat-act.i-no
                    item.i-name
                    mat-act.qty
                    with frame edit-mat.
            down with frame edit-mat.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                 '"' item.procat                                  '",'
                 '"' (IF mat-act.mat-date NE ? THEN
                         STRING(mat-act.mat-date) ELSE "")        '",'
                 '"' (STRING(work-edit.job-no) + "-" +
                      STRING(work-edit.job-no2))                  '",'
                 '"' mat-act.s-num                                '",'
                 '"' mat-act.b-num                                '",'
                 '"' mat-act.i-no                                 '",'
                 '"' item.i-name                                  '",'
                 '"' STRING(mat-act.qty,"->>>>>>>>>9")            '",'
                 SKIP.*/

            ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING(item.procat) .
                                    WHEN "trns-dt"      THEN cVarValue =  (IF mat-act.mat-date NE ? THEN STRING(mat-act.mat-date) ELSE "")  .
                                    WHEN "job-no"           THEN cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2)) .
                                    WHEN "frm"              THEN cVarValue =  STRING(mat-act.s-num) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(mat-act.b-num) .
                                    WHEN "i-no"             THEN cVarValue =  STRING(mat-act.i-no) .
                                    WHEN "dscr"             THEN cVarValue =  STRING(item.i-name) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(mat-act.qty,"->>>>>>>>>9")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "".
                                    WHEN "mch-hrs"          THEN cVarValue =  "".
                                    WHEN "mch-cd"           THEN cVarValue =  "".
                                    WHEN "job-cd"           THEN cVarValue =  "".       
                                    WHEN "vc"               THEN cVarValue =  "".
                    
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

            if item.mat-type = "B" then
               assign v-brd-job = v-brd-job + mat-act.qty
                      v-brd-tot = v-brd-tot + mat-act.qty.
            else
               assign v-oth-job = v-oth-job + mat-act.qty
                      v-oth-tot = v-oth-tot + mat-act.qty.
         end.

         for each mch-act where mch-act.company = cocode and
                                mch-act.job = work-edit.job
                                use-index job
                                no-lock:
            if not mch-act.opn then next.

            find mach where mach.company = cocode and
                            mach.loc     = locode and
                            mach.m-code  = mch-act.m-code
                            no-lock no-error.
            if not available mach then next.

           /* display "HRS" @ item.procat
                    mch-act.op-date
                    work-edit.job-no
                    work-edit.job-no2
                    mch-act.frm
                    mch-act.blank-no
                    mch-act.i-no
                    mach.m-dscr
                    mch-act.qty
                    mch-act.waste
                    mch-act.hours
                    mch-act.m-code
                    mch-act.code
                    mch-act.complete
                    with frame edit-mch.
            down with frame edit-mch.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                 '"' "HRS"                                   '",'
                 '"' (IF mch-act.op-date NE ? THEN
                         STRING(mch-act.op-date) ELSE "")    '",'
                 '"' (STRING(work-edit.job-no) + "-" +
                      STRING(work-edit.job-no2))             '",'
                 '"' mch-act.frm                             '",'
                 '"' mch-act.blank-no                        '",'
                 '"' mch-act.i-no                            '",'
                 '"' mach.m-dscr                             '",'
                 '"' STRING(mch-act.qty,"->>>>>>>9.99")      '",'
                 '"' STRING(mch-act.waste,">>>>>>>9")        '",'
                 '"' STRING(mch-act.hours,">>>>>9.99")       '",'
                 '"' mch-act.m-code                          '",'
                 '"' mch-act.CODE                            '",'
                 '"' mch-act.COMPLETE                        '",'
                 SKIP. */
            ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING("HRS") .
                                    WHEN "trns-dt"      THEN cVarValue =  (IF mch-act.op-date NE ? THEN STRING(mch-act.op-date) ELSE "")  .
                                    WHEN "job-no"           THEN cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2)) .
                                    WHEN "frm"              THEN cVarValue =  STRING(mch-act.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(mch-act.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  STRING(mch-act.i-no) .
                                    WHEN "dscr"             THEN cVarValue =  STRING(mach.m-dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(mch-act.qty,"->>>>>>>>>9")  .
                                    WHEN "wst-qty"          THEN cVarValue =  STRING(mch-act.waste,">>>>>>>9") .   
                                    WHEN "mch-hrs"          THEN cVarValue =  STRING(mch-act.hours,">>>>>9.99")  . 
                                    WHEN "mch-cd"           THEN cVarValue =  mch-act.m-code        .              
                                    WHEN "job-cd"           THEN cVarValue =  string(mch-act.CODE)  .                      
                                    WHEN "vc"               THEN cVarValue =  string(mch-act.COMPLETE)  .                  
                    
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

            assign v-mch-job = v-mch-job + mch-act.qty
                   v-wst-job = v-wst-job + mch-act.waste
                   v-hrs-job = v-hrs-job + mch-act.hours
                   v-mch-tot = v-mch-tot + mch-act.qty
                   v-wst-tot = v-wst-tot + mch-act.waste
                   v-hrs-tot = v-hrs-tot + mch-act.hours.
         end.
         for each fg-act where fg-act.company = cocode and
                               fg-act.job = work-edit.job
                               use-index job-idx
                               no-lock:
            if not fg-act.opn then next.

          /*  display "F.G." @ item.procat
                    fg-act.fg-date
                    work-edit.job-no
                    work-edit.job-no2
                    fg-act.s-num
                    fg-act.b-num
                    fg-act.i-no
                    fg-act.i-name
                    fg-act.qty
                    with frame edit-fg.
            down with frame edit-fg.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                 '"' "F.G."                                  '",'
                 '"' (IF fg-act.fg-date NE ? THEN
                         STRING(fg-act.fg-date) ELSE "")     '",'
                 '"' (STRING(work-edit.job-no) + "-" +
                      STRING(work-edit.job-no2))             '",'
                 '"' fg-act.s-num                            '",'
                 '"' fg-act.b-num                            '",'
                 '"' fg-act.i-no                             '",'
                 '"' fg-act.i-name                           '",'
                 '"' STRING(fg-act.qty,"->>>>>>>9.99")       '",'
                 SKIP. */

            ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING("F.G.") .
                                    WHEN "trns-dt"      THEN cVarValue =  (IF fg-act.fg-date NE ? THEN STRING(fg-act.fg-date) ELSE "")  .
                                    WHEN "job-no"           THEN cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2)) .
                                    WHEN "frm"              THEN cVarValue =  STRING(fg-act.s-num) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(fg-act.b-num) .
                                    WHEN "i-no"             THEN cVarValue =  STRING(fg-act.i-no) .
                                    WHEN "dscr"             THEN cVarValue =  STRING(fg-act.i-name) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(fg-act.qty,"->>>>>>>>>9")  .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .   
                                    WHEN "mch-hrs"          THEN cVarValue =  ""  . 
                                    WHEN "mch-cd"           THEN cVarValue =  ""       .              
                                    WHEN "job-cd"           THEN cVarValue =  ""  .                      
                                    WHEN "vc"               THEN cVarValue =  ""  .                  
                    
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

            assign v-fg-job = v-fg-job + fg-act.qty
                   v-fg-tot = v-fg-tot + fg-act.qty.
         end.

         for each misc-act where misc-act.company = cocode and
                                misc-act.job = work-edit.job
                                use-index misc-idx
                                no-lock:
            if not misc-act.opn then next.

            if misc-act.ml then
            do:
               /*display "MSC-M" @ item.procat
                       misc-act.misc-date @ mat-act.mat-date
                       work-edit.job-no @ mat-act.job-no
                       work-edit.job-no2 @ mat-act.job-no2
                       misc-act.frm @ mat-act.s-num
                       misc-act.blank-no @ mat-act.b-num
                       misc-act.i-no @ mat-act.i-no
                       misc-act.dscr @ item.i-dscr
                       misc-act.cost @ mat-act.qty
                       with frame edit-mat.
               down with frame edit-mat.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' "MSC-M"                                 '",'
                      '"' (IF misc-act.misc-date NE ? THEN
                              STRING(misc-act.misc-date) ELSE "") '",'
                      '"' (STRING(work-edit.job-no) + "-" +
                           STRING(work-edit.job-no2))             '",'
                      '"' misc-act.frm                            '",'
                      '"' misc-act.blank-no                       '",'
                      '"' misc-act.i-no                           '",'
                      '"' misc-act.dscr                           '",'
                      '"' STRING(misc-act.cost,"->>,>>9.99")      '",'
                      SKIP. */
                ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING("MSC-M") .
                                    WHEN "trns-dt"      THEN cVarValue =  (IF misc-act.misc-date NE ? THEN STRING(misc-act.misc-date) ELSE "")  .
                                    WHEN "job-no"           THEN cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2)) .
                                    WHEN "frm"              THEN cVarValue =  STRING(misc-act.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(misc-act.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  STRING(misc-act.i-no) .
                                    WHEN "dscr"             THEN cVarValue =  STRING(misc-act.dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(misc-act.cost,"->>,>>9.99")   .
                                    WHEN "wst-qty"          THEN cVarValue =  "" .   
                                    WHEN "mch-hrs"          THEN cVarValue =  ""  . 
                                    WHEN "mch-cd"           THEN cVarValue =  ""       .              
                                    WHEN "job-cd"           THEN cVarValue =  ""  .                      
                                    WHEN "vc"               THEN cVarValue =  ""  .                  
                    
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
            end.
            else
            do:
               /*display "MSC-H" @ item.procat
                       misc-act.misc-date @ mch-act.op-date
                       misc-act.job-no @ work-edit.job-no
                       misc-act.job-no2 @ work-edit.job-no2
                       misc-act.frm @ mch-act.frm
                       misc-act.blank-no @ mch-act.blank-no
                       misc-act.i-no @ mat-act.i-no
                       misc-act.dscr @ mach.m-dscr
                       misc-act.cost @ mch-act.qty
                       mch-act.waste when available mch-act
                       mch-act.hours when available mch-act
                       misc-act.m-code @ mch-act.m-code
                       mch-act.code when available mch-act
                       mch-act.complete when available mch-act
                       with frame edit-mch.
               down with frame edit-mch.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' "MSC-H"                                 '",'
                      '"' (IF misc-act.misc-date NE ? THEN
                              STRING(misc-act.misc-date) ELSE "") '",'
                      '"' (STRING(work-edit.job-no) + "-" +
                           STRING(work-edit.job-no2))             '",'
                      '"' misc-act.frm                            '",'
                      '"' misc-act.blank-no                       '",'
                      '"' misc-act.i-no                               '",'
                      '"' misc-act.dscr                           '",'
                      '"' STRING(misc-act.cost,"->>,>>9.99")      '",'
                      '"' (IF AVAIL mch-act THEN
                              STRING(mch-act.waste) ELSE "")      '",'
                      '"' (IF AVAIL mch-act THEN
                              STRING(mch-act.hours) ELSE "")      '",'
                      '"' misc-act.m-code
                      '"' (IF AVAIL mch-act THEN mch-act.CODE
                           ELSE "")                               '",'
                      '"' (IF AVAIL mch-act THEN
                              STRING(mch-act.COMPLETE) ELSE "")   '",'
                      SKIP.*/
                ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                               CASE cTmpField:             
                                    WHEN "trns-typ"         THEN cVarValue =  STRING("MSC-H") .
                                    WHEN "trns-dt"      THEN cVarValue =  (IF misc-act.misc-date NE ? THEN STRING(misc-act.misc-date) ELSE "")  .
                                    WHEN "job-no"           THEN cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2)) .
                                    WHEN "frm"              THEN cVarValue =  STRING(misc-act.frm) .
                                    WHEN "blnk"             THEN cVarValue =  STRING(misc-act.blank-no) .
                                    WHEN "i-no"             THEN cVarValue =  STRING(misc-act.i-no) .
                                    WHEN "dscr"             THEN cVarValue =  STRING(misc-act.dscr) .
                                    WHEN "qty-pstd"         THEN cVarValue =  STRING(misc-act.cost,"->>,>>9.99")   .
                                    WHEN "wst-qty"          THEN cVarValue =  (IF AVAIL mch-act THEN STRING(mch-act.waste) ELSE "") .   
                                    WHEN "mch-hrs"          THEN cVarValue =  (IF AVAIL mch-act THEN STRING(mch-act.hours) ELSE "")  . 
                                    WHEN "mch-cd"           THEN cVarValue =  misc-act.m-code       .              
                                    WHEN "job-cd"           THEN cVarValue =  (IF AVAIL mch-act THEN mch-act.CODE ELSE "")  .                      
                                    WHEN "vc"               THEN cVarValue =  (IF AVAIL mch-act THEN STRING(mch-act.COMPLETE) ELSE "")  .                  
                    
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
            end.
         end.

      /*   put skip(1) "JOB TOTALS - " at 20 work-edit.job-no
             space(0) "-" space(0) work-edit.job-no2 format "99"
             "         BOARD TOTALS: " at 56 v-brd-job skip
             "       MACHINE TOTALS: " at 56 v-mch-job " " v-wst-job " "
                                             v-hrs-job skip
             "FINISHED GOODS TOTALS: " at 56 v-fg-job skip
             "OTHER MATERIAL TOTALS: " at 56 v-oth-job skip(2).

         IF tb_excel THEN
            RUN print-job-tot-excel(INPUT v-brd-job, INPUT v-mch-job,
                                    INPUT v-wst-job, INPUT v-hrs-job, 
                                    INPUT v-fg-job, INPUT v-oth-job). */
         PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "JOB TOTALS - " + work-edit.job-no + "-" + string(work-edit.job-no2,"99") + "         BOARD TOTALS: "
                           substring(cDisplay,45,300) SKIP.
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " JOB TOTALS    " work-edit.job-no + "-" + string(work-edit.job-no2,"99") + "         BOARD TOTALS: " substring(cExcelDisplay,3,300) SKIP.
                       END. 

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "MACHINE TOTALS : " substring(cDisplay,17,300) SKIP.
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
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "FINISHED GOODS TOTALS : " substring(cDisplay,24,300) SKIP.
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
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "OTHER MATERIAL TOTALS : " substring(cDisplay,24,300) SKIP(2).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

      end.
    /*  put skip(1) "REPORT TOTALS" at 20
             "         BOARD TOTALS: " at 56 v-brd-tot skip
             "       MACHINE TOTALS: " at 56 v-mch-tot " " v-wst-tot " "
                                             v-hrs-tot skip
             "FINISHED GOODS TOTALS: " at 56 v-fg-tot skip
             "OTHER MATERIAL TOTALS: " at 56 v-oth-tot skip.

      IF tb_excel THEN
         RUN print-rep-tot-excel(INPUT v-brd-tot, INPUT v-mch-tot,
                                 INPUT v-wst-tot, INPUT v-hrs-tot, 
                                 INPUT v-fg-tot, INPUT v-oth-tot). */

      PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "REPORT TOTALS  "  "             BOARD TOTALS: "
                           substring(cDisplay,43,300) SKIP(1).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " REPORT TOTALS    "   "         BOARD TOTALS: " substring(cExcelDisplay,3,300) SKIP.
                       END.

                       PUT SKIP str-line SKIP .
               ASSIGN cDisplay = ""
                              cTmpField = ""
                              cVarValue = ""
                              cExcelDisplay = ""
                              cExcelVarValue = "" .
                       
                       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "MACHINE TOTALS : " substring(cDisplay,17,300) SKIP.
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
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "FINISHED GOODS TOTALS : " substring(cDisplay,24,300) SKIP.
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
                          cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
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
                    
                               END CASE.  
                                 
                               cExcelVarValue = cVarValue.
                               cDisplay = cDisplay + cVarValue +
                                          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                               cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                       END.

                       PUT UNFORMATTED "OTHER MATERIAL TOTALS : " substring(cDisplay,24,300) SKIP(1).
                       IF tb_excel THEN DO:
                            PUT STREAM excel UNFORMATTED  
                                  " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                       END.

    /***************************************************** CTS Enh 052495-01
    put control v-end-compress.   /** reset printer pitch **/
    output close.
    ****** Call sys/inc/close.i so Smart Screen print works correctly ******/

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */


IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
  def var lv-label as cha NO-UNDO.
  
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

