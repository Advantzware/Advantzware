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

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF NEW SHARED BUFFER xest FOR est.

{sys/inc/msfcalc.i}

{ce/mach-ink.i NEW}

DEF STREAM st-excell.

DEFINE VARIABLE v-style-list AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v-sel-style-list AS CHARACTER   NO-UNDO.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Date,Job#,Machine,Form#,Board,Adder,Style,Blank Width,Blank Length," + 
                           "Gross Sheet Wid,Gross Sheet Len,Net Sheet Wid,Net Sheet Len," + 
                           "Qty,MSF Prod,Cust #,Length,Width,Depth,Tot Up," + 
                           "Colors"
       cFieldListToSelect = "date,job,mach,form,board,adder,style,blk-wid,blk-len," + /*9*/
                            "gro-sht-wid,gro-sht-len,net-sht-wid,net-sht-len," +     /*4*/
                            "qty,msf-pro,cust,len,wid,dep,total-up-die,color" /*9*/

       cFieldLength = "8,9,10,5,10,12,6,11,12," + "15,15,13,13," + "10,10,8,8,7,7,7,6"  
         cFieldType = "c,c,c,i,c,c,c,i,i," + "i,i,i,i," + "i,i,c,i,i,i,i,i" .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Date,Job#,Machine,Form#,Board,Adder,Style,Blank Width,Blank Length," + 
                           "Gross Sheet Wid,Gross Sheet Len,Net Sheet Wid,Net Sheet Len," + 
                           "Qty,MSF Prod,Cust #,Length,Width,Depth,Tot Up," + 
                           "Colors".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date rsQty ~
tb_styles sel-styles sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date rsQty tb_styles ~
sel-styles sl_avail sl_selected lv-ornt lines-per-page rd-dest lv-font-no ~
lv-font-name tb_excel tb_runExcel fi_file td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-colors C-Win 
FUNCTION get-colors RETURNS INTEGER
    (in-est AS CHAR, in-form AS INT )  FORWARD.

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi-styles AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selected styles" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipstd.csv" 
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

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
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

DEFINE VARIABLE rsQty AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Actual Qty", "A",
"Estimated Qty", "E"
     SIZE 56 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.43.

DEFINE VARIABLE sel-styles AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 20 BY 4.29 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_styles AS LOGICAL INITIAL no 
     LABEL "Select styles (uncheck box for report on all styles)" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 2.48 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 2.48 COL 64 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 3.52 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 2
     begin_job-no2 AT ROW 3.52 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 4
     end_job-no AT ROW 3.52 COL 64 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 6
     end_job-no2 AT ROW 3.52 COL 77 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 8
     begin_date AT ROW 4.57 COL 26 COLON-ALIGNED
     end_date AT ROW 4.57 COL 64 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     rsQty AT ROW 5.67 COL 28 NO-LABEL WIDGET-ID 10
     tb_styles AT ROW 7.05 COL 24 WIDGET-ID 20
     sel-styles AT ROW 7.86 COL 27.8 NO-LABEL WIDGET-ID 14
     fi-styles AT ROW 9.19 COL 65 COLON-ALIGNED WIDGET-ID 16 NO-TAB-STOP 
     sl_avail AT ROW 13.24 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.24 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 13.24 COL 58.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.24 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.24 COL 39 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.29 COL 39 WIDGET-ID 40
     btn_down AT ROW 17.29 COL 39 WIDGET-ID 42
     lv-ornt AT ROW 18.81 COL 33 NO-LABEL
     lines-per-page AT ROW 18.81 COL 86 COLON-ALIGNED
     rd-dest AT ROW 19.76 COL 8 NO-LABEL
     lv-font-no AT ROW 20.24 COL 37 COLON-ALIGNED
     lv-font-name AT ROW 21.43 COL 31 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 22.86 COL 70 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.86 COL 92 RIGHT-ALIGNED
     fi_file AT ROW 23.81 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     td-show-parm AT ROW 24.05 COL 7
     btn-ok AT ROW 25.91 COL 27
     btn-cancel AT ROW 25.91 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.52 COL 58.4 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 18.67 COL 5
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.52 COL 3.8 WIDGET-ID 38
     RECT-6 AT ROW 18.57 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.1
         SIZE 94.4 BY 26.57.


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
         TITLE              = "Machines MSF Produced by Job"
         HEIGHT             = 26.86
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR FILL-IN fi-styles IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi-styles:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd-dest IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Machines MSF Produced by Job */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Machines MSF Produced by Job */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
  END CASE. 
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME sel-styles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sel-styles C-Win
ON VALUE-CHANGED OF sel-styles IN FRAME FRAME-A
DO:
    assign
        tb_styles:SCREEN-VALUE = "YES"
        tb_styles.
    RUN GetSelectedStyles.
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


&Scoped-define SELF-NAME tb_styles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_styles C-Win
ON VALUE-CHANGED OF tb_styles IN FRAME FRAME-A /* Select styles (uncheck box for report on all styles) */
DO:
  ASSIGN tb_styles.


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

  ASSIGN
   begin_date  = DATE (1,1,YEAR(TODAY))
   end_date    = DATE (12,31,year(TODAY)).
  RUN DisplaySelectionList.
  RUN enable_UI.

  for each style WHERE style.company = cocode NO-LOCK:
     v-style-list = v-style-list + string(style.style,"x(5)") /* + " " + mat.dscr */ + ",".
   end.
   if substr(v-style-list,length(trim(v-style-list)),1) eq "," then
     substr(v-style-list,length(trim(v-style-list)),1) = "".

   sel-styles:LIST-ITEMS = v-style-list.

/*    do i = 1 to sel-styles:num-items:    */
/*        sel-styles:is-selected(i) = YES. */
/*    end.                                 */

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_mach.
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
  DISPLAY begin_mach end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_date end_date rsQty tb_styles sel-styles sl_avail sl_selected 
          lv-ornt lines-per-page rd-dest lv-font-no lv-font-name tb_excel 
          tb_runExcel fi_file td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_date end_date rsQty tb_styles sel-styles 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectedStyles C-Win 
PROCEDURE GetSelectedStyles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
v-sel-style-list = "".
DO WITH FRAME {&frame-name}:
    DO i = 1 to sel-styles:NUM-ITEMS:
      IF sel-styles:IS-SELECTED(i) then
          v-sel-style-list = v-sel-style-list + TRIM(SUBSTR(sel-styles:ENTRY(i),1,5)) + ",".
    END.
    IF v-sel-style-list NE "" THEN
        IF SUBSTR(v-sel-style-list,LENGTH(TRIM(v-sel-style-list)),1) EQ "," THEN
            SUBSTR(v-sel-style-list,LENGTH(TRIM(v-sel-style-list)),1) = "". 
    fi-styles = v-sel-style-list.
    DO i = 1 TO LENGTH(fi-styles):
        IF SUBSTR(fi-styles,i,1) EQ "," THEN SUBSTR(fi-styles,i,1) = " ".
    END.  
    DISPLAY fi-styles.
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
    /* DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.*/
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

  /*list-name = fi_file.*/

  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
/*{sys/form/r-top3w.f}*/

DEF VAR str_buffa   AS   CHAR NO-UNDO.
DEF VAR v-hdr       AS   CHAR NO-UNDO.
DEF VAR lv-rc-seq   LIKE dept.fc NO-UNDO.
DEF VAR ld-msf      AS   DEC NO-UNDO.
DEF VAR li-up       AS   INT NO-UNDO.
DEF VAR lv-out      AS   CHAR NO-UNDO.
DEF VAR ld-qty-ton AS DEC NO-UNDO.
DEF VAR ld-qty-msf AS DEC NO-UNDO.
DEF VAR ld-tot-msf AS DEC NO-UNDO.
DEF VAR v-adder AS CHAR NO-UNDO.

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



ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112} .


IF tb_styles THEN RUN GetSelectedStyles. 

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

        IF LOOKUP(ttRptSelected.TextList, "MSF Prod") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

 IF tb_excel THEN DO:
     OUTPUT STREAM st-excell TO VALUE(fi_file).

     PUT STREAM st-excell UNFORMATTED excelheader SKIP.
 END.

SESSION:SET-WAIT-STATE ("general").

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

/*IF td-show-parm THEN RUN show-param.*/

display "" with frame r-top.

RUN est/rc-seq.p (OUTPUT lv-rc-seq).


IF rsQty = "A" THEN DO:
  FOR EACH mch-act NO-LOCK
      WHERE mch-act.company EQ cocode
        AND mch-act.m-code  GE begin_mach
        AND mch-act.m-code  LE end_mach
        AND mch-act.job-no GE begin_job-no
        AND mch-act.job-no LE END_job-no
        AND mch-act.op-date GE begin_date
        AND mch-act.op-date LE end_date
      USE-INDEX dly-idx,

      FIRST mach NO-LOCK
      WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code,

      FIRST job NO-LOCK
      WHERE job.company EQ mch-act.company
        AND job.job     EQ mch-act.job
        AND job.job-no  EQ mch-act.job-no
        AND job.job-no2 EQ mch-act.job-no2
      USE-INDEX job:

      {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    FIND FIRST job-code WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.

    FIND FIRST job-mch NO-LOCK
        WHERE job-mch.company  EQ mch-act.company
          AND job-mch.job      EQ mch-act.job
          AND job-mch.job-no   EQ mch-act.job-no
          AND job-mch.job-no2  EQ mch-act.job-no2
          AND job-mch.frm      EQ mch-act.frm
          AND job-mch.blank-no EQ mch-act.blank-no
          AND job-mch.m-code   EQ mch-act.m-code
          AND job-mch.pass     EQ mch-act.pass
        NO-ERROR.

    RELEASE est.
    RELEASE ef.
    RELEASE eb.

    IF TRIM(job.est-no) NE "" THEN
    FIND FIRST est NO-LOCK
        WHERE est.company EQ job.company
          AND est.est-no  EQ job.est-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ mch-act.frm
        NO-ERROR.

    IF AVAIL ef THEN
    FIND FIRST eb NO-LOCK
        WHERE eb.company   EQ ef.company
          AND eb.est-no    EQ ef.est-no
          AND eb.form-no   EQ ef.form-no
          AND (eb.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
        NO-ERROR.

    IF tb_styles AND AVAIL eb THEN
          IF LOOKUP(eb.style,v-sel-style-list) = 0 THEN NEXT.

    IF AVAIL ef THEN
    FIND FIRST est-flm NO-LOCK
        WHERE est-flm.company  EQ ef.company
          AND est-flm.est-no   EQ ef.est-no
          AND est-flm.snum     EQ ef.form-no
          AND est-flm.bnum     EQ mch-act.blank-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST est-op NO-LOCK
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
          AND est-op.s-num   EQ mch-act.frm
          AND (est-op.b-num  EQ mch-act.blank-no OR
               mch-act.blank-no EQ 0)
          AND est-op.m-code  EQ mch-act.m-code
          AND est-op.op-pass EQ mch-act.pass
          AND est-op.dept    EQ mch-act.dept
          AND est-op.line    LT 500
        NO-ERROR.

    ld-msf = 0.
    IF AVAIL ef THEN DO:
      IF mach.d-seq LT lv-rc-seq THEN
        ld-msf = ef.nsh-len * ef.nsh-wid.
      ELSE
        ld-msf = ef.gsh-len * ef.gsh-wid.

      IF v-corr THEN ld-msf = ld-msf * .007.
                ELSE ld-msf = ld-msf / 144.

      ld-msf = ld-msf / 1000.
    END.

    RELEASE w-ink.
    IF AVAIL est THEN DO:
      FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
      RUN ce/mach-ink.p.
      FIND FIRST w-ink
          WHERE w-ink.form-no EQ mch-act.frm
            AND w-ink.pass    EQ mch-act.pass
          NO-ERROR.
    END.

    li-up = 1.
    IF AVAIL job-mch THEN DO:
      IF job-mch.n-out GT 0 THEN li-up = job-mch.n-out.
      IF job-mch.n-on  GT 0 THEN li-up = job-mch.n-on / li-up.
      IF li-up EQ ? THEN li-up = 1.
    END.


    IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:     
        IF AVAIL eb THEN 
        IF mach.p-type = "R" THEN
           ld-qty-msf = mch-act.qty * ef.gsh-wid * ef.gsh-len / 1000 / 144.
        ELSE IF mach.p-type = "S" THEN
           ld-qty-msf = mch-act.qty * ef.nsh-wid * ef.nsh-len / 1000 / 144.
        ELSE IF mach.p-type = "B" THEN
           ld-qty-msf = mch-act.qty * eb.t-wid * eb.t-len / 1000 / 144.

         ld-tot-msf = ld-tot-msf + ld-qty-msf.
     END.  
     /* Task 11251304 */
      FIND FIRST item WHERE item.company EQ cocode 
                 AND item.i-no EQ ef.adder[1]
                 AND (ITEM.mat-type = "A") NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN v-adder = ITEM.i-no .
         ELSE v-adder = "".  /* Task 11251304 */

       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "date"    THEN cVarValue = STRING(mch-act.op-date,"99/99/99") .
                         WHEN "job"   THEN cVarValue = TRIM(job.job-no) + "-" + STRING(job.job-no2,"99").
                         WHEN "mach"   THEN cVarValue = STRING(mch-act.m-code) .
                         WHEN "form"  THEN cVarValue = IF AVAIL eb THEN string(eb.form-no) ELSE "" .
                         WHEN "board"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.board) ELSE "".
                         WHEN "adder"  THEN cVarValue = STRING(v-adder,"x(12)") .
                         WHEN "style"   THEN cVarValue = IF AVAIL eb THEN eb.style ELSE "" .
                         WHEN "blk-wid"  THEN cVarValue = IF AVAIL eb THEN STRING(eb.t-wid,">>>>>>>>9.9<<<<") ELSE "" .

                         WHEN "blk-len"    THEN cVarValue = IF AVAIL eb THEN STRING(eb.t-len,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "gro-sht-wid"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>>>>>>>>9.9<<<<") ELSE "".
                         WHEN "gro-sht-len"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.gsh-len,">>>>>>>>>>>9.9<<<<") ELSE "".
                         WHEN "net-sht-wid"  THEN cVarValue = IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "net-sht-len"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.nsh-len,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "qty"  THEN cVarValue = STRING(mch-act.qty,"->>>>>>>>>").
                         WHEN "msf-pro"   THEN cVarValue = STRING(ld-qty-msf,">>>>>9.999") .
                        WHEN "cust"    THEN cVarValue = IF AVAIL eb THEN eb.cust-no ELSE "" .
                         WHEN "len"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "".
                         WHEN "wid"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "".
                         WHEN "dep"  THEN cVarValue = IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "" .
                         WHEN "total-up-die"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.num-up,">>>>>>>9") ELSE "" .
                         WHEN "color"  THEN cVarValue = IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">>>>>9") ELSE "" .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM st-excell UNFORMATTED  
                       cExcelDisplay SKIP.
             END.


     /*IF tb_excel THEN DO:

      lv-out =
          TRIM(STRING(mch-act.op-date,"99/99/99"))                      + "," +
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")             + "," +
          TRIM(mch-act.m-code)                                          + "," +
          TRIM(string(eb.form-no))                                      + "," +
          TRIM(ef.board)                                               + "," +
          TRIM(v-adder)                                               + "," +
          TRIM(IF AVAIL eb THEN eb.style ELSE "")                       + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-wid,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-len,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(STRING(mch-act.qty,">>>>>>>>>>"))                        + "," +
          STRING(ld-qty-msf)                                            + ",," +
          TRIM(IF AVAIL eb THEN eb.cust-no ELSE "")                     + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.num-up,">9") ELSE "")         + "," +
          TRIM(IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">9") ELSE "")
          .

         IF lv-out NE ? THEN PUT STREAM st-excell UNFORMATTED lv-out SKIP.
     END.*/
  END.  /* for each mch-act */
END.  /* rsQty = "A" */
ELSE DO:   /* rsQty = "E" */    

      FOR EACH job-mch NO-LOCK
      WHERE job-mch.company EQ cocode
        AND job-mch.job-no  GE begin_job-no
        AND job-mch.job-no LE END_job-no
        AND job-mch.m-code GE begin_mach
        AND job-mch.m-code LE END_mach
        ,
        FIRST job OF job-mch NO-LOCK WHERE job.close-date GE begin_date
                    AND job.close-date LE END_date
          ,
       FIRST mach NO-LOCK
      WHERE mach.company EQ job-mch.company
        AND mach.m-code  EQ job-mch.m-code:

          {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    IF TRIM(job.est-no) NE "" THEN
    FIND FIRST est NO-LOCK
        WHERE est.company EQ job.company
          AND est.est-no  EQ job.est-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ job-mch.frm
        NO-ERROR.    

    IF AVAIL ef THEN
     FIND FIRST eb NO-LOCK
       WHERE eb.company   EQ ef.company
         AND eb.est-no    EQ ef.est-no
         AND eb.form-no   EQ ef.form-no
         AND (eb.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
       NO-ERROR.

    IF tb_styles AND AVAIL eb THEN
          IF LOOKUP(eb.style,v-sel-style-list) = 0 THEN NEXT.

  IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:     
        IF AVAIL eb THEN
         IF mach.p-type = "R" THEN
           ld-qty-msf = job-mch.run-qty * ef.gsh-wid * ef.gsh-len / 1000 / 144.
        ELSE IF mach.p-type = "S" THEN
           ld-qty-msf = job-mch.run-qty * ef.nsh-wid * ef.nsh-len / 1000 / 144.
        ELSE IF mach.p-type = "B" THEN
           ld-qty-msf = job-mch.run-qty * eb.t-wid * eb.t-len / 1000 / 144.

         ld-tot-msf = ld-tot-msf + ld-qty-msf.
     END.  
  /* Task 11251304 */
    FIND FIRST item WHERE item.company EQ cocode 
                 AND item.i-no EQ ef.adder[1]
                 AND (ITEM.mat-type = "A") NO-LOCK NO-ERROR .
         IF AVAIL ITEM THEN
             ASSIGN v-adder = ITEM.i-no .
         ELSE v-adder = "".  /* Task 11251304 */

               ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "date"    THEN cVarValue = STRING(job.close-date,"99/99/99") .
                         WHEN "job"   THEN cVarValue = TRIM(job.job-no) + "-" + STRING(job.job-no2,"99").
                         WHEN "mach"   THEN cVarValue = STRING(job-mch.m-code) .
                         WHEN "form"  THEN cVarValue = IF AVAIL eb THEN string(eb.form-no) ELSE "" .
                         WHEN "board"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.board) ELSE "".
                         WHEN "adder"  THEN cVarValue = STRING(v-adder,"x(12)") .
                         WHEN "style"   THEN cVarValue = IF AVAIL eb THEN eb.style ELSE "" .
                         WHEN "blk-wid"  THEN cVarValue = IF AVAIL eb THEN STRING(eb.t-wid,">>>>>>>>9.9<<<<") ELSE "" .

                         WHEN "blk-len"    THEN cVarValue = IF AVAIL eb THEN STRING(eb.t-len,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "gro-sht-wid"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>>>>>>>>9.9<<<<") ELSE "".
                         WHEN "gro-sht-len"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.gsh-len,">>>>>>>>>>>9.9<<<<") ELSE "".
                         WHEN "net-sht-wid"  THEN cVarValue = IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "net-sht-len"   THEN cVarValue = IF AVAIL ef THEN STRING(ef.nsh-len,">>>>>>>>>9.9<<<<") ELSE "" .
                         WHEN "qty"  THEN cVarValue = STRING(job-mch.run-qty,"->>>>>>>>>").
                         WHEN "msf-pro"   THEN cVarValue = STRING(ld-qty-msf,">>>>>9.999") .
                         WHEN "cust"    THEN cVarValue = IF AVAIL eb THEN eb.cust-no ELSE "" .
                         WHEN "len"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "".
                         WHEN "wid"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "".
                         WHEN "dep"  THEN cVarValue = IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "" .
                         WHEN "total-up-die"   THEN cVarValue = IF AVAIL eb THEN STRING(eb.num-up,">>>>>>>9") ELSE "" .
                         WHEN "color"  THEN cVarValue = IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">>>>>9") ELSE "" .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM st-excell UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

     /*IF tb_excel THEN DO:
      lv-out =
          TRIM(STRING(job.close-date,"99/99/99"))                      + "," +
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")             + "," +
          TRIM(job-mch.m-code)                                          + "," +
          TRIM(string(eb.form-no))                                     + "," +
          TRIM(ef.board)                                               + "," +
          TRIM(v-adder)                                               + "," +
          TRIM(IF AVAIL eb THEN eb.style ELSE "")                       + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-wid,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-len,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(STRING(job-mch.run-qty,">>>>>>>>>>"))                    + "," +
          STRING(ld-qty-msf)                                            + ",," +
          TRIM(IF AVAIL eb THEN eb.cust-no ELSE "")                     + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.num-up,">9") ELSE "")         + "," +
          TRIM(IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">9") ELSE "").
        IF lv-out NE ? THEN PUT STREAM st-excell UNFORMATTED lv-out SKIP.
     END.*/
  END.  /* for each job-mch */
END.       /* rsQty = "E" */

  PUT str-line SKIP .
   ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "date"    THEN cVarValue = "" .
                         WHEN "job"   THEN cVarValue = "".
                         WHEN "mach"   THEN cVarValue = "" .
                         WHEN "form"  THEN cVarValue = "" .
                         WHEN "board"   THEN cVarValue = "".
                         WHEN "adder"  THEN cVarValue = "" .
                         WHEN "style"   THEN cVarValue = "" .
                         WHEN "blk-wid"  THEN cVarValue = "" .
                         WHEN "blk-len"    THEN cVarValue = "" .
                         WHEN "gro-sht-wid"   THEN cVarValue =   "".
                         WHEN "gro-sht-len"   THEN cVarValue =   "".
                         WHEN "net-sht-wid"  THEN cVarValue =   "" .
                         WHEN "net-sht-len"   THEN cVarValue =   "" .
                         WHEN "qty"  THEN cVarValue =  "".
                         WHEN "msf-pro"   THEN cVarValue = STRING(ld-tot-msf,">>>>>9.999") .
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "len"   THEN cVarValue =   "".
                         WHEN "wid"   THEN cVarValue =   "".
                         WHEN "dep"  THEN cVarValue =    "" .
                         WHEN "total-up-die"   THEN cVarValue =   "" .
                         WHEN "color"  THEN cVarValue =    "" .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM st-excell UNFORMATTED  
                       cExcelDisplay SKIP.
             END.


SESSION:SET-WAIT-STATE("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
  OUTPUT STREAM st-excell CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-colors C-Win 
FUNCTION get-colors RETURNS INTEGER
    (in-est AS CHAR, in-form AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-col-list AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.
DEF BUFFER bf-eb FOR eb.

v-col-list = "".
FOR EACH bf-eb WHERE bf-eb.company EQ cocode 
                 AND bf-eb.est-no  EQ in-est
                 AND bf-eb.form-no EQ in-form
               NO-LOCK.  
 /* task 11251304  */
 IF est.est-type >= 1 and est.est-type <= 4 THEN do:
  DO i = 1 TO 10:
      IF bf-eb.i-code2[i] GT "" THEN DO:
          IF  LOOKUP(bf-eb.i-code2[i], v-col-list) = 0 THEN
              v-col-list = v-col-list + bf-eb.i-code2[i] + ",".
      END.
  END.
 END.
 IF est.est-type >= 5 THEN do:
  DO i = 1 TO 10:
      IF bf-eb.i-code[i] GT "" THEN DO:
          IF  LOOKUP(bf-eb.i-code[i], v-col-list) = 0 THEN
              v-col-list = v-col-list + bf-eb.i-code[i] + ",".
      END.
  END.
 END.  /* task 11251304  */


END.
v-col-list = TRIM(v-col-list, ",").
RETURN NUM-ENTRIES(v-col-list).   /* Function return */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

