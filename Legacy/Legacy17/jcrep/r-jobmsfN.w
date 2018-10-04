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

assign
 cocode = gcompany
 locode = gloc.

DEF TEMP-TABLE tt-report LIKE report FIELD pct AS DEC
                                     FIELD sqft AS DEC
                                     FIELD qty AS DEC
                                     FIELD waste AS DEC
                                     INDEX rec-id rec-id.

{fg/fullset.i NEW}

def stream s-temp.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "FG Item#,Mach#,Charge,Start Date,End Date,Total Time,"
              + "Job#,Run Qty,Waste,Shift,SQF"
       cFieldListToSelect = "ino,mch,chrg,strt-dt,end-dt,ttl-time," 
              + "job,run-qty,whst,shft,sqf"  
       cFieldLength = "15,6,6,10,10,10," + "10,11,11,5,12"
       cFieldType = "c,c,c,c,c,i," + "c,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "FG Item#,Mach#,Charge,Start Date,End Date,Total Time,"
              + "Job#,Run Qty,Waste,Shift,SQF" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_exp-exel ~
tb_runExcel exp-name btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_exp-exel tb_runExcel ~
exp-name sl_avail sl_selected

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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

DEFINE VARIABLE exp-name AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jobmsf.csv" 
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
     SIZE 95 BY 10.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 5.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_exp-exel AS LOGICAL INITIAL yes 
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
     begin_mach AT ROW 3.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.38 COL 64 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 4.33 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.33 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.33 COL 64 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.33 COL 76 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 5.29 COL 26 COLON-ALIGNED
     end_date AT ROW 5.29 COL 64 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     sl_avail AT ROW 7.81 COL 3.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 7.81 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 7.81 COL 61.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 8.81 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 9.81 COL 41.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 10.86 COL 41.2 WIDGET-ID 40
     btn_down AT ROW 11.86 COL 41.2 WIDGET-ID 42
     rd-dest AT ROW 15.19 COL 6 NO-LABEL
     lv-ornt AT ROW 15.43 COL 31 NO-LABEL
     lines-per-page AT ROW 15.43 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.57 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.52 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.57 COL 31
     tb_exp-exel AT ROW 20.67 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.67 COL 72 RIGHT-ALIGNED
     exp-name AT ROW 21.48 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.19 COL 22
     btn-cancel AT ROW 24.19 COL 60
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 7.05 COL 3.8 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.24 COL 3
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 7 COL 61.8 WIDGET-ID 44
     RECT-6 AT ROW 13.29 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1
         SIZE 95.6 BY 24.67.


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
         TITLE              = "MSF by Job"
         HEIGHT             = 24.67
         WIDTH              = 96.8
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
                                                                        */
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

ASSIGN 
       exp-name:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_exp-exel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_exp-exel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* MSF by Job */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MSF by Job */
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
  run run-report. 

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


&Scoped-define SELF-NAME exp-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL exp-name C-Win
ON LEAVE OF exp-name IN FRAME FRAME-A /* If Yes, File Name */
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

&Scoped-define SELF-NAME tb_exp-exel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exp-exel C-Win
ON VALUE-CHANGED OF tb_exp-exel IN FRAME FRAME-A /* Export To Excel? */
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

  ASSIGN
   begin_date  = DATE (1,1,YEAR(TODAY))
   END_date    = DATE (12,31,year(TODAY)) 
   exp-name    = "jobmsf.csv".

  RUN DisplaySelectionList.
  RUN enable_UI.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt C-Win 
PROCEDURE create-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-i-no   LIKE job-hdr.i-no NO-UNDO.
  DEF INPUT PARAM ip-pct    AS   DEC          NO-UNDO.
  DEF INPUT PARAM ip-p-type LIKE mach.p-type  NO-UNDO.
  DEF INPUT PARAM ip-on     LIKE job-hdr.n-on NO-UNDO.

  DEF VAR ld-factor AS DEC NO-UNDO.


  IF AVAIL mch-act THEN DO:
    FIND FIRST itemfg
        WHERE itemfg.company EQ mch-act.company
          AND itemfg.i-no    EQ ip-i-no
        NO-LOCK NO-ERROR.

    IF ip-pct EQ ? THEN ip-pct = 0.
    IF ip-on  EQ 0 THEN ip-on  = 1.

    ld-factor = IF CAN-DO("A,B",ip-p-type) THEN 1
                ELSE
                IF ip-p-type EQ "P"        THEN (ip-pct / 100)
                ELSE ip-on.

    CREATE tt-report.
    ASSIGN
     tt-report.rec-id = RECID(mch-act)
     tt-report.key-01 = ip-i-no
     tt-report.key-02 = mch-act.m-code
     tt-report.pct    = ip-pct
     tt-report.sqft   = IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0
     tt-report.qty    = mch-act.qty * ld-factor
     tt-report.waste  = mch-act.waste * ld-factor.
  END.

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
  DISPLAY begin_mach end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_date end_date rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_exp-exel tb_runExcel exp-name 
          sl_avail sl_selected 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_date end_date rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_exp-exel tb_runExcel 
         exp-name btn-ok btn-cancel btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-excel-total C-Win 
PROCEDURE print-excel-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-text AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-dec1 AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-dec2 AS DEC NO-UNDO.

   DEF VAR viLoop AS INT NO-UNDO.

   IF ip-text EQ "Total Time" THEN
      PUT STREAM s-temp UNFORMATTED
          SKIP(1).

   DO viLoop = 1 TO 4:
      PUT STREAM s-temp UNFORMATTED
          '"' "" '",'.
   END.

   IF ip-text EQ "Total Time" THEN
      PUT STREAM s-temp UNFORMATTED
          '"' ip-text '",'
          '"' STRING(ip-dec1,"->>,>>9.99")       '",'
          '"' ""                                 '",'
          '"' ""                                 '",'
          '"' ""                                 '",'
          '"' ""                                 '",'
          '"' STRING(ip-dec2,"->,>>>,>>9.9<<<<") '",'
          SKIP(1).
   ELSE
      PUT STREAM s-temp UNFORMATTED
          '"' ip-text                      '",'
          '"' STRING(ip-dec1,"$>>,>>9.99") '",'
          SKIP.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*{sys/form/r-top3w.f}*/

DEF BUFFER b-job-hdr FOR job-hdr.

DEF VAR v-fmch   LIKE mch-act.m-code.
DEF VAR v-tmch   LIKE v-fmch                             INIT "zzzzzz".
DEF VAR v-fdat   LIKE job.start-date FORMAT "99/99/9999" INIT 01/01/0001.
DEF VAR v-tdat   LIKE v-fdat                             INIT 12/31/9999.
DEF VAR v-fjob   LIKE job.job-no.
DEF VAR v-tjob   LIKE v-fjob                             INIT "zzzzzz".
DEF VAR v-fjob2  LIKE job.job-no2.
DEF VAR v-tjob2  LIKE v-fjob2                            INIT 99.
DEF VAR v-export AS   LOG FORMAT "Yes/No"                INIT NO.

DEF VAR ld-cost AS DEC NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.
DEF VAR ld-time AS DEC NO-UNDO.
DEF VAR lv-job# AS CHAR NO-UNDO.
DEF VAR ld-sqft AS DEC NO-UNDO.
DEF VAR ld-rate AS DEC NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR ll-one-only AS LOG NO-UNDO.

DEF VAR ld-tot-cost AS DEC NO-UNDO.
DEF VAR ld-tot-time AS DEC NO-UNDO.
DEF VAR ld-tot-sqft AS DEC NO-UNDO.

DEF VAR v-exp-name  AS   CHAR FORMAT "x(40)" INIT "job-msf.csv" NO-UNDO.

DEF VAR v-hdr       AS   CHAR NO-UNDO.
DEF VAR v-hdr-exp   AS   CHAR NO-UNDO.
DEF VAR v-comma     AS   CHAR FORMAT "x" INIT "," NO-UNDO.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(190)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(190)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(190)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}. 
DEF VAR excelheader AS   CHAR NO-UNDO.

/*FORM HEADER SKIP(1)
            ENTRY(01,v-hdr)     FORMAT "x(15)"
            ENTRY(02,v-hdr)     FORMAT "x(6)"
            ENTRY(03,v-hdr)     FORMAT "x(6)"
            ENTRY(04,v-hdr)     FORMAT "x(10)"
            ENTRY(05,v-hdr)     FORMAT "x(10)"
            ENTRY(06,v-hdr)     FORMAT "x(10)"
            ENTRY(07,v-hdr)     FORMAT "x(9)"
            ENTRY(08,v-hdr)     FORMAT "x(11)"
            ENTRY(09,v-hdr)     FORMAT "x(11)"
            ENTRY(10,v-hdr)     FORMAT "x(5)"
            ENTRY(11,v-hdr)     FORMAT "x(12)"
            SKIP

            "---------------"
            "------"
            "------"
            "----------"
            "----------"
            "----------"
            "---------"
            "-----------"
            "-----------"
            "-----"
            "------------"
            SKIP

    WITH FRAME r-top. */


ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 v-fmch     = begin_mach
 v-tmch     = end_mach
 v-fdat     = begin_date
 v-tdat     = end_date  
 v-export   = tb_exp-exel
 v-exp-name = exp-name

 v-fjob     = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
              TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
 v-tjob     = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
              TRIM(end_job-no)   + STRING(INT(end_job-no2),"99"). 


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

        IF LOOKUP(ttRptSelected.TextList, "Total Time,SQF") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

SESSION:SET-WAIT-STATE ("general").

IF tb_exp-exel THEN DO:
  OUTPUT STREAM s-temp TO VALUE(exp-name).
 /* excelheader = "FG Item#,Mach#,Charge,Start Date,End Date,Total Time,"
              + "Job#,Run Qty,Waste,Shift,SQF". */
  PUT STREAM s-temp UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  FOR EACH mch-act NO-LOCK
      WHERE mch-act.company EQ cocode
        AND mch-act.m-code  GE v-fmch
        AND mch-act.m-code  LE v-tmch
        AND mch-act.op-date GE v-fdat
        AND mch-act.op-date LE v-tdat
        AND mch-act.job-no  GE SUBSTR(v-fjob,1,6)
        AND mch-act.job-no  LE SUBSTR(v-tjob,1,6)
        AND FILL(" ",6 - LENGTH(TRIM(mch-act.job-no))) +
            TRIM(mch-act.job-no) + STRING(mch-act.job-no2,"99")
                            GE v-fjob
        AND FILL(" ",6 - LENGTH(TRIM(mch-act.job-no))) +
            TRIM(mch-act.job-no) + STRING(mch-act.job-no2,"99")
                            LE v-tjob
      USE-INDEX operation,

      FIRST mach
      WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code
      NO-LOCK,

      FIRST job NO-LOCK
      WHERE job.company EQ mch-act.company
        AND job.job     EQ mch-act.job
        AND job.job-no  EQ mch-act.job-no
        AND job.job-no2 EQ mch-act.job-no2
      USE-INDEX job,

      FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ mch-act.company
        AND job-hdr.job     EQ mch-act.job
        AND job-hdr.job-no  EQ mch-act.job-no
        AND job-hdr.job-no2 EQ mch-act.job-no2:

    FOR EACH tt-fg-set:
      DELETE tt-fg-set.
    END.

    ASSIGN
     ld-cost     = 0
     ll-one-only = CAN-FIND(b-job-hdr WHERE b-job-hdr.company EQ mch-act.company
                                        AND b-job-hdr.job     EQ mch-act.job
                                        AND b-job-hdr.job-no  EQ mch-act.job-no
                                        AND b-job-hdr.job-no2 EQ mch-act.job-no2).

    IF mach.p-type NE "A"                                   AND
       CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                  AND reftable.company  EQ job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(job.job,"999999999")
                  AND reftable.val[12]  EQ mch-act.frm)     AND
       NOT CAN-FIND(FIRST reftable
                    WHERE reftable.reftable EQ "jc/jc-calc.p"
                      AND reftable.company  EQ job.company
                      AND reftable.loc      EQ ""
                      AND reftable.code     EQ STRING(job.job,"999999999")
                      AND reftable.code2    EQ job-hdr.i-no
                      AND reftable.val[12]  EQ mch-act.frm) THEN DO:

      IF mach.p-type EQ "P" THEN DO:
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
              AND itemfg.i-no    EQ job-hdr.i-no
            NO-ERROR.
        IF AVAIL itemfg THEN RUN fg/fullset.p (ROWID(itemfg)).
      END.

      FOR EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND (reftable.val[12] EQ mch-act.frm OR mach.p-type EQ "P")
          NO-LOCK:

        FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2 NO-ERROR.

        ld-cost = ld-cost + IF AVAIL tt-fg-set THEN tt-fg-set.part-qty-dec
                                               ELSE reftable.val[5].
      END.

      FOR EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND (reftable.val[12] EQ mch-act.frm OR mach.p-type EQ "P")
          NO-LOCK:

        FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2 NO-ERROR.

        ld = IF AVAIL tt-fg-set THEN tt-fg-set.part-qty-dec
                                ELSE reftable.val[5].

        RUN create-tt (reftable.code2,
                       ld / ld-cost * 100,
                       mach.p-type,
                       reftable.val[11]).
      END.
    END.

    ELSE
    IF ll-one-only THEN RUN create-tt (job-hdr.i-no, 100, mach.p-type, job-hdr.n-on).

    ELSE DO:
      FOR EACH b-job-hdr FIELDS(std-tot-cost)
          WHERE b-job-hdr.company     EQ job-hdr.company
            AND b-job-hdr.job         EQ job-hdr.job
            AND b-job-hdr.job-no      EQ job-hdr.job-no
            AND b-job-hdr.job-no2     EQ job-hdr.job-no2
            AND (ll-one-only OR
                 (b-job-hdr.frm       EQ mch-act.frm AND
                  (b-job-hdr.blank-no EQ mch-act.blank-no OR
                   mch-act.blank-no   EQ 0)))
            NO-LOCK:
        ld-cost = ld-cost + b-job-hdr.std-tot-cost.
      END.
      FOR EACH b-job-hdr FIELDS(i-no std-tot-cost n-on) NO-LOCK
          WHERE b-job-hdr.company     EQ job-hdr.company
            AND b-job-hdr.job         EQ job-hdr.job
            AND b-job-hdr.job-no      EQ job-hdr.job-no
            AND b-job-hdr.job-no2     EQ job-hdr.job-no2
            AND (ll-one-only OR
                 (b-job-hdr.frm       EQ mch-act.frm AND
                  (b-job-hdr.blank-no EQ mch-act.blank-no OR
                   mch-act.blank-no   EQ 0))):
        RUN create-tt (b-job-hdr.i-no,
                       b-job-hdr.std-tot-cost / ld-cost * 100,
                       mach.p-type,
                       b-job-hdr.n-on).
      END.
    END.
  END.

 /* v-hdr = "FG Item#,Mach#,Charge,Start Date,End Date,Total Time," +
          "     Job#,    Run Qty,      Waste,Shift,         SQF".*/

  VIEW FRAME r-top.

  FOR EACH tt-report,

      FIRST mch-act WHERE RECID(mch-act) EQ tt-report.rec-id NO-LOCK,

      FIRST mach
      WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code
      NO-LOCK,

      FIRST job-code WHERE job-code.code EQ mch-act.code NO-LOCK

      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY mch-act.op-date
            BY mch-act.start
            BY mch-act.job-no2
            BY mch-act.frm
            BY mch-act.blank-no:  

    ASSIGN
     lv-date = STRING(mch-act.op-date,"99/99/9999") + " " +
               STRING(mch-act.op-date,"99/99/9999")
     ld-time = (mch-act.stopp - mch-act.start) / 3600 *
               mch-act.crew * (tt-report.pct / 100)
     lv-job# = FILL(" ",6 - LENGTH(TRIM(mch-act.job-no))) +
               TRIM(mch-act.job-no) + "-" + STRING(INT(mch-act.job-no2),"99")
     ld-sqft = tt-report.qty * tt-report.sqft.

    IF ld-time EQ ? THEN ld-time = 0.

  /*  DISPLAY tt-report.key-01              FORMAT "x(15)"
            mch-act.m-code
            mch-act.code                  FORMAT "x(6)"
            lv-date                       FORMAT "x(21)"
            ld-time                       FORMAT "->>,>>9.99"
            lv-job#                       FORMAT "x(9)"
            tt-report.qty                 FORMAT "->>,>>>,>>9"
            tt-report.waste               FORMAT "->>,>>>,>>9"
            mch-act.shift                 FORMAT ">>>>>"
            ld-sqft                       FORMAT "->,>>>,>>9.9<<<<"

        WITH FRAME detail NO-ATTR-SPACE NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.
    DOWN WITH FRAME detail.

    IF v-export THEN DO:

      PUT STREAM s-temp UNFORMATTED
          '"' tt-report.key-01                      '",'
          '"' mch-act.m-code                        '",'
          '"' mch-act.CODE                          '",'
          '"' IF mch-act.op-date NE ? THEN
                 STRING(mch-act.op-date,"99/99/9999")
                 ELSE ""                            '",'
          '"' IF mch-act.op-date NE ? THEN
                 STRING(mch-act.op-date,"99/99/9999")
                 ELSE ""                            '",'
          '"' STRING(ld-time,"->>,>>9.99")          '",'
          '"' lv-job#                               '",'
          '"' STRING(tt-report.qty,"->>,>>>,>>9")   '",'
          '"' STRING(tt-report.waste,"->>,>>>,>>9") '",'
          '"' STRING(mch-act.shift,">>>>>")         '",'
          '"' STRING(ld-sqft,"->,>>>,>>9.9<<<<")    '",'
          SKIP.
    END.*/

    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "" .

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                 WHEN "ino"      THEN cVarValue =  STRING(tt-report.key-01) .
                 WHEN "mch"      THEN cVarValue =  STRING(mch-act.m-code) .
                 WHEN "chrg"     THEN cVarValue =  STRING(mch-act.CODE) .
                 WHEN "strt-dt"  THEN cVarValue =  IF mch-act.op-date NE ? THEN STRING(mch-act.op-date,"99/99/9999") ELSE "" .
                 WHEN "end-dt"   THEN cVarValue =  IF mch-act.op-date NE ? THEN STRING(mch-act.op-date,"99/99/9999") ELSE ""  .
                 WHEN "ttl-time" THEN cVarValue =  STRING(ld-time,"->>,>>9.99") .
                 WHEN "job"      THEN cVarValue =  lv-job# .
                 WHEN "run-qty"  THEN cVarValue =  STRING(tt-report.qty,"->>,>>>,>>9") .
                 WHEN "whst"     THEN cVarValue =  STRING(tt-report.waste,"->>,>>>,>>9") .
                 WHEN "shft"     THEN cVarValue =  STRING(mch-act.shift,">>>>>") .
                 WHEN "sqf"      THEN cVarValue =  STRING(ld-sqft,"->,>>>,>>9.9<<<<") .

            END CASE.  

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED cDisplay SKIP.
    IF v-export THEN DO:
         PUT STREAM s-temp UNFORMATTED  
               cExcelDisplay SKIP.
    END.

    ld-rate = IF job-code.cat EQ "MR" THEN (mach.mr-rate / mach.mr-crusiz)
                                      ELSE (mach.run-rate / mach.run-crusiz).
    IF ld-rate EQ ? THEN ld-rate = 0.
    IF ld-rate EQ 0 THEN ld-rate = mach.lab-rate[mach.lab-drate] NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ld-rate = 0.

    ASSIGN
     ld-tot-cost = ld-tot-cost + (ld-time * ld-rate)
     ld-tot-time = ld-tot-time + ld-time
     ld-tot-sqft = ld-tot-sqft + ld-sqft.

    IF LAST(tt-report.key-01) THEN DO:
     /* UNDERLINE ld-time ld-sqft WITH FRAME detail.

      DISPLAY "           Total Time"           @ lv-date
              ld-tot-time                       @ ld-time
              ld-tot-sqft                       @ ld-sqft

          WITH FRAME detail.
      DOWN WITH FRAME detail.*/

         PUT SKIP str-line SKIP .
         ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "" .

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField:             
                      WHEN "ino"      THEN cVarValue =  "" .
                      WHEN "mch"      THEN cVarValue =  "" .
                      WHEN "chrg"     THEN cVarValue =  "" .
                      WHEN "strt-dt"  THEN cVarValue =  "" .
                      WHEN "end-dt"   THEN cVarValue =  "" .
                      WHEN "ttl-time" THEN cVarValue =  STRING(ld-tot-time,"$>>,>>9.99") .
                      WHEN "job"      THEN cVarValue =  "" .
                      WHEN "run-qty"  THEN cVarValue =  "" .
                      WHEN "whst"     THEN cVarValue =  "" .
                      WHEN "shft"     THEN cVarValue =  "" .
                      WHEN "sqf"      THEN cVarValue =  STRING(ld-tot-sqft,"->,>>>,>>9.9<<<<") .

                 END CASE.  

                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.

         PUT UNFORMATTED " Total Hours: " substring(cDisplay,15,300) SKIP.
         IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   "Total Time: " + substring(cExcelDisplay,3,300) SKIP.
        END.

      PUT SKIP(1).

     /* ld-time:FORMAT IN FRAME detail = "$>>,>>9.99".*/

      /*DISPLAY "  Labor Per Crew Hour"           @ lv-date
              ld-tot-cost / ld-tot-time         @ ld-time

          WITH FRAME detail.
      DOWN WITH FRAME detail. */


         ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "" .

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField:             
                      WHEN "ino"      THEN cVarValue =  "" .
                      WHEN "mch"      THEN cVarValue =  "" .
                      WHEN "chrg"     THEN cVarValue =  "" .
                      WHEN "strt-dt"  THEN cVarValue =  "" .
                      WHEN "end-dt"   THEN cVarValue =  "" .
                      WHEN "ttl-time" THEN cVarValue =  STRING((ld-tot-cost / ld-tot-time),"$>>,>>9.99") .
                      WHEN "job"      THEN cVarValue =  "" .
                      WHEN "run-qty"  THEN cVarValue =  "" .
                      WHEN "whst"     THEN cVarValue =  "" .
                      WHEN "shft"     THEN cVarValue =  "" .
                      WHEN "sqf"      THEN cVarValue =  "" .

                 END CASE.  

                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.

         PUT UNFORMATTED " Labor Per Crew Hour " substring(cDisplay,22,300) SKIP.
         IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   "Labor Per Crew Hour " + substring(cExcelDisplay,3,300) SKIP.
        END.

    /*  DISPLAY "   Total Direct Labor"           @ lv-date
              ld-tot-cost                       @ ld-time

          WITH FRAME detail.
      DOWN WITH FRAME detail. */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "" .

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField:             
                      WHEN "ino"      THEN cVarValue =  "" .
                      WHEN "mch"      THEN cVarValue =  "" .
                      WHEN "chrg"     THEN cVarValue =  "" .
                      WHEN "strt-dt"  THEN cVarValue =  "" .
                      WHEN "end-dt"   THEN cVarValue =  "" .
                      WHEN "ttl-time" THEN cVarValue =  STRING(ld-tot-cost,"$>>,>>9.99") .
                      WHEN "job"      THEN cVarValue =  "" .
                      WHEN "run-qty"  THEN cVarValue =  "" .
                      WHEN "whst"     THEN cVarValue =  "" .
                      WHEN "shft"     THEN cVarValue =  "" .
                      WHEN "sqf"      THEN cVarValue =  "" .

                 END CASE.  

                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.

         PUT UNFORMATTED " Total Direct Labor " substring(cDisplay,21,300) SKIP.
         IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   "Total Direct Labor " + substring(cExcelDisplay,3,300) SKIP.
        END.

     /* DISPLAY "         Cost Per MSF"           @ lv-date
              ld-tot-cost / ld-tot-sqft * 1000  @ ld-time

          WITH FRAME detail.
      DOWN WITH FRAME detail. */


         ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "" .

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField:             
                      WHEN "ino"      THEN cVarValue =  "" .
                      WHEN "mch"      THEN cVarValue =  "" .
                      WHEN "chrg"     THEN cVarValue =  "" .
                      WHEN "strt-dt"  THEN cVarValue =  "" .
                      WHEN "end-dt"   THEN cVarValue =  "" .
                      WHEN "ttl-time" THEN cVarValue =  STRING((ld-tot-cost / ld-tot-sqft * 1000),"$>>,>>9.99") .
                      WHEN "job"      THEN cVarValue =  "" .
                      WHEN "run-qty"  THEN cVarValue =  "" .
                      WHEN "whst"     THEN cVarValue =  "" .
                      WHEN "shft"     THEN cVarValue =  "" .
                      WHEN "sqf"      THEN cVarValue =  "" .

                 END CASE.  

                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.

         PUT UNFORMATTED " Cost Per MSF " substring(cDisplay,15,300) SKIP.
         IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   "Cost Per MSF " + substring(cExcelDisplay,3,300) SKIP.
        END.

    /*  IF tb_exp-exel THEN
      DO:
         RUN print-excel-total(INPUT "Total Time",
                               INPUT ld-tot-time,
                               INPUT ld-tot-sqft).
         RUN print-excel-total(INPUT "Labor Per Crew Hour",
                               INPUT ld-tot-cost / ld-tot-time,
                               INPUT 0).
         RUN print-excel-total(INPUT "Total Direct Labor",
                               INPUT ld-tot-cost,
                               INPUT 0).
         RUN print-excel-total(INPUT "Cost Per MSF",
                               INPUT ld-tot-cost / ld-tot-sqft * 1000,
                               INPUT 0).
      END.*/
    END.
  END.

SESSION:SET-WAIT-STATE("").

IF v-export THEN DO:
  OUTPUT STREAM s-temp CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(exp-name)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).  

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


