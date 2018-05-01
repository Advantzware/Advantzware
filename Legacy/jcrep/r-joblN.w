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


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Job Number,Job2,Customer,Cust Name,FG Item,Estimate,Order#,Start Date,Close Date," + 
                           "Status,Cust Part #,Job Qty,Ordered Qty,Prod Qty,On Hand Qty,Shipped Qty,Invoice Qty," + 
                           "WIP Qty,Overage %,Material $,Direct Labor$,Fixed OH$,Var OH$,Total Cost$" 

       cFieldListToSelect = "job,job2,cust,cust-name,fg-item,est-no,order,start-date,close-date," + /*9*/
                            "status,cust-part,job-qty,ord-qty,prod-qty,onhand-qty,ship-qty,inv-qty," +     /*8*/
                            "wip-qty,overage,material,dir-labor,fixed-oh,var-oh,total-cost" /*7*/

       cFieldLength = "10,4,8,30,15,8,6,10,10," + "6,15,12,11,11,11,11,11," + "11,11,11,13,11,11,13"  
         cFieldType = "c,c,c,c,c,c,c,c,c," + "c,c,i,i,i,i,i,i," + "i,i,i,i,i,i,i" .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Job Number,Job2,Customer,Cust Name,FG Item,Estimate,Order#,Start Date,Close Date," + 
                           "Status,Cust Part #,Job Qty,Ordered Qty,Prod Qty,On Hand Qty,Shipped Qty,Invoice Qty," + 
                           "WIP Qty,Overage %,Material $,Direct Labor$,Fixed OH$,Var OH$,Total Cost$" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_date end_date rsQty sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_date end_date rsQty sl_avail sl_selected lv-ornt ~
lines-per-page rd-dest lv-font-no lv-font-name tb_excel tb_runExcel fi_file ~
td-show-parm 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-total-cost C-Win 
FUNCTION get-total-cost RETURNS DECIMAL
    ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD orderQty C-Win 
FUNCTION orderQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD producedQty C-Win 
FUNCTION producedQty RETURNS INTEGER
  ( /*OUTPUT opBalance AS INTEGER */)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shipQty C-Win 
FUNCTION shipQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD invoiceQty C-Win 
FUNCTION invoiceQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wipQty C-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD overUnderPct C-Win 
FUNCTION overUnderPct RETURNS INTEGER
  (ipBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD onHandQty C-Win 
FUNCTION onHandQty RETURNS INTEGER
  (/*OUTPUT opQtyOnHand AS INTEGER*/)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD custPart C-Win 
FUNCTION custPart RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-i-name C-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE rsQty AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job Details", "J",
"Routing Details", "R",
"Both", "B"
     SIZE 56 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.91.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 2.71 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 2
     begin_job-no2 AT ROW 2.71 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 4
     end_job-no AT ROW 2.71 COL 64 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 6
     end_job-no2 AT ROW 2.71 COL 77 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 8
     begin_date AT ROW 3.76 COL 26 COLON-ALIGNED
     end_date AT ROW 3.76 COL 64 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     rsQty AT ROW 5.52 COL 28 NO-LABEL WIDGET-ID 10
     sl_avail AT ROW 8.91 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 8.91 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 8.91 COL 58.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.91 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.91 COL 39 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11.95 COL 39 WIDGET-ID 40
     btn_down AT ROW 12.95 COL 39 WIDGET-ID 42
     lv-ornt AT ROW 14.48 COL 33 NO-LABEL
     lines-per-page AT ROW 14.48 COL 86 COLON-ALIGNED
     rd-dest AT ROW 15.43 COL 8 NO-LABEL
     lv-font-no AT ROW 15.91 COL 37 COLON-ALIGNED
     lv-font-name AT ROW 17.1 COL 31 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 18.52 COL 70 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.52 COL 92 RIGHT-ALIGNED
     fi_file AT ROW 19.48 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     td-show-parm AT ROW 19.71 COL 7
     btn-ok AT ROW 21.57 COL 27
     btn-cancel AT ROW 21.57 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.19 COL 3.8 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 14.33 COL 5
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.19 COL 58.4 WIDGET-ID 44
     RECT-6 AT ROW 14.24 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.1
         SIZE 94.4 BY 22.48.


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
         TITLE              = "Job Production Detail Report "
         HEIGHT             = 23
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

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
  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
  END CASE. 
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

  ASSIGN
   begin_date  = DATE (1,1,YEAR(TODAY))
   end_date    = DATE (12,31,year(TODAY)).
  RUN DisplaySelectionList.
  RUN enable_UI.
 

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date 
          rsQty sl_avail sl_selected lv-ornt lines-per-page rd-dest lv-font-no 
          lv-font-name tb_excel tb_runExcel fi_file td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
         begin_date end_date rsQty sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel 
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
DEFINE VARIABLE excelheader2 AS CHARACTER  NO-UNDO.

excelheader2 = "Job number,Job2,Side,Blank,Pass,Machine,"
               + "Item Name,Lag Time,Dept,MR Hrs,MRWaste,RunHrs,Speed,Waste%,Run Qty" .

form job-hdr.job-no       column-label "Job Number"
     job-hdr.job-no2      column-label " "
     job-mch.frm          column-label "Side"               
     job-mch.blank-no     COLUMN-LABEL "Blank"
     job-mch.pass         COLUMN-LABEL "Pass"   
     job-mch.m-code       COLUMN-LABEL "Machine"
     job-mch.i-name        COLUMN-LABEL "Item Name"
     job-mch.lag-time           COLUMN-LABEL "Lag!Time"
     job-mch.dept             COLUMN-LABEL "Dept"
     job-mch.mr-hr              COLUMN-LABEL "MRHrs"
     job-mch.mr-waste           COLUMN-LABEL "MRWst"
     job-mch.run-hr           COLUMN-LABEL "RunHrs"
     job-mch.speed             COLUMN-LABEL "Speed"
     job-mch.wst-prct              COLUMN-LABEL "Wst %"
     job-mch.run-qty           COLUMN-LABEL "Run Qty"
   with no-box frame itemx down STREAM-IO width 232.
              

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112} .




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



FOR EACH job-hdr NO-LOCK
      WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no GE begin_job-no
        AND job-hdr.job-no LE END_job-no
       
      USE-INDEX job-no,

      FIRST job NO-LOCK
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
        AND job.start-date GE begin_date
        AND job.start-date LE end_date
      USE-INDEX job:

      {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    
          FIND FIRST cust NO-LOCK
           WHERE cust.company EQ cocode
             AND cust.cust-no EQ job-hdr.cust-no NO-ERROR .
   
      IF rsQty = "J" OR rsQty EQ "B" THEN DO: 

        
       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"    THEN cVarValue = STRING(job-hdr.job-no) .
                         WHEN "job2"   THEN cVarValue = STRING(job.job-no2,"99").
                         WHEN "cust"   THEN cVarValue = STRING(job-hdr.cust-no) .
                         WHEN "cust-name"  THEN cVarValue = IF AVAIL cust THEN string(cust.NAME) ELSE "" .
                         WHEN "fg-item"  THEN cVarValue = STRING(job-hdr.i-no,"x(15)") .
                         WHEN "est-no"   THEN cVarValue =  STRING(job.est-no) .
                         WHEN "order"  THEN cVarValue = STRING(job-hdr.ord-no,">>>>>9") .
                         WHEN "start-date"   THEN cVarValue = IF job.start-date NE ? THEN STRING(job.start-date) ELSE "" .
                         WHEN "close-date"  THEN cVarValue = IF job.close-date NE ? THEN STRING(job.close-date) ELSE "" .

                         WHEN "status"    THEN cVarValue = STRING(job.stat) .
                         WHEN "cust-part"   THEN cVarValue =  STRING(custpart()) .
                         WHEN "job-qty"   THEN cVarValue = STRING(job-hdr.qty,"->>>,>>>,>>9").
                         WHEN "ord-qty"  THEN cVarValue = STRING(orderQty(),"->>,>>>,>>>") .
                         WHEN "prod-qty"   THEN cVarValue = STRING(producedQty(),"->>,>>>,>>>") .
                         WHEN "onhand-qty"  THEN cVarValue = STRING(onHandQty(),"->>,>>>,>>>").
                         WHEN "ship-qty"   THEN cVarValue = STRING(shipQty(),"->>,>>>,>>>") .
                         WHEN "inv-qty"    THEN cVarValue = STRING(invoiceQty(),"->>,>>>,>>>") .
                         WHEN "wip-qty"   THEN cVarValue = STRING(wipQty(),"->>,>>>,>>>").
                         WHEN "overage"  THEN cVarValue = STRING(overUnderPct(onHandQty()),"->>>>>%")  .
                         WHEN "material"   THEN cVarValue =  STRING(job-hdr.std-mat-cost,"->>>,>>9.99<<") .
                         WHEN "dir-labor"  THEN cVarValue =  STRING(job-hdr.std-lab-cost,"->,>>>,>>9.99<<") .
                         WHEN "fixed-oh"   THEN cVarValue =  STRING(job-hdr.std-fix-cost,"->>>,>>9.99<<")  .
                         WHEN "var-oh"  THEN cVarValue =  STRING(job-hdr.std-var-cost,"->>>,>>9.99<<")  .
                         WHEN "total-cost"  THEN cVarValue = STRING(get-total-cost(),"->>,>>>,>>9.99<<") .


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
      END.

      IF rsQty = "R" OR rsQty EQ "B"  THEN DO: 
 
          FOR EACH job-mch WHERE job-mch.company = job.company 
              AND job-mch.job = job.job 
              AND job-mch.job-no = job.job-no 
              AND job-mch.job-no2 = job.job-no2 
              use-index line-idx NO-LOCK BREAK BY job-mch.m-code :

              IF FIRST(job-mch.m-code) THEN do:

                  PUT SKIP(1)
                      "Job number   Side  Blank  Pass   Machine " 
                      "Item Name         Lag Time Dept  MR Hrs MRWaste  RunHrs    Speed    Waste%    Run Qty" SKIP
                      "----------------------------------------------------------------------------------"
                      "--------------------------------------------" SKIP .
                  IF tb_excel THEN
                      PUT STREAM st-excell UNFORMATTED  SKIP(1) '"' REPLACE(excelheader2,',','","') '"' SKIP.
                  
              END.

              display 
                  job-hdr.job-no  FORMAT "x(6)"
                  job-hdr.job-no2 FORMAT "99" SPACE (2)
                  job-mch.frm     FORMAT ">>>>>"  SPACE(2)
                  job-mch.blank-no FORMAT ">>>>>" SPACE(2)
                  job-mch.pass    FORMAT ">>>>>" SPACE(2)
                  job-mch.m-code  FORMAT "x(8)" SPACE(1)
                  display-i-name()  FORMAT "x(20)" SPACE(1)
                  job-mch.lag-time FORMAT ">>>>9"  SPACE(1) 
                  job-mch.dept FORMAT "x(5)"   SPACE(1)
                  job-mch.mr-hr  FORMAT ">>9.99"  SPACE(1)
                  job-mch.mr-waste FORMAT ">>>9" SPACE(1)
                  job-mch.run-hr FORMAT ">>>,>>9.99" SPACE(1)
                  job-mch.speed  FORMAT ">,>>>,>>9" SPACE(1)
                  job-mch.wst-prct FORMAT ">>9.99" SPACE(1)
                  job-mch.run-qty FORMAT ">>>,>>>,>>9" SPACE(1)
                   with frame itemx1 NO-BOX NO-LABEL down stream-io width 230.

              IF tb_excel THEN
               PUT STREAM st-excell UNFORMATTED
                   '"' job-hdr.job-no                           '",'
                   '"' job-hdr.job-no2                          '",'
                   '"' job-mch.frm                              '",'
                   '"' job-mch.blank-no                         '",'
                   '"' job-mch.pass                             '",'
                   '"' job-mch.m-code                           '",'
                   '"' job-mch.i-name                           '",'
                   '"' job-mch.lag-time                         '",'
                   '"' job-mch.dept                             '",'
                   '"' job-mch.mr-hr                            '",'
                   '"' job-mch.mr-waste                         '",'
                   '"' job-mch.run-hr                           '",'
                   '"' job-mch.speed                           '",'
                   '"' job-mch.wst-prct                         '",'
                   '"' job-mch.run-qty                        '",'
                   SKIP.

              IF LAST(job-mch.m-code) THEN do:

                  PUT SKIP(1) .
                  IF tb_excel THEN
                     PUT STREAM st-excell UNFORMATTED  SKIP(1) .
              END.



          END. /* for each job-mch*/
      

          
       END.  /*  rsQty = "R"*/
      
  END.  /* for each mch-act */



  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-total-cost B-table-Win 
FUNCTION get-total-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dTotalCost AS DECIMAL FORMAT "->,>>>,>>>,>>>.99<<" NO-UNDO INIT 0.

  IF AVAIL job-hdr THEN
    ASSIGN dTotalCost = job-hdr.std-mat-cost + job-hdr.std-lab-cost + job-hdr.std-fix-cost + job-hdr.std-var-cost.

  RETURN dTotalCost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION orderQty B-table-Win 
FUNCTION orderQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.qty.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION producedQty B-table-Win 
FUNCTION producedQty RETURNS INTEGER
  ( /*OUTPUT opBalance AS INTEGER */) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE cJobNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iJobNo2 AS INTEGER NO-UNDO.
  DEFINE VARIABLE cINo AS CHARACTER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    ASSIGN 
        cINo    = job-hdr.i-no
        cJobNo  = job-hdr.job-no
        iJobNo2 = job-hdr.job-no2
        .
/*     FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company          */
/*                                  AND oe-ordl.i-no EQ job-hdr.i-no                */
/*                                  AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.  */
/*     IF AVAILABLE oe-ordl THEN                                                    */
/*     DO:                                                                          */
/*        cINo = oe-ordl.i-no.                                                      */
/*        IF oe-ordl.job-no NE '' THEN                                              */
/*            ASSIGN                                                                */
/*                 cJobNo = oe-ordl.job-no                                          */
/*                 iJobNo2 = oe-ordl.job-no2                                        */
/*                 .                                                                */
/*           FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK   */
/*              WHERE fg-rcpth.company EQ oe-ordl.company       */
/*                AND fg-rcpth.job-no EQ oe-ordl.job-no         */
/*                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2       */
/*                AND fg-rcpth.i-no EQ oe-ordl.i-no             */
/*                AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,  */
/*               EACH fg-rdtlh FIELDS(qty) NO-LOCK              */
/*              WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no            */
/*                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
/*               rtnValue = rtnValue + fg-rdtlh.qty.            */
/*        END. */
/*       ELSE */
/*          FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
/*              WHERE fg-rcpth.company   EQ cocode             */
/*                AND fg-rcpth.job-no    EQ job-hdr.job-no     */
/*                AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
/*                AND fg-rcpth.i-no      EQ oe-ordl.i-no       */
/*                AND fg-rcpth.rita-code EQ "R"                */
/*                USE-INDEX job,                               */
/*              EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE        */
/*                   fg-rdtlh.r-no      EQ fg-rcpth.r-no AND   */
/*                   fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
/*                   rtnValue = rtnValue + fg-rdtlh.qty.       */
/*          END.                                               */
/*     END. /* avail oe-ordl */ */
/*     ELSE DO: */
/*                                                            */
/*         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
/*             WHERE fg-rcpth.company   EQ cocode             */
/*               AND fg-rcpth.job-no    EQ job-hdr.job-no     */
/*               AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
/*               AND fg-rcpth.i-no      EQ job-hdr.i-no       */
/*               AND fg-rcpth.rita-code EQ "R"                */
/*               USE-INDEX job,                               */
/*             EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE        */
/*                  fg-rdtlh.r-no      EQ fg-rcpth.r-no AND   */
/*                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
/*                  rtnValue = rtnValue + fg-rdtlh.qty.       */
/*         END.                                               */
/*                                                            */
/*     END. */
    RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                                INPUT cJobNo,
                                INPUT iJobNo2,
                                INPUT cINo,
                                INPUT NO,
                                OUTPUT rtnValue).
  END. /* avail job-hdr */
  /*opBalance = rtnValue.*/
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shipQty B-table-Win 
FUNCTION shipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
  DEFINE VARIABLE li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                         OUTPUT li-inv-qty, OUTPUT li-ship-qty).

      rtnValue = li-ship-qty.
    END.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION invoiceQty B-table-Win 
FUNCTION invoiceQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.inv-qty.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wipQty B-table-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE qtyOnHand AS INTEGER NO-UNDO .
  IF AVAILABLE job-hdr THEN DO:

    FOR EACH fg-bin FIELDS(qty) NO-LOCK
        WHERE fg-bin.company EQ job-hdr.company
          AND fg-bin.job-no EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
          AND fg-bin.i-no EQ job-hdr.i-no:
      rtnValue = rtnValue + fg-bin.qty.
    END. /* each fg-bin */

    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
      IF rtnValue LT 0 OR
         rtnValue LT oe-ordl.qty *
                     (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
        rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION overUnderPct B-table-Win 
FUNCTION overUnderPct RETURNS INTEGER
  (ipBalance AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
    rtnValue = ((ipBalance / oe-ordl.qty) - 1) * 100.
    IF rtnValue EQ 0 THEN rtnValue = 100.
    IF rtnValue EQ -100 THEN rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION onHandQty B-table-Win 
FUNCTION onHandQty RETURNS INTEGER
  (/*OUTPUT opQtyOnHand AS INTEGER*/) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FOR EACH fg-bin FIELDS(qty) NO-LOCK
        WHERE fg-bin.company EQ job-hdr.company
          AND fg-bin.job-no EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
          AND fg-bin.i-no EQ job-hdr.i-no:
      rtnValue = rtnValue + fg-bin.qty.
    END. /* each fg-bin */
  END. /* avail job-hdr */
  
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION custPart B-table-Win 
FUNCTION custPart RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RELEASE itemfg.

  IF AVAIL job-hdr THEN
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no
      NO-ERROR.

  RETURN IF AVAIL itemfg THEN itemfg.part-no ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-nam LIKE job-mch.i-name NO-UNDO.
  DEF VAR lv-frm LIKE job-mch.frm NO-UNDO.
  DEF VAR lv-blk LIKE job-mch.blank-no NO-UNDO.
  


  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL job-mch THEN
      ASSIGN
       lv-nam = job-mch.i-name
       lv-frm = job-mch.frm
       lv-blk = job-mch.blank-no.
    
    IF lv-nam EQ "" THEN DO:
      FIND job-hdr
          WHERE job-hdr.company   EQ job.company
            AND job-hdr.job       EQ job.job
            AND job-hdr.job-no    EQ job.job-no
            AND job-hdr.job-no2   EQ job.job-no2
            AND job-hdr.frm       EQ lv-frm
            AND (job-hdr.blank-no EQ lv-blk OR lv-blk EQ 0)
          NO-LOCK NO-ERROR.

      RELEASE itemfg.
      IF AVAIL job-hdr THEN
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN do:
          lv-nam = itemfg.i-name.
          IF lv-nam EQ "" THEN DO:
             FOR FIRST fg-set WHERE fg-set.company EQ itemfg.company
                                 AND fg-set.set-no = itemfg.i-no
                               NO-LOCK,
                FIRST itemfg WHERE itemfg.i-no = fg-set.part-no
                               AND itemfg.i-name GT ""
                                   NO-LOCK .
                  lv-nam = itemfg.i-name.
             END.
          END.
      END.
    END.
    IF lv-nam EQ "" AND avail(job-mch) THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-mch.company
            AND itemfg.i-no    EQ job-mch.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
          lv-nam = itemfg.i-name.
          IF lv-nam EQ "" THEN DO:

             FOR EACH fg-set WHERE fg-set.company EQ itemfg.company
                                 AND fg-set.set-no = itemfg.i-no
                               NO-LOCK.
                              
                 FIND FIRST itemfg WHERE itemfg.i-no = fg-set.part-no
                                   NO-LOCK NO-ERROR.
                 IF AVAIL itemfg AND itemfg.i-name GT "" THEN DO:
                     lv-nam = itemfg.i-name.
                     LEAVE.
                 END.
             END.
          END.
      END.

    END.

  END.
  RETURN lv-nam.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
