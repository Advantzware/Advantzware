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

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
                                 FIELD due-date LIKE ar-inv.due-date.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Cust#,Cust Name,Invoice#,Inv Date,Due Date,Disc Date," +
                           "Gross1,Disc1,Gross2,Disc2,Gross3,Disc3," +
                           "Beyond Gross,Beyond Disc,Gross,Total Disc"
       cFieldListToSelect = "cust,cst-nam,inv,inv-dt,due-dt,dsc-dt," +
                            "grs1,dsc1,grs2,dsc2,grs3,dsc3," +
                            "bynd-grs,bynd-disc,gros,ttl-disc"
       cFieldLength = "10,30,8,10,10,10," + "11,10,11,10,11,10," + "12,11,16,10"
       cFieldType = "c,c,i,c,c,c," + "i,i,i,i,i,i," + "i,i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Cust#,Cust Name,Invoice#,Inv Date,Due Date," +
                           "Gross1,Disc1,Gross2,Disc2,Gross3,Disc3," +
                           "Beyond Gross,Beyond Disc,Gross,Total Disc"  .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date-1 begin_date-2 ~
begin_date-3 rd_sort rd_age sl_avail Btn_Def sl_selected Btn_Add Btn_Remove ~
btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel fi_file tb_runExcel btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date-1 begin_date-2 begin_date-3 ~
lbl_sort rd_sort lbl_age rd_age sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel fi_file ~
tb_runExcel 

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

DEFINE VARIABLE begin_date-1 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 1" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 2" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-3 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 3" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cshreq.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_age AS CHARACTER FORMAT "X(256)":U INITIAL "Age By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_age AS CHARACTER INITIAL "Due Date" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Due Date", "Due Date",
"Avg Days", "Avg Days"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Name", "Customer Name",
"Due Date", "Due Date"
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

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
     begin_date-1 AT ROW 2.91 COL 39 COLON-ALIGNED HELP
          "Enter First Date"
     begin_date-2 AT ROW 4.1 COL 39 COLON-ALIGNED HELP
          "Enter the Second Date"
     begin_date-3 AT ROW 5.29 COL 39 COLON-ALIGNED HELP
          "Enter the Third Date"
     lbl_sort AT ROW 6.71 COL 25 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 6.76 COL 34 NO-LABEL
     lbl_age AT ROW 7.91 COL 22 COLON-ALIGNED NO-LABEL
     rd_age AT ROW 7.91 COL 34 NO-LABEL
     sl_avail AT ROW 10.29 COL 3.6 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.29 COL 39.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.29 COL 59 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.29 COL 39.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.29 COL 39.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.33 COL 39.6 WIDGET-ID 40
     btn_down AT ROW 14.33 COL 39.6 WIDGET-ID 42
     rd-dest AT ROW 16.95 COL 5 NO-LABEL
     lv-ornt AT ROW 16.95 COL 31 NO-LABEL
     lines-per-page AT ROW 16.95 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 19.1 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 20.05 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.19 COL 31
     tb_excel AT ROW 22.67 COL 51 RIGHT-ALIGNED
     fi_file AT ROW 23.48 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     tb_runExcel AT ROW 22.67 COL 72 RIGHT-ALIGNED
     btn-ok AT ROW 25.24 COL 19
     btn-cancel AT ROW 25.24 COL 58
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.57 COL 59 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.24 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Cash Needed On:":U VIEW-AS TEXT
          SIZE 24 BY 1 AT ROW 1.71 COL 37
          BGCOLOR 8 FGCOLOR 9 FONT 5
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.57 COL 4.4 WIDGET-ID 38
     RECT-6 AT ROW 15.76 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1
         SIZE 94.4 BY 25.91.


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
         TITLE              = "AR Cash Requirements Report"
         HEIGHT             = 26
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_age IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_age:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_age".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

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
ON END-ERROR OF C-Win /* AR Cash Requirements Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AR Cash Requirements Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-1 C-Win
ON LEAVE OF begin_date-1 IN FRAME FRAME-A /* Date 1 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Date 2 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-3 C-Win
ON LEAVE OF begin_date-3 IN FRAME FRAME-A /* Date 3 */
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
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_date-1
                            &END_cust=begin_date-1
                            &fax-subject="Customer List"
                            &fax-body="Customer List"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject="Customer List"
                             &mail-body="Customer List"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= ''
                                  &END_cust=''
                                  &mail-subject="Customer List"
                                  &mail-body="Customer List"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
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


&Scoped-define SELF-NAME rd_age
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_age C-Win
ON VALUE-CHANGED OF rd_age IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
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

  assign
   begin_date-1 = today
   begin_date-2 = begin_date-1 + 7
   begin_date-3 = begin_date-2 + 7.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_date-1.
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
  DISPLAY begin_date-1 begin_date-2 begin_date-3 lbl_sort rd_sort lbl_age rd_age 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel fi_file tb_runExcel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date-1 begin_date-2 begin_date-3 rd_sort rd_age 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         fi_file tb_runExcel btn-ok btn-cancel
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN custom/dprint.w (list-name).

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
/* --------------------------------------------------- ap/ap-flow.p 12/92 cd  */
/*                                                                            */
/* a/p - cash requirements report                                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

DEF VAR ws_gross LIKE ar-inv.net NO-UNDO.
DEF VAR d1 AS DATE EXTENT 3 FORMAT "99/99/9999" NO-UNDO.
DEF VAR ni AS INT NO-UNDO.
DEF VAR cust-t AS DEC EXTENT 4 FORMAT "->>>>>.99" NO-UNDO.
DEF VAR cust-d AS DEC EXTENT 4 FORMAT "->>>>.99" NO-UNDO.
DEF VAR inv-t AS DEC FORMAT "->>>>>>9.99" EXTENT 4 NO-UNDO.
DEF VAR inv-d LIKE cust-d NO-UNDO.
DEF VAR grand-t LIKE cust-t NO-UNDO.
DEF VAR grand-d LIKE cust-d NO-UNDO.
DEF VAR s AS INT NO-UNDO.
DEF VAR ag AS DEC NO-UNDO.
DEF VAR amt LIKE ag NO-UNDO.
DEF VAR t1 AS DEC FORMAT "$->>>,>>>.99" NO-UNDO.
DEF VAR c1 AS DEC FORMAT "$->>>,>>>.99" NO-UNDO.
DEF VAR m1 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR m2 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR m3 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR first-time AS LOG INIT YES NO-UNDO.
DEF VAR ws_disc-avail AS DEC NO-UNDO COLUMN-LABEL "Disc"
    FORMAT '->>>>.99'.
DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-disc AS LOG INIT NO NO-UNDO.
DEF VAR v-disc-date AS DATE NO-UNDO.
DEF VAR li AS INT NO-UNDO.

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
DEF VAR excelheader2 AS CHAR NO-UNDO.

FORM HEADER
     d1[1] TO 47 d1[2] TO 69 d1[3] TO 91 "Beyond" TO 111 "Total" TO 135 SKIP
     "Invoice#  Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc          Gross     Disc"
     SKIP
     FILL("_",135) FORMAT "x(135)"

    WITH PAGE-TOP FRAME f-top STREAM-IO WIDTH 135 NO-BOX.

FORM ar-inv.inv-no
     SPACE(4)
     ar-inv.inv-date        FORMAT "99/99/99"
     tt-report.due-date     FORMAT "99/99/99"
     inv-t[1]
     inv-d[1]               FORMAT "->>>>.99" 
     inv-t[2]
     inv-d[2]               FORMAT "->>>>.99" 
     inv-t[3]
     inv-d[3]               FORMAT "->>>>.99" 
     inv-t[4]
     inv-d[4]               FORMAT "->>>>.99" 
     ws_gross
     ws_disc-avail

    WITH STREAM-IO WIDTH 135 FRAME a NO-LABELS DOWN NO-BOX.


EMPTY TEMP-TABLE tt-report.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 d1[1]  = begin_date-1
 d1[2]  = begin_date-2
 d1[3]  = begin_date-3
 v-sort = SUBSTR(rd_sort,1,1)
 /*v-disc = tb_disc-date */ . 


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



        IF ttRptSelected.TextList EQ "Gross1" OR ttRptSelected.TextList EQ "Disc1" THEN
         ASSIGN
            str-tit3 = str-tit3 + string(string(d1[1]),"x(11)") + " " 
            excelheader2 = excelHeader2 + string(d1[1]) + "," .
        ELSE IF ttRptSelected.TextList EQ "Gross2" OR ttRptSelected.TextList EQ "Disc2" THEN
         ASSIGN
            str-tit3 = str-tit3 + string(string(d1[2]),"x(11)") + " " 
            excelheader2 = excelHeader2 + string(d1[2]) + "," . 
        ELSE IF ttRptSelected.TextList EQ "Gross3" OR ttRptSelected.TextList EQ "Disc3" THEN
         ASSIGN
            str-tit3 = str-tit3 + string(string(d1[3]),"x(11)") + " " 
            excelheader2 = excelHeader2 + string(d1[3]) + "," .
        ELSE
            ASSIGN
                str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " " 
                excelheader2 = excelHeader2 + "," .

 END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
/*  excelheader = "Cust#,Cust. Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc". */
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader2,',','","') '"' SKIP.
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH ar-inv
      WHERE ar-inv.company EQ cocode
        AND ar-inv.posted  EQ YES
        AND ar-inv.due     NE 0
      NO-LOCK,

      FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq ar-inv.cust-no
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    CREATE tt-report.
    ASSIGN
     tt-report.due-date = IF rd_age EQ "Due Date" THEN ar-inv.due-date
                          ELSE (ar-inv.inv-date + cust.avg-pay)
     tt-report.key-01   = IF v-sort EQ "N" THEN cust.name
                          ELSE
                          IF v-sort EQ "D" THEN STRING((YEAR(tt-report.due-date) * 10000) +
                                                       (MONTH(tt-report.due-date) * 100)  +
                                                       DAY(tt-report.due-date))
                          ELSE ""
     tt-report.key-02  = cust.cust-no
     tt-report.key-03  = STRING(ar-inv.inv-no,"9999999999")
     tt-report.rec-id  = RECID(ar-inv).
  END.

  DISPLAY "" WITH FRAME r-top.
  /*DISPLAY "" WITH FRAME f-top. */

  FOR EACH tt-report,
      FIRST ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK

      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03:

      {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-report.key-02) "}

    FIND FIRST terms WHERE terms.t-code EQ ar-inv.terms NO-LOCK NO-ERROR.

    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK NO-ERROR.

    IF FIRST-OF(tt-report.key-02) THEN DO:
    /*  PUT ar-inv.cust-no.
      IF AVAIL cust THEN PUT cust.name.
      PUT SKIP. */
    END.

    ASSIGN
     ws_gross      = ar-inv.due
     ws_disc-avail = IF ar-inv.net NE 0 THEN
                       (ar-inv.net * (ar-inv.disc-% / 100) - ar-inv.disc-taken)
                     ELSE 0.

    DO li = 1 to 4:
      ASSIGN
       inv-t[li] = 0
       inv-d[li] = 0.
    END.

    IF tt-report.due-date GT d1[3] THEN
      ASSIGN
       cust-t[4] = cust-t[4] + ws_gross
       inv-t[4]  = ws_gross.

    ELSE
    IF tt-report.due-date GT d1[2] THEN
      ASSIGN
       cust-t[3] = cust-t[3] + ws_gross
       inv-t[3]  = ws_gross.

    ELSE
    IF tt-report.due-date GT d1[1] THEN
      ASSIGN
       cust-t[2] = cust-t[2] + ws_gross
       inv-t[2]  = ws_gross.

    ELSE
      ASSIGN
       cust-t[1] = cust-t[1] + ws_gross
       inv-t[1]  = ws_gross.

    v-disc-date = IF AVAIL terms THEN (ar-inv.inv-date + terms.disc-days)
                                 ELSE tt-report.due-date.

    IF v-disc-date GT d1[3] THEN
      ASSIGN
       cust-d[4] = cust-d[4] + ws_disc-avail
       inv-d[4]  = ws_disc-avail.

    ELSE
    IF v-disc-date GT d1[2] THEN
      ASSIGN
       cust-d[3] = cust-d[3] + ws_disc-avail
       inv-d[3]  = ws_disc-avail.

    ELSE
    IF v-disc-date GT d1[1] THEN
      ASSIGN
       cust-d[2] = cust-d[2] + ws_disc-avail
       inv-d[2]  = ws_disc-avail.

    ELSE
      ASSIGN
       cust-d[1] = cust-d[1] + ws_disc-avail
       inv-d[1]  = ws_disc-avail.

  /*  DISPLAY ar-inv.inv-no
            ar-inv.inv-date
            tt-report.due-date
              ar-inv.inv-date + ar-inv.disc-days
                WHEN ws_disc-avail NE 0 AND v-disc @ tt-report.due-date
            inv-t[1] WHEN inv-t[1] NE 0
            inv-d[1] WHEN inv-d[1] NE 0
            inv-t[2] WHEN inv-t[2] NE 0
            inv-d[2] WHEN inv-d[2] NE 0
            inv-t[3] WHEN inv-t[3] NE 0
            inv-d[3] WHEN inv-d[3] NE 0
            inv-t[4] WHEN inv-t[4] NE 0
            inv-d[4] WHEN inv-d[4] NE 0
            ws_gross
            ws_disc-avail

        WITH FRAME a.
    DOWN WITH FRAME a.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' IF FIRST-OF(tt-report.key-02) THEN ar-inv.cust-no
               ELSE ""                                             '",'
           '"' IF FIRST-OF(tt-report.key-02) AND AVAIL cust THEN
                  cust.NAME ELSE ""                                '",'
           '"' ar-inv.inv-no                                       '",'
           '"' ar-inv.inv-date                                     '",'
           '"' IF ws_disc-avail NE 0 AND v-disc THEN
                  STRING(ar-inv.inv-date + ar-inv.disc-days)
               ELSE STRING(tt-report.due-date)                     '",'
           '"' IF inv-t[1] NE 0 THEN STRING(inv-t[1],"->>>>>>9.99")
                  ELSE ""                                          '",'   
           '"' IF inv-d[1] NE 0 THEN STRING(inv-d[1],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[2] NE 0 THEN STRING(inv-t[2],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[2] NE 0 THEN STRING(inv-d[2],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[3] NE 0 THEN STRING(inv-t[3],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[3] NE 0 THEN STRING(inv-d[3],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[4] NE 0 THEN STRING(inv-t[4],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[4] NE 0 THEN STRING(inv-d[4],"->>>>.99")
                  ELSE ""                                          '",'
           '"' STRING(ws_gross,"->>,>>>,>>9.99")                   '",'
           '"' STRING(ws_disc-avail,'->>>>.99')                    '",'
           SKIP. */

      ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "cust"       THEN cVarValue = IF FIRST-OF(tt-report.key-02) THEN ar-inv.cust-no ELSE "" .
                  WHEN "cst-nam"    THEN cVarValue = IF FIRST-OF(tt-report.key-02) AND AVAIL cust THEN cust.NAME ELSE "" .
                  WHEN "inv"        THEN cVarValue = STRING(ar-inv.inv-no) .
                  WHEN "inv-dt"     THEN cVarValue = STRING(ar-inv.inv-date) .
                  WHEN "due-dt"     THEN cVarValue = STRING(tt-report.due-date) .
                  WHEN "dsc-dt"     THEN cVarValue = STRING(ar-inv.inv-date + ar-inv.disc-days) .
                  WHEN "grs1"       THEN cVarValue = IF inv-t[1] NE 0 THEN STRING(inv-t[1],"->>>>>>9.99") ELSE ""  .
                  WHEN "dsc1"       THEN cVarValue = IF inv-d[1] NE 0 THEN STRING(inv-d[1],"->>>>>>.99") ELSE "" .
                  WHEN "grs2"       THEN cVarValue = IF inv-t[2] NE 0 THEN STRING(inv-t[2],"->>>>>>9.99") ELSE "" .
                  WHEN "dsc2"       THEN cVarValue = IF inv-d[2] NE 0 THEN STRING(inv-d[2],"->>>>>>.99") ELSE "" . 
                  WHEN "grs3"       THEN cVarValue = IF inv-t[3] NE 0 THEN STRING(inv-t[3],"->>>>>>9.99") ELSE "" .
                  WHEN "dsc3"       THEN cVarValue = IF inv-d[3] NE 0 THEN STRING(inv-d[3],"->>>>>>.99") ELSE "" .
                  WHEN "bynd-grs"   THEN cVarValue = IF inv-t[4] NE 0 THEN STRING(inv-t[4],"->>>>>>>>9.99") ELSE "" .
                  WHEN "bynd-disc"  THEN cVarValue = IF inv-d[4] NE 0 THEN STRING(inv-d[4],"->>>>>>.99") ELSE "" .
                  WHEN "gros"       THEN cVarValue = STRING(ws_gross,"->>>>,>>>,>>9.99") .  
                  WHEN "ttl-disc"   THEN cVarValue = STRING(ws_disc-avail,'->>>>>>.99') .  

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

    IF LAST-OF(tt-report.key-02) THEN DO:
     /* DISPLAY "       *" @ tt-report.due-date
              cust-t[1]  @ inv-t[1]
              cust-d[1]  @ inv-d[1]
              cust-t[2]  @ inv-t[2]
              cust-d[2]  @ inv-d[2]
              cust-t[3]  @ inv-t[3]
              cust-d[3]  @ inv-d[3]
              cust-t[4]  @ inv-t[4]
              cust-d[4]  @ inv-d[4]
              cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4] @ ws_gross
              cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4] @ ws_disc-avail

          WITH FRAME a.
      DOWN 2 WITH FRAME a.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' "*"                                 '",'
             '"' STRING(cust-t[1],"->>>>>.99")     '",'   
             '"' STRING(cust-d[1],"->>>>.99")        '",'
             '"' STRING(cust-t[2],"->>>>>.99")     '",'
             '"' STRING(cust-d[2],"->>>>.99")        '",'
             '"' STRING(cust-t[3],"->>>>>.99")     '",'
             '"' STRING(cust-d[3],"->>>>.99")        '",'
             '"' STRING(cust-t[4],"->>>>>.99")     '",'
             '"' STRING(cust-d[4],"->>>>.99")        '",'
             '"' STRING(cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4],"->>,>>>,>>9.99")   '",'
             '"' STRING(cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4],'->>>>.99')    '",'
             SKIP(1). */

      /*  ASSIGN 
           ttl-grs = STRING(cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4]
             '"' STRING(cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4],'->>>>.99')   */

         ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "cust"       THEN cVarValue = "" .
                  WHEN "cst-nam"    THEN cVarValue = "" .
                  WHEN "inv"        THEN cVarValue = "" .   
                  WHEN "inv-dt"     THEN cVarValue = "" .   
                  WHEN "due-dt"     THEN cVarValue = "" .   
                  WHEN "dsc-dt"     THEN cVarValue = "" .   
                  WHEN "grs1"       THEN cVarValue = STRING(cust-t[1],"->>>>>.99") .
                  WHEN "dsc1"       THEN cVarValue = STRING(cust-d[1],"->>>>.99")  .
                  WHEN "grs2"       THEN cVarValue = STRING(cust-t[2],"->>>>>.99").
                  WHEN "dsc2"       THEN cVarValue = STRING(cust-d[2],"->>>>.99") .
                  WHEN "grs3"       THEN cVarValue = STRING(cust-t[3],"->>>>>.99").
                  WHEN "dsc3"       THEN cVarValue = STRING(cust-d[3],"->>>>.99"). 
                  WHEN "bynd-grs"   THEN cVarValue = STRING(cust-t[4],"->>>>>.99").
                  WHEN "bynd-disc"  THEN cVarValue = STRING(cust-d[4],"->>>>.99") .
                  WHEN "gros"       THEN cVarValue = STRING(cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4],"->>,>>>,>>9.99") .  
                  WHEN "ttl-disc"   THEN cVarValue = STRING(cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4],'->>>>.99') .  

             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
     END.
     PUT UNFORMATTED  "       *" substring(cDisplay,7,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "  *  " + substring(cExcelDisplay,3,300) SKIP.
     END.

      DO li = 1 TO 4:
        ASSIGN
         grand-t[li] = grand-t[li] + cust-t[li]
         grand-d[li] = grand-d[li] + cust-d[li]

         cust-t[li]  = 0
         cust-d[li]  = 0.
      END.
    END.  /* last-of loop */

    IF LAST(tt-report.key-02) THEN DO:
     /* DOWN 1 WITH FRAME a.

      DISPLAY "      **" @ tt-report.due-date
              grand-t[1] @ inv-t[1]
              grand-d[1] @ inv-d[1]
              grand-t[2] @ inv-t[2]
              grand-d[2] @ inv-d[2]
              grand-t[3] @ inv-t[3]
              grand-d[3] @ inv-d[3]
              grand-t[4] @ inv-t[4]
              grand-d[4] @ inv-d[4]
              grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
              grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail

          WITH FRAME a.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' "**"                                                '",'
             '"' IF grand-t[1] NE 0 THEN STRING(grand-t[1],"->>>>>>9.99")
                    ELSE ""                                          '",'   
             '"' IF grand-d[1] NE 0 THEN STRING(grand-d[1],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[2] NE 0 THEN STRING(grand-t[2],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[2] NE 0 THEN STRING(grand-d[2],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[3] NE 0 THEN STRING(grand-t[3],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[3] NE 0 THEN STRING(grand-d[3],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[4] NE 0 THEN STRING(grand-t[4],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[4] NE 0 THEN STRING(grand-d[4],"->>>>.99")
                    ELSE ""                                          '",'
             '"' STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->>,>>>,>>9.99")                   '",'
             '"' STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>.99')                    '",'
             SKIP. */
        ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "cust"       THEN cVarValue = "" .
                  WHEN "cst-nam"    THEN cVarValue = "" .
                  WHEN "inv"        THEN cVarValue = "" .   
                  WHEN "inv-dt"     THEN cVarValue = "" .   
                  WHEN "due-dt"     THEN cVarValue = "" .   
                  WHEN "dsc-dt"     THEN cVarValue = "" .   
                  WHEN "grs1"       THEN cVarValue = IF grand-t[1] NE 0 THEN STRING(grand-t[1],"->>>>>>9.99") ELSE ""  .
                  WHEN "dsc1"       THEN cVarValue = IF grand-d[1] NE 0 THEN STRING(grand-d[1],"->>>>>>.99") ELSE ""  .
                  WHEN "grs2"       THEN cVarValue = IF grand-t[2] NE 0 THEN STRING(grand-t[2],"->>>>>>9.99") ELSE ""  .
                  WHEN "dsc2"       THEN cVarValue = IF grand-d[2] NE 0 THEN STRING(grand-d[2],"->>>>>>.99") ELSE ""  .
                  WHEN "grs3"       THEN cVarValue = IF grand-t[3] NE 0 THEN STRING(grand-t[3],"->>>>>>9.99") ELSE ""  .
                  WHEN "dsc3"       THEN cVarValue = IF grand-d[3] NE 0 THEN STRING(grand-d[3],"->>>>>>.99") ELSE ""  .
                  WHEN "bynd-grs"   THEN cVarValue = IF grand-t[4] NE 0 THEN STRING(grand-t[4],"->>>>>>>9.99") ELSE ""  .
                  WHEN "bynd-disc"  THEN cVarValue = IF grand-d[4] NE 0 THEN STRING(grand-d[4],"->>>>>>.99") ELSE ""  .
                  WHEN "gros"       THEN cVarValue = STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->>>>,>>>,>>9.99")  .  
                  WHEN "ttl-disc"   THEN cVarValue = STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>>>.99') .  

             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
     END.
     PUT UNFORMATTED  "      **" substring(cDisplay,9,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "  **  " + substring(cExcelDisplay,3,300) SKIP.
     END.
    END.
  END. /* for EACH */

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

