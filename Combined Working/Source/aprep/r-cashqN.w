&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-cashrq.w

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

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Vendor,Vendor Name,Invoice#,Inv Date,Due Date,1Gross,1Disc,2Gross,2Disc," +
			   "3Gross,3Disc,Old Gross,Old Disc,Total Gross,Total Disc,Company,Terms,Days Old,CC/ACH"
       cFieldListToSelect = "ap-inv.vend-no,vend-name,ap-inv.inv-no,ap-inv.inv-date,due-date,inv-t1,inv-d1,inv-t2,inv-d2," +
                            "inv-t3,inv-d3,inv-t4,inv-d4,ws_gross,ws_disc-avail,ap-inv.company,terms,dy-old,cc-ach"
       cFieldLength = "8,30,12,10,10,11,8,11,8," +
                      "11,8,11,8,13,10,7,5,8,6"
       cFieldType   = "c,c,c,c,c,i,i,i,i," + "i,i,i,i,i,i,c,c,c,c"
       .
ASSIGN cTextListToDefault  = "Vendor,Vendor Name,Invoice#,Inv Date,Due Date,1Gross,1Disc,2Gross,2Disc," +
			     "3Gross,3Disc,Old Gross,Old Disc,Total Gross,Total Disc,Company".
{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_company end_company ~
begin_date-1 tb_sort begin_date-2 tb_disc-date begin_date-3 ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS begin_company end_company begin_date-1 ~
tb_sort begin_date-2 tb_disc-date begin_date-3 sl_avail ~
sl_selected lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

DEFINE VARIABLE begin_company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE end_company AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cashrq.csv" 
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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.88 .

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 4.67 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 4.67 NO-UNDO.

DEFINE VARIABLE tb_disc-date AS LOGICAL INITIAL no 
     LABEL "Print Discount Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

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


DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort By Vendor Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_company AT ROW 2.91 COL 23 COLON-ALIGNED
     end_company AT ROW 2.91 COL 63 COLON-ALIGNED
     begin_date-1 AT ROW 4.33 COL 28.2 COLON-ALIGNED HELP
          "Enter First Date"
     tb_sort AT ROW 4.52 COL 55.4
     begin_date-2 AT ROW 5.52 COL 28.2 COLON-ALIGNED HELP
          "Enter the Second Date"
     tb_disc-date AT ROW 5.57 COL 55.4
     begin_date-3 AT ROW 6.71 COL 28.2 COLON-ALIGNED HELP
          "Enter the Third Date"
     Btn_Def AT ROW 8.57 COL 40.8 HELP
          "Default Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 8.62 COL 2.4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 8.62 COL 60.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.52 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.48 COL 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11.43 COL 40.8 WIDGET-ID 40
     btn_down AT ROW 12.38 COL 40.8 WIDGET-ID 42
     lv-ornt AT ROW 15.1 COL 31 NO-LABEL
     lines-per-page AT ROW 15.1 COL 83 COLON-ALIGNED
     rd-dest AT ROW 15.19 COL 5 NO-LABEL
     lv-font-no AT ROW 17 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.38 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.57 COL 31
     tb_excel AT ROW 20.76 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.76 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 22.05 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-cancel AT ROW 24.24 COL 58.2
     btn-ok AT ROW 24.29 COL 17
     "Cash Needed On:":U VIEW-AS TEXT
          SIZE 24 BY 1 AT ROW 1.71 COL 37
          BGCOLOR 8 FGCOLOR 9 FONT 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 7.95 COL 3 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 7.95 COL 60.6 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.33 COL 2
     RECT-6 AT ROW 14 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.95.


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
         TITLE              = "AP Cash Requirements Report"
         HEIGHT             = 25.19
         WIDTH              = 96
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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_disc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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


ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AP Cash Requirements Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AP Cash Requirements Report */
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

  RUN DisplaySelectionDefault.  /* task 04141407 */ 
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


&Scoped-define SELF-NAME tb_disc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disc-date C-Win
ON VALUE-CHANGED OF tb_disc-date IN FRAME FRAME-A /* Print Discount Date? */
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


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort By Vendor Name? */
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
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.

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
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
  END.
  /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
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
  DISPLAY begin_company end_company begin_date-1 tb_sort begin_date-2 
          tb_disc-date begin_date-3 sl_avail sl_selected lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_company end_company begin_date-1 tb_sort 
         begin_date-2 tb_disc-date begin_date-3 Btn_Def 
         sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-cancel btn-ok 
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
DEF BUFFER bap-inv FOR ap-inv .

{sys/form/r-top5L3.f} 

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
def var ws_gross like ap-inv.net no-undo.
def var b-comp as CHAR format "x(3)" no-undo.
def var e-comp as CHAR format "x(3)" no-undo.
def var d1 as date extent 3 format "99/99/9999" no-undo.
def var ni as int no-undo.
def var vend-t as dec extent 4 format "->>>>>.99" no-undo.
def var vend-d as dec extent 4 format "->>>>.99" no-undo.
def var inv-t as dec format "->>>>>>9.99" extent 4 no-undo.
def var inv-d like vend-d no-undo.
def var grand-t like vend-t no-undo.
def var grand-d like vend-d no-undo.
def var s as int no-undo.
def var ag as dec no-undo.
def var amt like ag no-undo.
def var t1 as dec format "$->>>,>>>.99" no-undo.
def var c1 as dec format "$->>>,>>>.99" no-undo.
def var m1 as char format "x(20)" no-undo.
def var m2 as char format "x(20)" no-undo.
def var m3 as char format "x(20)" no-undo.
def var first-time as log init yes no-undo.
def var ws_disc-avail as dec no-undo column-label "Disc"
    format '->>>>.99'.
def var v-sort as log init no no-undo.
def var v-disc as log init no no-undo.
def var v-disc-date as date no-undo.
def var v-company as log init no no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR excelheader-2 AS CHAR NO-UNDO.

/*form header
     d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
     "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc         Gross     Disc company"
     skip
     fill("_",145) format "x(145)"

    with page-top frame b-top stream-io width 145 no-box.

form ap-inv.inv-no
     ap-inv.inv-date format "99/99/99"
     ap-inv.due-date format "99/99/99"
     inv-t[1]
     inv-d[1] format "->>>>.99" 
     inv-t[2]
     inv-d[2] format "->>>>.99" 
     inv-t[3]
     inv-d[3] format "->>>>.99" 
     inv-t[4]
     inv-d[4] format "->>>>.99" 
     ws_gross
     ws_disc-avail
     ap-inv.company FORMAT "x(8)"

    with stream-io width 150 frame b no-labels down no-box.*/

/*form header
     d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
     "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc         Gross     Disc"
     skip
     fill("_",137) format "x(137)"

    with page-top frame f-top stream-io width 137 no-box.*/

form ap-inv.inv-no
     ap-inv.inv-date format "99/99/99"
     ap-inv.due-date format "99/99/99"
     inv-t[1]
     inv-d[1] format "->>>>.99" 
     inv-t[2]
     inv-d[2] format "->>>>.99" 
     inv-t[3]
     inv-d[3] format "->>>>.99" 
     inv-t[4]
     inv-d[4] format "->>>>.99" 
     ws_gross
     ws_disc-avail

    with stream-io width 137 frame a no-labels down no-box.

EMPTY TEMP-TABLE tt-report.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 b-comp = begin_company
 e-comp = end_company
 d1[1]  = begin_date-1
 d1[2]  = begin_date-2
 d1[3]  = begin_date-3
 v-sort = tb_sort
 v-disc = tb_disc-date
 . 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

/*IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  IF NOT v-company THEN DO:
  excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.
  ELSE DO:
       excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc,Company".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.
END.*/

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

        IF LOOKUP(ttRptSelected.TextList, "1Gross") <> 0    THEN 
         ASSIGN
         str-tit3 = str-tit3 + string(d1[1]) + "    " 
         excelheader-2  = excelheader-2 + string(d1[1]) + "," .
        ELSE IF LOOKUP(ttRptSelected.TextList, "2Gross") <> 0    THEN 
         ASSIGN
         str-tit3 = str-tit3 + string(d1[2]) + "    "
          excelheader-2  = excelheader-2 + string(d1[2]) + ",".
        ELSE IF LOOKUP(ttRptSelected.TextList, "3Gross") <> 0    THEN 
         ASSIGN
         str-tit3 = str-tit3 + string(d1[3]) + "    "
         excelheader-2  = excelheader-2 + string(d1[3]) + "," .
        ELSE
         ASSIGN
             str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
              excelheader-2  = excelheader-2 + " ,"  .


   /*lCountedDateSelected = IF lCountedDateSelected = NO AND ttRptSelected.FieldList = "v-counted-date" THEN YES ELSE NO.
   */    
 END.

  IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).
     /* excelheader = "Name,Inv Number,FG Item,Prod.Category,Qty shipped,Total MSF," +
                    "$/MSF,Sales Amt,Full Cost,Profit". */
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader-2,',','","') '"' skip.
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.

   END. 

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

   FOR EACH company WHERE
       company.company GE b-comp AND
       company.company LE e-comp
       NO-LOCK,
       each ap-inv      
       where ap-inv.company EQ company.company
        and ap-inv.posted  eq yes
        and ap-inv.due     ne 0
      no-lock,

      first vend
      where vend.company eq ap-inv.company 
        and vend.vend-no eq ap-inv.vend-no
      no-lock:
      {custom/statusMsg.i " 'Processing Invoice#  '  + string(ap-inv.inv-no) "}
    create tt-report.
    assign
     tt-report.key-01  = if v-sort then vend.name else ""
     tt-report.key-02  = vend.vend-no
     tt-report.key-03  = ap-inv.inv-no
     tt-report.rec-id  = recid(ap-inv).
  end.
  display "" with frame r-top.
  /*IF v-company THEN
      display "" with frame b-top.
  ELSE
      display "" with frame f-top.*/

  for each tt-report,
      first ap-inv where recid(ap-inv) eq tt-report.rec-id no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03:

      {custom/statusMsg.i " 'Processing Invoice#  '  + string(ap-inv.inv-no) "}

    find first terms where terms.t-code eq ap-inv.terms no-lock no-error.

    find first vend
        where vend.company eq ap-inv.company
          and vend.vend-no eq ap-inv.vend-no
        no-lock no-error.

    /*if first-of (tt-report.key-02) then do:
      put ap-inv.vend-no.
      if avail vend then put vend.name.
      put skip.
    end.*/

    assign
     ws_gross      = ap-inv.due
     ws_disc-avail = if ap-inv.net ne 0 then
                       (ap-inv.net * (ap-inv.disc-% / 100) - ap-inv.disc-taken)
                     else 0.

    do i = 1 to 4:
      assign
       inv-t[i] = 0
       inv-d[i] = 0.
    end.

    if ap-inv.due-date gt d1[3] then
      assign
       vend-t[4] = vend-t[4] + ws_gross
       inv-t[4]  = ws_gross.

    else
    if ap-inv.due-date gt d1[2] then
      assign
       vend-t[3] = vend-t[3] + ws_gross
       inv-t[3]  = ws_gross.

    else
    if ap-inv.due-date gt d1[1] then
      assign
       vend-t[2] = vend-t[2] + ws_gross
       inv-t[2]  = ws_gross.

    else
      assign
       vend-t[1] = vend-t[1] + ws_gross
       inv-t[1]  = ws_gross.

    v-disc-date = if avail terms then
                    (ap-inv.inv-date + terms.disc-days) else ap-inv.due-date.

    if v-disc-date gt d1[3] then
      assign
       vend-d[4] = vend-d[4] + ws_disc-avail
       inv-d[4]  = ws_disc-avail.

    else
    if v-disc-date gt d1[2] then
      assign
       vend-d[3] = vend-d[3] + ws_disc-avail
       inv-d[3]  = ws_disc-avail.

    else
    if v-disc-date gt d1[1] then
      assign
       vend-d[2] = vend-d[2] + ws_disc-avail
       inv-d[2]  = ws_disc-avail.

    else
      assign
       vend-d[1] = vend-d[1] + ws_disc-avail
       inv-d[1]  = ws_disc-avail.
  /*IF NOT v-company THEN do:
    display ap-inv.inv-no
            ap-inv.inv-date
            ap-inv.due-date
              ap-inv.inv-date + ap-inv.disc-days
                when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
            inv-t[1] when inv-t[1] ne 0
            inv-d[1] when inv-d[1] ne 0
            inv-t[2] when inv-t[2] ne 0
            inv-d[2] when inv-d[2] ne 0
            inv-t[3] when inv-t[3] ne 0
            inv-d[3] when inv-d[3] ne 0
            inv-t[4] when inv-t[4] ne 0
            inv-d[4] when inv-d[4] ne 0
            ws_gross
            ws_disc-avail

        with frame a.
    down with frame a.
  END. 
   ELSE do:
    display ap-inv.inv-no
            ap-inv.inv-date
            ap-inv.due-date
              ap-inv.inv-date + ap-inv.disc-days
                when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
            inv-t[1] when inv-t[1] ne 0
            inv-d[1] when inv-d[1] ne 0
            inv-t[2] when inv-t[2] ne 0
            inv-d[2] when inv-d[2] ne 0
            inv-t[3] when inv-t[3] ne 0
            inv-d[3] when inv-d[3] ne 0
            inv-t[4] when inv-t[4] ne 0
            inv-d[4] when inv-d[4] ne 0
            ws_gross
            ws_disc-avail
            ap-inv.company

        with frame b.
    down with frame b.
   END.*/

    /*IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
          '"' IF first-of(tt-report.key-02) THEN
                 ap-inv.vend-no ELSE ""                                    '",'
          '"' IF FIRST-OF(tt-report.key-02) AND    
                 avail vend then vend.NAME ELSE ""                         '",'
          '"' ap-inv.inv-no                                                '",'
          '"' IF ap-inv.inv-date NE ? THEN
                 STRING(ap-inv.inv-date) ELSE ""                           '",'
          '"' IF ws_disc-avail ne 0 and v-disc THEN
                 STRING(ap-inv.inv-date + ap-inv.disc-days)               
              ELSE IF ap-inv.due-date NE ? THEN
                   STRING(ap-inv.due-date) ELSE ""                         '",'
          '"' IF inv-t[1] ne 0 THEN STRING(inv-t[1],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[1] ne 0 THEN STRING(inv-d[1],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[2] ne 0 THEN STRING(inv-t[2],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[2] ne 0 THEN STRING(inv-d[2],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[3] ne 0 THEN STRING(inv-t[3],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[3] ne 0 THEN STRING(inv-d[3],"->>>>.99") ELSE ""    '",'
          '"' IF inv-t[4] ne 0 THEN STRING(inv-t[4],"->>>>>>9.99") ELSE "" '",'
          '"' IF inv-d[4] ne 0 THEN STRING(inv-d[4],"->>>>.99") ELSE ""    '",'
          '"' STRING(ws_gross,"->,>>>,>>9.99")                             '",'
          '"' STRING(ws_disc-avail,'->>>>.99')                             '",'
          '"' IF v-company THEN STRING(ap-inv.company) ELSE ""             '",'
          SKIP.*/



       ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bap-inv:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO: 
                       CASE cTmpField:                                           
                            WHEN "vend-name" THEN cVarValue = IF AVAIL vend THEN string(vend.name,"x(30)") ELSE "" .
                            WHEN "due-date" THEN cVarValue = IF ws_disc-avail ne 0 and v-disc THEN STRING(ap-inv.inv-date + ap-inv.disc-days)  ELSE IF ap-inv.due-date NE ? THEN STRING(ap-inv.due-date) ELSE "" .
                            WHEN "inv-t1" THEN cVarValue = IF inv-t[1] ne 0 THEN  STRING(inv-t[1],"->>>>>>9.99") ELSE ""  .
                            WHEN "inv-d1" THEN cVarValue = IF inv-d[1] ne 0 THEN  STRING(inv-d[1],"->>>>.99") ELSE "" .  
                            WHEN "inv-t2" THEN cVarValue = IF inv-t[2] ne 0 THEN  STRING(inv-t[2],"->>>>>>9.99") ELSE "". 
                            WHEN "inv-d2" THEN cVarValue = IF inv-d[2] ne 0 THEN  STRING(inv-d[2],"->>>>.99") ELSE ""    .
                            WHEN "inv-t3" THEN cVarValue = IF inv-t[3] ne 0 THEN  STRING(inv-t[3],"->>>>>>9.99") ELSE ""  .
                            WHEN "inv-d3" THEN cVarValue = IF inv-d[3] ne 0 THEN  STRING(inv-d[3],"->>>>.99") ELSE "" .   
                            WHEN "inv-t4" THEN cVarValue = IF inv-t[4] ne 0 THEN  STRING(inv-t[4],"->>>>>>9.99") ELSE "" .
                            WHEN "inv-d4" THEN cVarValue = IF inv-d[4] ne 0 THEN  STRING(inv-d[4],"->>>>.99") ELSE "" .    
                            WHEN "ws_gross" THEN cVarValue      = STRING(ws_gross,"->,>>>,>>9.99")  .
                            WHEN "ws_disc-avail" THEN cVarValue = STRING(ws_disc-avail,'->>>>>>.99')  .

                            WHEN "terms" THEN cVarValue = IF AVAIL vend THEN vend.terms ELSE "" . 
                            WHEN "dy-old" THEN cVarValue =  STRING(TODAY - ap-inv.inv-date ,">>>>>>>9") .
                            WHEN "cc-ach" THEN cVarValue = IF AVAIL vend AND vend.payment-type EQ "ACH" THEN "Yes" ELSE "No". 

                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                  END.
              END.
              PUT UNFORMATTED cDisplay SKIP.
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP.
              END.

    if last-of(tt-report.key-02) then do:
       /* IF NOT v-company THEN do:
        display "       *" @ ap-inv.due-date
              vend-t[1]  @ inv-t[1]
              vend-d[1]  @ inv-d[1]
              vend-t[2]  @ inv-t[2]
              vend-d[2]  @ inv-d[2]
              vend-t[3]  @ inv-t[3]
              vend-d[3]  @ inv-d[3]
              vend-t[4]  @ inv-t[4]
              vend-d[4]  @ inv-d[4]
              vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
              vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail

          with frame a.
      down 2 with frame a.
        END.
      ELSE do:
      display "       *" @ ap-inv.due-date
              vend-t[1]  @ inv-t[1]
              vend-d[1]  @ inv-d[1]
              vend-t[2]  @ inv-t[2]
              vend-d[2]  @ inv-d[2]
              vend-t[3]  @ inv-t[3]
              vend-d[3]  @ inv-d[3]
              vend-t[4]  @ inv-t[4]
              vend-d[4]  @ inv-d[4]
              vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
              vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail

          with frame b.
      down 2 with frame b.
      END.*/

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                       CASE cTmpField:                                          
                            WHEN "ap-inv.vend-no" THEN cVarValue =  "*" .
                            WHEN "vend-name" THEN cVarValue =  "" .
                            WHEN "ap-inv.inv-no" THEN cVarValue =  "" .
                            WHEN "ap-inv.inv-date" THEN cVarValue =  "" .
                            WHEN "due-date" THEN cVarValue =  "" .
                            WHEN "ap-inv.company" THEN cVarValue =  "" .
                            WHEN "inv-t1" THEN cVarValue =  STRING(vend-t[1],"->>>>>>9.99")   .
                            WHEN "inv-d1" THEN cVarValue =  STRING(vend-d[1],"->>>>.99")  .  
                            WHEN "inv-t2" THEN cVarValue =  STRING(vend-t[2],"->>>>>>9.99") . 
                            WHEN "inv-d2" THEN cVarValue =  STRING(vend-d[2],"->>>>.99")     .
                            WHEN "inv-t3" THEN cVarValue =  STRING(vend-t[3],"->>>>>>9.99")   .
                            WHEN "inv-d3" THEN cVarValue =  STRING(vend-d[3],"->>>>.99")  .   
                            WHEN "inv-t4" THEN cVarValue =  STRING(vend-t[4],"->>>>>>9.99")  .
                            WHEN "inv-d4" THEN cVarValue =  STRING(vend-d[4],"->>>>.99") .    
                            WHEN "ws_gross" THEN cVarValue      = STRING(vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4],"->,>>>,>>9.99")  .
                            WHEN "ws_disc-avail" THEN cVarValue = STRING(vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4],'->>>>>>.99')  .

                            WHEN "terms" THEN cVarValue =  "" . 
                            WHEN "dy-old" THEN cVarValue = "" . 
                            WHEN "cc-ach" THEN cVarValue = "" . 

                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

              END.
              PUT  UNFORMATTED "    *" substring(cDisplay,6,400) SKIP(1).
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP(1).
              END.

      do i = 1 to 4:
        assign
         grand-t[i] = grand-t[i] + vend-t[i]
         grand-d[i] = grand-d[i] + vend-d[i]

         vend-t[i]  = 0
         vend-d[i]  = 0.
      end.
    end.  /* last-of loop */

    if last(tt-report.key-02) then do:
      /*IF NOT v-company THEN DO:
      down 1 with frame a.

      display "      **" @ ap-inv.due-date
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

          with frame a.
      END.
      ELSE do:
      down 1 with frame b.

      display "      **" @ ap-inv.due-date
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

          with frame b.
      END.*/

         ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                       CASE cTmpField:                                          
                            WHEN "ap-inv.vend-no" THEN cVarValue =  "" .
                            WHEN "vend-name" THEN cVarValue =  "" .
                            WHEN "ap-inv.inv-no" THEN cVarValue =  "" .
                            WHEN "ap-inv.inv-date" THEN cVarValue =  "" .
                            WHEN "due-date" THEN cVarValue =  "" .
                            WHEN "ap-inv.company" THEN cVarValue =  "" .
                            WHEN "inv-t1" THEN cVarValue =  STRING(grand-t[1],"->>>>>>9.99")   .
                            WHEN "inv-d1" THEN cVarValue =  STRING(grand-d[1],"->>>>.99")  .  
                            WHEN "inv-t2" THEN cVarValue =  STRING(grand-t[2],"->>>>>>9.99"). 
                            WHEN "inv-d2" THEN cVarValue =  STRING(grand-d[2],"->>>>.99")    .
                            WHEN "inv-t3" THEN cVarValue =  STRING(grand-t[3],"->>>>>>9.99")   .
                            WHEN "inv-d3" THEN cVarValue =  STRING(grand-d[3],"->>>>.99")  .   
                            WHEN "inv-t4" THEN cVarValue =  STRING(grand-t[4],"->>>>>>9.99")  .
                            WHEN "inv-d4" THEN cVarValue =  STRING(grand-d[4],"->>>>.99") .    
                            WHEN "ws_gross" THEN cVarValue      = STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->,>>>,>>9.99")  .
                            WHEN "ws_disc-avail" THEN cVarValue = STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>>>.99')  .

                            WHEN "terms" THEN cVarValue =  "" . 
                            WHEN "dy-old" THEN cVarValue = "" . 
                            WHEN "cc-ach" THEN cVarValue =  "" . 

                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

              END.
              PUT  UNFORMATTED "    **" substring(cDisplay,7,400) SKIP(1).
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP.
              END.



    end.
  end. /* for each */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

