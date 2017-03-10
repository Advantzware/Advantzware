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
DEFINE VARIABLE cc-codeValue AS CHARACTER NO-UNDO.

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_whs end_whs begin_rm-no ~
end_rm-no begin_code end_code begin_bin end_bin begin_procat end_procat ~
rd_sort tb_qty-oh tb_zero-bal rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_whs end_whs begin_rm-no end_rm-no ~
begin_code end_code begin_bin end_bin begin_procat end_procat lbl_sort ~
rd_sort tb_qty-oh tb_zero-bal rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_code AS CHARACTER FORMAT "XX":U 
     LABEL "Beginning Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning  Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_bin AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_code AS CHARACTER FORMAT "XX":U INITIAL "zz" 
     LABEL "Ending Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-rmphct.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>9":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item", "Item",
"Bin", "Bin",
"Desc.", "Desc"
     SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.95.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_qty-oh AS LOGICAL INITIAL no 
     LABEL "Do you want Quantity On Hand to Print?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_zero-bal AS LOGICAL INITIAL no 
     LABEL "Do you want to print Zero Balances Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_whs AT ROW 2.91 COL 29 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_rm-no AT ROW 3.86 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_code AT ROW 4.81 COL 29 COLON-ALIGNED HELP
          "Enter Begining Cycle Count Code"
     end_code AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Cycle Count Code"
     begin_bin AT ROW 5.76 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Bin"
     end_bin AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter ending Bin"
     begin_procat AT ROW 6.71 COL 29 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     lbl_sort AT ROW 8.14 COL 29 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.14 COL 41 NO-LABEL
     tb_qty-oh AT ROW 9.33 COL 31
     tb_zero-bal AT ROW 10.52 COL 31
     rd-dest AT ROW 14.1 COL 7 NO-LABEL
     lv-ornt AT ROW 14.33 COL 32 NO-LABEL
     lines-per-page AT ROW 14.33 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 16.48 COL 36 COLON-ALIGNED
     lv-font-name AT ROW 17.43 COL 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.62 COL 33
     tb_excel AT ROW 19.57 COL 66 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.57 COL 90 RIGHT-ALIGNED
     fi_file AT ROW 20.67 COL 44 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.14 COL 18
     btn-cancel AT ROW 23.14 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 4
     RECT-6 AT ROW 12.67 COL 2
     RECT-7 AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 24.62.


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
         TITLE              = "RM Inventory By Bin/Tag"
         HEIGHT             = 24.62
         WIDTH              = 95.8
         MAX-HEIGHT         = 24.62
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 24.62
         VIRTUAL-WIDTH      = 95.8
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
       begin_bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_qty-oh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* RM Inventory By Bin/Tag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* RM Inventory By Bin/Tag */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bin C-Win
ON LEAVE OF begin_bin IN FRAME FRAME-A /* Beginning Bin */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON HELP OF begin_code IN FRAME FRAME-A /* Beginning Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeRM.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON LEAVE OF begin_code IN FRAME FRAME-A /* Beginning Cycle Count Code */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning  Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* Beginning Warehouse */
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

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "begin_procat"
                            &END_cust= "begin_procat" 
                            &fax-subject="RM Inventory By Bin/Tag"
                            &fax-body="RM Inventory By Bin/Tag"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_procat"
                             &END_cust= "begin_procat"
                             &mail-subject="RM Inventory By Bin/Tag"
                             &mail-body="RM Inventory By Bin/Tag"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE =''
                                  &begin_cust="begin_procat"
                                  &END_cust="begin_procat"
                                  &mail-subject="RM Inventory By Bin/Tag"
                                  &mail-body="RM Inventory By Bin/Tag"
                                  &mail-file=list-name }
           END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
  SESSION:SET-WAIT-STATE ("").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bin C-Win
ON LEAVE OF end_bin IN FRAME FRAME-A /* Ending Bin */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON HELP OF end_code IN FRAME FRAME-A /* Ending Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeRM.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON LEAVE OF end_code IN FRAME FRAME-A /* Ending Cycle Count Code */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_qty-oh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-oh C-Win
ON VALUE-CHANGED OF tb_qty-oh IN FRAME FRAME-A /* Do you want Quantity On Hand to Print? */
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


&Scoped-define SELF-NAME tb_zero-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-bal C-Win
ON VALUE-CHANGED OF tb_zero-bal IN FRAME FRAME-A /* Do you want to print Zero Balances Items? */
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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
     APPLY "entry" TO begin_whs.
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
  DISPLAY begin_whs end_whs begin_rm-no end_rm-no begin_code end_code begin_bin 
          end_bin begin_procat end_procat lbl_sort rd_sort tb_qty-oh tb_zero-bal 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_whs end_whs begin_rm-no end_rm-no begin_code 
         end_code begin_bin end_bin begin_procat end_procat rd_sort tb_qty-oh 
         tb_zero-bal rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
   */
  {custom\out2file.i}

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
 RUN custom\d-print.w (list-name).
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
/* ------------------------------------------------ rm/rep/rm-cyclc.p 9/91 cd */
/* raw materials - cycle count worksheet                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}
def var it-spaces as ch format "X(21)" initial "      _______________".
def var price as dec          format "->>>>9.99".
def var alloc as dec          format "->>>,>>9.99".
def var xwhse like rm-bin.loc.
def var fcode as ch format "x(2)" initial "".
def var tcode like fcode initial "ZZ".
def var type as log format "Y/N" initial no.
def var type-prt as ch format "X(13)" initial "             ".
def var v-floc as ch format "x(5)" initial "".
def var v-tloc like v-floc initial "ZZZZZ".
def var v-cat as ch format "x(5)" extent 2 initial ["","ZZZZZ"].
def var v-cust as ch format "x(8)" extent 2 initial ["","ZZZZZZZZ"].
def var v-i-no as ch format "x(15)" extent 2 initial ["","ZZZZZZZZZZZZZZZ"].
def var v-loc-bin as ch format "x(8)" extent 2 initial ["", "ZZZZZZZZ"] no-undo.
def var v-item-bin as CHAR no-undo.
def var v-prnt-onh as logical initial "N".
def var v-prnt-zer as log init "N".


form
    rm-bin.i-no
    item.i-name
    rm-bin.loc
    rm-bin.loc-bin
    rm-bin.tag
    rm-bin.qty     format "->>,>>>,>>9.999"
    it-spaces
    skip
header
"ITEM       DESCRIPTION                    WHSE  BIN      TAG                          ON HAND       QUANTITY COUNTED"
    with frame itemx no-box no-labels down STREAM-IO width 132.

xwhse = locode.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-floc       = begin_whs
 v-tloc       = end_whs
 v-i-no[1]    = begin_rm-no
 v-i-no[2]    = end_rm-no
 fcode        = begin_code
 tcode        = end_code
 v-loc-bin[1] = begin_bin
 v-loc-bin[2] = END_bin
 v-cat[1]     = begin_procat
 v-cat[2]     = end_procat
 v-item-bin   = rd_sort
 v-prnt-onh   = tb_qty-oh
 v-prnt-zer   = tb_zero-bal.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

VIEW frame r-top.

SESSION:SET-WAIT-STATE("general").

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","
       "ITEM"
       "DESCRIPTION"
       "WHSE"
       "BIN"
       "TAG" 
       "ON HAND"
       SKIP.
END. 

if v-item-bin EQ "Item" then

   for each item where item.company =  cocode
                   and item.i-code  <> "E"
                   and item.i-no    >= v-i-no[1]
                   and item.i-no    <= v-i-no[2]
                   and item.i-no    ne ""
                   and item.cc-code >= fcode
                   and item.cc-code <= tcode
                   and item.procat  >= v-cat[1]
                   and item.procat  <= v-cat[2]
                   and (item.q-onh ne 0 OR v-prnt-zer) 
                   no-lock,
             each rm-bin where rm-bin.company  = cocode                           
                           and rm-bin.i-no     = item.i-no
                           and rm-bin.loc      >= v-floc
                           and rm-bin.loc      <= v-tloc
                           and rm-bin.loc-bin  >= v-loc-bin[1]
                           and rm-bin.loc-bin  <= v-loc-bin[2]
                           AND ((rm-bin.qty <> 0 AND v-prnt-onh) OR NOT v-prnt-onh OR v-prnt-zer)  /* task 11201306 */
                           no-lock
             break by rm-bin.i-no by rm-bin.loc by rm-bin.loc-bin by rm-bin.tag
             with frame itemx:

         assign price = 0
                alloc = 0.

         if first-of(rm-bin.i-no) then put skip(1).

         display rm-bin.i-no    when first-of(rm-bin.i-no)
                 item.i-name    when first-of(rm-bin.i-no)
                 rm-bin.loc     when first-of(rm-bin.loc)
                 rm-bin.loc-bin
                 rm-bin.tag FORM "x(20)"
                 rm-bin.qty     when v-prnt-onh = true
                 it-spaces.
                 down.
         IF tb_excel THEN 
            EXPORT STREAM excel DELIMITER ","
            (IF first-of(rm-bin.i-no) THEN rm-bin.i-no ELSE "")
            (IF first-of(rm-bin.i-no) THEN item.i-name ELSE "")
            (IF first-of(rm-bin.loc) THEN rm-bin.loc ELSE "")
            rm-bin.loc-bin
            rm-bin.tag
            (IF v-prnt-onh THEN rm-bin.qty ELSE 0)
            SKIP.
   end. /* for each item */

ELSE IF v-item-bin EQ "Bin" THEN
    for each rm-bin
       where rm-bin.company  eq cocode
         and rm-bin.i-no     ge v-i-no[1]
         and rm-bin.i-no     le v-i-no[2]
         and rm-bin.loc      ge v-floc
         and rm-bin.loc      le v-tloc
         and rm-bin.i-no     ne ""
         and rm-bin.loc-bin  ge v-loc-bin[1]
         and rm-bin.loc-bin  le v-loc-bin[2] 
         AND ((rm-bin.qty <> 0 AND v-prnt-onh ) OR NOT v-prnt-onh OR v-prnt-zer)  /* task 11201306 */
         no-lock,
         first item
         where item.company eq cocode
         and item.i-no    eq rm-bin.i-no 
         and item.procat  ge v-cat[1]
         and item.procat  le v-cat[2]
         and item.cc-code ge fcode
         and item.cc-code le tcode 
         and (item.q-onh ne 0 or v-prnt-zer)
         no-lock
         break by rm-bin.loc
               by rm-bin.loc-bin
               by rm-bin.tag
               by rm-bin.i-no 
        with frame itemx:

     assign
       price = 0
       alloc = 0.

     if first-of(rm-bin.tag) then put skip(1).

     display rm-bin.i-no
             item.i-name
             rm-bin.loc
             rm-bin.loc-bin
             rm-bin.tag FORM "x(20)"
             rm-bin.qty      when v-prnt-onh
             it-spaces.
     down.
     IF tb_excel THEN 
            EXPORT STREAM excel DELIMITER ","
            IF rm-bin.i-no <> "" THEN rm-bin.i-no ELSE ""
            IF item.i-name <> "" THEN item.i-name ELSE ""
            IF rm-bin.loc <> ""  THEN rm-bin.loc ELSE ""
            rm-bin.loc-bin
            rm-bin.tag
            (IF v-prnt-onh THEN rm-bin.qty ELSE 0)
            SKIP.
   end. /* for each item */

ELSE
   for each item where item.company =  cocode
                   and item.i-code  <> "E"
                   and item.i-no    >= v-i-no[1]
                   and item.i-no    <= v-i-no[2]
                   and item.i-no    ne ""
                   and item.cc-code >= fcode
                   and item.cc-code <= tcode
                   and item.procat  >= v-cat[1]
                   and item.procat  <= v-cat[2]
                   and (item.q-onh ne 0 or v-prnt-zer)
                   no-lock,
             each rm-bin where rm-bin.company  = cocode                           
                           and rm-bin.i-no     = item.i-no
                           and rm-bin.loc      >= v-floc
                           and rm-bin.loc      <= v-tloc
                           and rm-bin.loc-bin  >= v-loc-bin[1]
                           and rm-bin.loc-bin  <= v-loc-bin[2]
                           AND ((rm-bin.qty <> 0 AND v-prnt-onh ) OR NOT v-prnt-onh OR v-prnt-zer)  /* task 11201306 */
                           no-lock
             break BY item.i-name by rm-bin.i-no by rm-bin.loc by rm-bin.loc-bin by rm-bin.tag
             with frame itemx:

         assign price = 0
                alloc = 0.

         if first-of(item.i-name) then put skip(1).

         display rm-bin.i-no    when first-of(rm-bin.i-no)
                 item.i-name    when first-of(ITEM.i-name)
                 rm-bin.loc     when first-of(rm-bin.loc)
                 rm-bin.loc-bin
                 rm-bin.tag FORM "x(20)"
                 rm-bin.qty     when v-prnt-onh = true
                 it-spaces.
                 down.
         IF tb_excel THEN 
            EXPORT STREAM excel DELIMITER ","
            (IF first-of(rm-bin.i-no) THEN rm-bin.i-no ELSE "")
            (IF first-of(rm-bin.i-no) THEN item.i-name ELSE "")
            (IF first-of(rm-bin.loc) THEN rm-bin.loc ELSE "")
            rm-bin.loc-bin
            rm-bin.tag
            (IF v-prnt-onh THEN rm-bin.qty ELSE 0)
            SKIP.
   end. /* for each item */


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.

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

