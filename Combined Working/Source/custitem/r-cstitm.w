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

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.

DEFINE VARIABLE tmp-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.

DEF STREAM excel.
DEF VAR excelcol AS INT NO-UNDO.

ASSIGN
  cocode = gcompany
  locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust-no end_cust-no ~
begin_shipto end_shipto begin_i-no end_i-no begin_cat end_cat ~
t-use-cust-inv TG_customer-plant-inv t-include-order rd-tot-rel ldt-as-of ~
rd-print rd-print-lot rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_shipto ~
end_shipto begin_i-no end_i-no begin_cat end_cat t-use-cust-inv ~
TG_customer-plant-inv t-include-order rd-tot-rel ldt-as-of rd-print ~
rd-print-lot rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)" 
     LABEL "Beginning Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_shipto AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Ship-To" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shipto AS CHARACTER FORMAT "X(30)" INITIAL "zzzzzzzz" 
     LABEL "Ending Ship-To" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-cstitm.r" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-as-of AS DATE FORMAT "99/99/9999":U 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd-print AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purchased", "P",
"Manufactured", "M",
"Both", "A"
     SIZE 44 BY .95 NO-UNDO.

DEFINE VARIABLE rd-print-lot AS CHARACTER INITIAL "R" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Level", "L",
"Reorder", "R",
"Both", "A"
     SIZE 39 BY .71 NO-UNDO.

DEFINE VARIABLE rd-tot-rel AS CHARACTER INITIAL "T" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Total Allocated", "T",
"Release Qty", "R",
"All", "A"
     SIZE 44 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 11.19.

DEFINE VARIABLE t-include-order AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE t-use-cust-inv AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export to Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG_customer-plant-inv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.14 COL 23 COLON-ALIGNED HELP
          "Enter Beginning <Name>"
     end_cust-no AT ROW 2.14 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer #"
     begin_shipto AT ROW 3.1 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Ship-To"
     end_shipto AT ROW 3.1 COL 65 COLON-ALIGNED HELP
          "Enter Ending Ship-To"
     begin_i-no AT ROW 4.05 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Item"
     end_i-no AT ROW 4.05 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item"
     begin_cat AT ROW 5 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 5 COL 65 COLON-ALIGNED HELP
          "Enter Ending Category"
     t-use-cust-inv AT ROW 6.24 COL 65
     TG_customer-plant-inv AT ROW 6.95 COL 64.8 WIDGET-ID 2
     t-include-order AT ROW 7.71 COL 65
     rd-tot-rel AT ROW 9.1 COL 19 NO-LABEL
     ldt-as-of AT ROW 9.1 COL 69 COLON-ALIGNED
     rd-print AT ROW 10.29 COL 32 NO-LABEL
     rd-print-lot AT ROW 11.71 COL 35 NO-LABEL
     rd-dest AT ROW 13.38 COL 7 NO-LABEL
     lv-ornt AT ROW 13.38 COL 30 NO-LABEL
     lines-per-page AT ROW 13.38 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 15.52 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.91 COL 30
     tb_excel AT ROW 19.1 COL 30 WIDGET-ID 6
     tb_runExcel AT ROW 19.1 COL 51 WIDGET-ID 8
     fi_file AT ROW 19.81 COL 28 COLON-ALIGNED WIDGET-ID 10
     btn-ok AT ROW 21.71 COL 20
     btn-cancel AT ROW 21.71 COL 52
     "Include Quantity on Order with Quantity on Hand:" VIEW-AS TEXT
          SIZE 47 BY .71 AT ROW 7.71 COL 17
     "Print" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 10.29 COL 24
     "Print Lot Controller" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 16
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 5
     "Use" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 9.1 COL 13
     "Customers Plants Inventory" VIEW-AS TEXT
          SIZE 26.8 BY .62 AT ROW 7 COL 37.2 WIDGET-ID 4
     "Use Customer Inventory" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 6.24 COL 40
     RECT-6 AT ROW 12.91 COL 2
     RECT-7 AT ROW 1.48 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.48.


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
         TITLE              = "Customer Inventory Reorder Report"
         HEIGHT             = 22.76
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Customer Inventory Reorder Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Inventory Reorder Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shipto C-Win
ON LEAVE OF begin_shipto IN FRAME FRAME-A /* Beginning Ship-To */
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
  ASSIGN {&displayed-objects}.

  RUN run-report.

  CASE rd-dest:
    WHEN 1 THEN RUN output-to-printer.
    WHEN 2 THEN RUN output-to-screen.
    WHEN 3 THEN RUN output-to-file.
    WHEN 4 THEN DO:
      /*run output-to-fax.*/
      {custom/asifax.i &type="Customer Inventory Reorder Report"
                       &begin_cust=begin_cust-no
                       &end_cust=end_cust-no
                       &fax-subject=CURRENT-WINDOW:TITLE
                       &fax-body=CURRENT-WINDOW:TITLE
                       &fax-file=list-name}
    END. 
    WHEN 5 THEN DO:
      IF is-xprint-form THEN DO:
        {custom/asimail.i &type="Customer Inventory Reorder Report"
                          &begin_cust=begin_cust-no
                          &end_cust=end_cust-no
                          &mail-subject=CURRENT-WINDOW:TITLE
                          &mail-body=CURRENT-WINDOW:TITLE
                          &mail-file=list-name}
      END.
      ELSE DO:
        {custom/asimailr.i &type="Customer Inventory Reorder Report"
                           &begin_cust=begin_cust-no
                           &end_cust=end_cust-no
                           &mail-subject=CURRENT-WINDOW:TITLE
                           &mail-body=CURRENT-WINDOW:TITLE
                           &mail-file=list-name}
      END.
    END.
    WHEN 6 THEN RUN output-to-port.
  END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shipto C-Win
ON LEAVE OF end_shipto IN FRAME FRAME-A /* Ending Ship-To */
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


&Scoped-define SELF-NAME ldt-as-of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ldt-as-of C-Win
ON LEAVE OF ldt-as-of IN FRAME FRAME-A /* As of */
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


&Scoped-define SELF-NAME rd-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-print C-Win
ON VALUE-CHANGED OF rd-print IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-print-lot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-print-lot C-Win
ON VALUE-CHANGED OF rd-print-lot IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-tot-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-tot-rel C-Win
ON VALUE-CHANGED OF rd-tot-rel IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-include-order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-include-order C-Win
ON VALUE-CHANGED OF t-include-order IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-use-cust-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-use-cust-inv C-Win
ON VALUE-CHANGED OF t-use-cust-inv IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export to Excel? */
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


&Scoped-define SELF-NAME TG_customer-plant-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_customer-plant-inv C-Win
ON VALUE-CHANGED OF TG_customer-plant-inv IN FRAME FRAME-A
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  ldt-as-of = today.           
  RUN init-proc.
  RUN enable_UI.
  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}
  END.
  {methods/nowait.i}
  APPLY 'ENTRY' TO begin_cust-no IN FRAME {&FRAME-NAME}.
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
  DISPLAY begin_cust-no end_cust-no begin_shipto end_shipto begin_i-no end_i-no 
          begin_cat end_cat t-use-cust-inv TG_customer-plant-inv t-include-order 
          rd-tot-rel ldt-as-of rd-print rd-print-lot rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_shipto end_shipto 
         begin_i-no end_i-no begin_cat end_cat t-use-cust-inv 
         TG_customer-plant-inv t-include-order rd-tot-rel ldt-as-of rd-print 
         rd-print-lot rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    begin_cust-no = ''
    end_cust-no = 'zzzzzzzz'
    begin_shipto = ''
    end_shipto = 'zzzzzzzz'
    begin_i-no = ''
    end_i-no = 'zzzzzzzzzzzzzzz'
    begin_cat = ''
    end_cat = 'zzzzzzzzzzzzzzz'.

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
  RUN custom/d-print.w (list-name).

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
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

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
  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* fg/rep/fgreord1.p 01/01 JLF         */
/* Reorder Advice Report by Ship-To    */

DEF VAR save_id AS RECID.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR time_stamp AS CHAR NO-UNDO.
time_stamp = STRING(TIME, "hh:mmam").

DEF VAR v-cust      LIKE itemfg.cust-no EXTENT 2 INIT ["","zzzzzzzz"].
DEF VAR v-ship      LIKE oe-rel.ship-id EXTENT 2 INIT ["","zzzzzzzz"].
DEF VAR v-cat       LIKE itemfg.procat EXTENT 2 INIT ["","zzzzzz"].
DEF VAR v-item      LIKE itemfg.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEF VAR v-inconh    AS LOG FORMAT "Y/N" init "Y".
DEF VAR v-totrel    AS LOG FORMAT "Tot All/Release" INIT "Y".
DEF VAR v-date      AS DATE FORMAT "99/99/9999" INIT TODAY.
DEF VAR v-pur-man   AS CHAR FORMAT "!" INIT "A".
DEF VAR v-lot-reo   AS CHAR FORMAT "!" INIT "R".
DEF VAR v-prt-cpn   AS LOG FORMAT "Y/N" INIT NO.

DEF VAR v-reord-qty AS INT NO-UNDO.
DEF VAR v-qty-avail AS INT NO-UNDO.
DEF VAR v-alloc-qty AS INT NO-UNDO.
DEF VAR v-rec-date  AS DATE NO-UNDO.
DEF VAR v-coverage AS DEC NO-UNDO.
DEF VAR v-mon-coverage AS DEC NO-UNDO.
DEF VAR v-new-qty AS CHAR  FORMAT "x(7)" INIT "_______"    NO-UNDO. 
DEF VAR v-tot-cust-qty AS INT NO-UNDO.
DEF VAR v-tot-annual-cons AS INT NO-UNDO.
DEF VAR v-first-rec AS LOG NO-UNDO.
DEF VAR ls-selection AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR ls-key-06 AS CHAR NO-UNDO.
DEF VAR v-tot-plant-qty AS INT NO-UNDO.
DEF VAR v-first-record AS LOG NO-UNDO.
DEF VAR v-total-cust-onhand-qty AS INT NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR excel-skip   AS LOG NO-UNDO.

SESSION:SET-WAIT-STATE('general').

IF rd-tot-rel = "A" THEN ls-selection = "All".
ELSE IF rd-tot-rel = "T" THEN ls-selection = "Total Allocated".
ELSE IF rd-tot-rel = "R" THEN ls-selection = "Release Qty".
ELSE ls-selection = "".

{sys/inc/sa-sls01.i}

FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   init-dir = users.user_program[2].
ELSE init-dir = "c:\temp".

list-name = init-dir + "\cstitm.rpt".
OUTPUT TO VALUE(list-name) PAGE-SIZE 45.

IF tb_excel THEN 
DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
END.

FORM HEADER
   "***** Finished Goods - Reorder List *****        As of" TODAY  
   "     Selection:" ls-selection                   
   "Page:" + STRING(PAGE-NUM,">>9") FORMAT "x(10)" TO 151 SKIP
   FILL("=",159)  form "x(159)"   /* 126 for font9 (pitch 10) */
   WITH FRAME rpt-ttl WIDTH 159 NO-BOX NO-LABEL STREAM-IO.

VIEW FRAME rpt-ttl.     

ASSIGN 
   cocode = gcompany
   v-cust[1] = begin_cust-no
   v-cust[2] = end_cust-no
   v-ship[1] = begin_shipto
   v-ship[2] = end_shipto
   v-item[1] = begin_i-no
   v-item[2] = end_i-no
   v-cat[1] = begin_cat
   v-cat[2] = end_cat
   v-lot-reo = rd-print-lot
   v-pur-man = rd-print
   v-inconh = t-include-order
   v-totrel = IF rd-tot-rel = "T" OR rd-tot-rel = "A" THEN YES ELSE NO
   v-date = ldt-as-of.

IF rd-tot-rel = "A" THEN DO:
   FOR EACH itemfg WHERE itemfg.company = cocode
                     AND itemfg.cust-no >= v-cust[1] 
                     AND itemfg.cust-no <= v-cust[2]
                     AND itemfg.i-no    >= v-item[1]
                     AND itemfg.i-no    <= v-item[2]
                     AND itemfg.procat  >= v-cat[1]
                     AND itemfg.procat  <= v-cat[2]
                     AND ((itemfg.ord-policy     AND v-lot-reo = "R") OR
                          (NOT itemfg.ord-policy AND v-lot-reo = "L") OR v-lot-reo = "A")
                     AND ((itemfg.pur-man        AND v-pur-man = "P") OR
                          (NOT itemfg.pur-man    AND v-pur-man = "M") OR v-pur-man = "A") USE-INDEX i-no NO-LOCK:

      ASSIGN  
         v-qty-avail = itemfg.q-onh + (IF v-inconh THEN itemfg.q-ono ELSE 0)
         v-alloc-qty = 0.

      IF v-totrel THEN 
         v-alloc-qty = itemfg.q-alloc.
      ELSE
         FOR EACH oe-ordl WHERE oe-ordl.company = cocode
                            AND oe-ordl.i-no    = itemfg.i-no USE-INDEX ITEM NO-LOCK,
            EACH oe-rel WHERE oe-rel.company  = cocode
                          AND oe-rel.ord-no   = oe-ordl.ord-no
                          AND oe-rel.i-no     = oe-ordl.i-no
                          AND oe-rel.link-no  = 0
                          AND oe-rel.rel-date le v-date USE-INDEX ord-item NO-LOCK:
               v-alloc-qty = v-alloc-qty + oe-rel.qty.
         END.
      v-qty-avail = v-qty-avail - v-alloc-qty.
      FIND FIRST oe-ordl WHERE oe-ordl.company = cocode 
                           AND oe-ordl.i-no = itemfg.i-no NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-ordl THEN DO:  /* no order - new item */
         ASSIGN 
            v-tot-cust-qty = 0
            v-tot-annual-cons = 0
            v-first-rec = YES
            v-total-cust-onhand-qty = 0.
         FOR EACH cust-itm WHERE cust-itm.company = cocode
                             AND cust-itm.cust-no = itemfg.cust-no
                             AND cust-itm.i-no = itemfg.i-no NO-LOCK:           
            v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
            IF v-first-rec THEN v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
            v-first-rec = no.
         END.
         ASSIGN 
            v-tot-plant-qty = 0
            v-first-record = YES.
         FOR EACH vend-whse-item WHERE vend-whse-item.company = cocode 
                                   AND TRIM(vend-whse-item.cust-no) = TRIM(itemfg.cust-no)
                                   AND TRIM(vend-whse-item.fg-item-no)   = TRIM(itemfg.i-no) NO-LOCK:  
            v-tot-plant-qty = v-tot-plant-qty + vend-whse-item.plant-tot-oh-qty.
            IF v-first-record THEN v-tot-annual-cons = v-tot-annual-cons + vend-whse-item.annual-usage-qty.
            v-first-record = no.
         END.
         IF t-use-cust-inv THEN
            v-total-cust-onhand-qty = v-tot-cust-qty. ELSE v-total-cust-onhand-qty = 0.
         IF TG_customer-plant-inv = YES THEN
            v-total-cust-onhand-qty = v-total-cust-onhand-qty + v-tot-plant-qty.

         /* ==== */          
         CREATE report.
         ASSIGN 
            report.term-id = v-term
            report.key-01  = itemfg.cust-no
            report.key-02  = " "/*oe-rel.ship-id */
            report.key-03  = itemfg.i-no
            report.key-04  = STRING(v-alloc-qty,"-999999999999")
            report.key-05  = STRING(v-qty-avail,"-999999999999")
            report.key-06  = STRING(v-total-cust-onhand-qty,"-999999999999") 
            report.key-07  = string(v-tot-annual-cons,"-999999999999").      
      END. /* = end  of no oe-ordl record= */  
      ELSE DO:                   
         IF itemfg.ord-level GT v-qty-avail THEN
            FOR EACH oe-ordl WHERE oe-ordl.company = cocode
                               AND oe-ordl.i-no    = itemfg.i-no NO-LOCK,
               EACH oe-rel WHERE oe-rel.company = cocode
                             AND oe-rel.ord-no  = oe-ordl.ord-no
                             AND oe-rel.i-no    = oe-ordl.i-no
                             AND oe-rel.line    = oe-ordl.line NO-LOCK
                              BY oe-rel.rel-date DESC
                              BY oe-rel.r-no     DESC:

               IF oe-rel.cust-no ge v-cust[1] AND oe-rel.cust-no LE v-cust[2] AND
                  oe-rel.ship-id ge v-ship[1] AND oe-rel.ship-id le v-ship[2] THEN DO: /* added logic for customer's inventory */
                  ASSIGN 
                     v-tot-cust-qty = 0
                     v-tot-annual-cons = 0
                     v-first-rec = YES
                     v-total-cust-onhand-qty = 0.

                  FOR EACH cust-itm WHERE cust-itm.company = cocode
                                      AND cust-itm.cust-no = itemfg.cust-no
                                      AND cust-itm.i-no = itemfg.i-no NO-LOCK:           
                     v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
                     IF v-first-rec THEN v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
                     v-first-rec = NO.
                  END. /* ========== */  
                  ASSIGN 
                     v-tot-plant-qty = 0
                     v-first-record = YES.
                  FOR EACH vend-whse-item WHERE vend-whse-item.company = cocode 
                                            AND TRIM(vend-whse-item.cust-no) = TRIM(itemfg.cust-no)
                                            AND TRIM(vend-whse-item.fg-item-no)   = TRIM(itemfg.i-no) NO-LOCK: 
                     v-tot-plant-qty = v-tot-plant-qty + vend-whse-item.plant-tot-oh-qty.
                     IF v-first-record THEN v-tot-annual-cons = v-tot-annual-cons + vend-whse-item.annual-usage-qty.
                     v-first-record = no.
                  END.
                  IF t-use-cust-inv = YES THEN
                     v-total-cust-onhand-qty = v-tot-cust-qty. ELSE v-total-cust-onhand-qty = 0.
                  IF TG_customer-plant-inv = YES THEN
                     v-total-cust-onhand-qty = v-total-cust-onhand-qty + v-tot-plant-qty.
                  CREATE report.
                  ASSIGN 
                     report.term-id = v-term
                     report.key-01  = itemfg.cust-no
                     report.key-02  = oe-rel.ship-id 
                     report.key-03  = itemfg.i-no
                     report.key-04  = STRING(v-alloc-qty,"-999999999999")
                     report.key-05  = STRING(v-qty-avail,"-999999999999")
                     report.key-06  = STRING(v-total-cust-onhand-qty,"-999999999999") 
                     report.key-07  = STRING(v-tot-annual-cons,"-999999999999").
                  LEAVE. 
               END.  /* if */   
            END.   /* for each */
      END.  /* else  */
   END. /* each itemfg  for "A" */

   IF tb_excel THEN 
   DO: 
     EXPORT STREAM excel DELIMITER ","
                       "Customer"
                       "Item"
                       "CustomerStock"
                       "OnHand"
                       "PO/JobsQty Due"
                       "OrderQtyReleased"
                       "Available"
                       "MonthlyUsage" 
                       "Month ofCoverage"
                       "QtyShort"
                       "ReordLevel"
                       "JobQty"
                       "NewQty".

   END.
   /* display information */
   FOR EACH report WHERE report.term-id = v-term,
      FIRST itemfg WHERE itemfg.company = cocode
                     AND itemfg.i-no    = report.key-03 NO-LOCK  
                BREAK BY report.key-01
                      BY report.key-02
                      BY report.key-03 TRANSACTION:
      ASSIGN  
         v-reord-qty = itemfg.ord-level - INT(report.key-05) - INT(report.key-06)
         v-rec-date  = ?      
         v-coverage = (INT(report.key-05) + INT(report.key-06)) / (INT(report.key-07) / 12)  /* monthly , weekly : 52 */
         v-mon-coverage = IF (INT(report.key-07) / 12) = ? THEN 0 ELSE (INT(report.key-07) / 12).
      IF v-coverage = ? THEN v-coverage = 0.

      IF rd-tot-rel = "A" AND v-reord-qty < 0 THEN 
         v-reord-qty = 0. /* display even if v-reord-qty < 0 when rd-tot-rel = "A" */
      ls-key-06 = IF INT(report.key-06) <> 0 THEN 
                     STRING(INT(report.key-06),"->>>>>>9")  
                  ELSE 
                     "________". 
      IF v-reord-qty >= 0 THEN              
         DISPLAY 
            itemfg.cust-no    COLUMN-LABEL "Customer"
            itemfg.i-no       COLUMN-LABEL "Item"
            ls-key-06         COLUMN-LABEL "Customer!Stock" FORMAT "x(8)" /*Inventory*/
            itemfg.q-onh      COLUMN-LABEL "On!Hand" FORMAT "->>>>>>9"
            itemfg.q-ono WHEN v-inconh COLUMN-LABEL "PO/Jobs!Qty Due" FORMAT "->>>>>9" /*"On!Order" */
            INT(report.key-04)         COLUMN-LABEL "OrderQty!Released" FORMAT "->>>>>>9"
            INT(report.key-05) + INT(report.key-06) COLUMN-LABEL "Avail!able" FORMAT "->>>>>>9"
            v-mon-coverage    COLUMN-LABEL "Monthly!Usage" FORMAT "->>>>9.99"                          
            v-coverage        COLUMN-LABEL "Month of!Coverage" FORMAT "->>>>9.99"
            v-reord-qty       COLUMN-LABEL "Qty!Short" FORMAT ">>>>>9"
            itemfg.ord-level  COLUMN-LABEL "Reord!Level" FORMAT ">>>>>9"
            itemfg.ord-min    COLUMN-LABEL "Job!Qty" FORMAT ">>>>>>9"        
            v-new-qty         COLUMN-LABEL "New!Qty"       
         WITH DOWN FRAME aaa NO-BOX WIDTH 180 STREAM-IO.

      IF tb_excel THEN
      DO: 
         EXPORT STREAM excel DELIMITER ","
            itemfg.cust-no
            itemfg.i-no   
            ls-key-06     
            itemfg.q-onh  
            (IF v-inconh THEN itemfg.q-ono ELSE 0)
            INT(report.key-04)
            INT(report.key-05) + INT(report.key-06)
            v-mon-coverage
            v-coverage    
            v-reord-qty   
            itemfg.ord-level
            itemfg.ord-min  
            v-new-qty.       
      END.

      DELETE report.  
   END.
END. /* ========== end for all itemfg ==========*/
ELSE DO: /* ========= reorder report =========*/         
   FOR EACH itemfg WHERE itemfg.company    = cocode
                     AND itemfg.i-no       GE v-item[1]
                     AND itemfg.i-no       LE v-item[2]
                     AND itemfg.procat     GE v-cat[1]
                     AND itemfg.procat     LE v-cat[2]
                     AND ((itemfg.ord-policy     AND v-lot-reo = "R") OR
                          (NOT itemfg.ord-policy AND v-lot-reo = "L") OR v-lot-reo = "A")
                     AND ((itemfg.pur-man        AND v-pur-man = "P") OR
                          (NOT itemfg.pur-man    AND v-pur-man = "M") OR v-pur-man = "A") USE-INDEX i-no NO-LOCK:
      ASSIGN
         v-qty-avail = itemfg.q-onh + (IF v-inconh THEN itemfg.q-ono ELSE 0)
         v-alloc-qty = 0.

      IF v-totrel THEN 
         v-alloc-qty = itemfg.q-alloc.
      ELSE
         FOR EACH oe-ordl WHERE oe-ordl.company = cocode
                            AND oe-ordl.i-no    = itemfg.i-no USE-INDEX item NO-LOCK,
            EACH oe-rel WHERE oe-rel.company  = cocode
                          AND oe-rel.ord-no   = oe-ordl.ord-no
                          AND oe-rel.i-no     = oe-ordl.i-no
                          AND oe-rel.link-no  = 0
                          AND oe-rel.rel-date LE v-date USE-INDEX ord-item NO-LOCK:

            v-alloc-qty = v-alloc-qty + oe-rel.qty.
         END.

      v-qty-avail = v-qty-avail - v-alloc-qty.

      IF itemfg.ord-level GT v-qty-avail THEN
         FOR EACH oe-ordl WHERE oe-ordl.company = cocode
                            AND oe-ordl.i-no    = itemfg.i-no NO-LOCK,
            EACH oe-rel WHERE oe-rel.company = cocode
                          AND oe-rel.ord-no  = oe-ordl.ord-no
                          AND oe-rel.i-no    = oe-ordl.i-no
                          AND oe-rel.line    = oe-ordl.LINE NO-LOCK
                           BY oe-rel.rel-date DESC
                           BY oe-rel.r-no     DESC:     
            IF oe-rel.cust-no GE v-cust[1] AND
               oe-rel.cust-no LE v-cust[2] AND
               oe-rel.ship-id GE v-ship[1] AND
               oe-rel.ship-id LE v-ship[2] THEN DO:

               /* added logic for customer's inventory */
               ASSIGN 
                  v-tot-cust-qty = 0
                  v-tot-annual-cons = 0
                  v-first-rec = YES
                  v-total-cust-onhand-qty = 0.
               FOR EACH cust-itm WHERE cust-itm.company = cocode
                                   AND cust-itm.cust-no = oe-rel.cust-no
                                   AND cust-itm.i-no = oe-rel.i-no NO-LOCK:            
                  v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
                  IF v-first-rec THEN 
                     v-tot-annual-cons = /*v-tot-annual-cons + */ cust-itm.consum.
                  v-first-rec = no.
               END.
               ASSIGN 
                  v-tot-plant-qty = 0
                  v-first-record = YES.
               FOR EACH vend-whse-item WHERE vend-whse-item.company = cocode 
                                         AND TRIM(vend-whse-item.cust-no) = TRIM(itemfg.cust-no)
                                         AND TRIM(vend-whse-item.fg-item-no)   = TRIM(itemfg.i-no) NO-LOCK: 
                  v-tot-plant-qty = v-tot-plant-qty + vend-whse-item.plant-tot-oh-qty.
                  IF v-first-record THEN v-tot-annual-cons = v-tot-annual-cons + vend-whse-item.annual-usage-qty.
                  v-first-record = no.
               END.
               IF t-use-cust-inv = YES THEN
                  v-total-cust-onhand-qty = v-tot-cust-qty. ELSE v-total-cust-onhand-qty = 0.
               IF TG_customer-plant-inv = YES THEN
                  v-total-cust-onhand-qty = v-total-cust-onhand-qty + v-tot-plant-qty.
               /* = */          
               CREATE report.
               ASSIGN 
                  report.term-id = v-term
                  report.key-01  = oe-rel.cust-no
                  report.key-02  = oe-rel.ship-id
                  report.key-03  = itemfg.i-no
                  report.key-04  = STRING(v-alloc-qty,"-999999999999")
                  report.key-05  = STRING(v-qty-avail,"-999999999999")
                  report.key-06  = STRING(v-total-cust-onhand-qty,"-999999999999") 
                  report.key-07  = STRING(v-tot-annual-cons,"-999999999999")
                  report.rec-id  = RECID(oe-rel).
               LEAVE. 
            END.
         END.
   END. /* each itemfg */

   IF tb_excel THEN 
   DO: 
      EXPORT STREAM excel DELIMITER ","
                "Customer"
                "ShipTo"
                "Item"
                "Customer PO"
                "Order"
                "Unit Pr"
                "CustomerStock" 
                "OnHand"
                "PO/JobsQty Due"
                "OrderQtyReleased"
                "Available" 
                "MonthlyUsage"
                "Month ofCoverage" 
                "ShortQty" 
                "ReordLevel"
                "JobQty" 
                "NewQty" .
   END.

   /* = display information = */
   FOR EACH report WHERE report.term-id = v-term,
      FIRST oe-rel WHERE RECID(oe-rel) = report.rec-id NO-LOCK,
      FIRST oe-ordl WHERE oe-ordl.company = cocode
                      AND oe-ordl.ord-no  = oe-rel.ord-no
                      AND oe-ordl.i-no    = oe-rel.i-no
                      AND oe-ordl.line    = oe-rel.LINE NO-LOCK,
      FIRST itemfg WHERE itemfg.company = cocode
                     AND itemfg.i-no    = oe-rel.i-no NO-LOCK  
                BREAK BY report.key-01
                      BY report.key-02
                      BY report.key-03 TRANSACTION:
      ASSIGN  
         v-reord-qty = itemfg.ord-level - INT(report.key-05) - INT(report.key-06)
         v-rec-date  = ?
         v-coverage = (INT(report.key-05) + INT(report.key-06) ) / (INT(report.key-07) / 12)  /* month  weekly - 52 */
         v-mon-coverage = IF (INT(report.key-07) / 12) = ? THEN 0 ELSE (INT(report.key-07) / 12). 
      IF v-coverage = ? THEN v-coverage = 0.

      IF rd-tot-rel = "A" AND v-reord-qty < 0 THEN v-reord-qty = 0. 
      /* display even if v-reord-qty < 0 when rd-tot-rel = "A" */
      ls-key-06 = IF INT(report.key-06) <> 0 THEN STRING(INT(report.key-06),"->>>>>>9")  ELSE "________". 
      IF v-reord-qty >= 0 THEN              
         DISPLAY 
            oe-rel.cust-no    COLUMN-LABEL "Customer"
            oe-rel.ship-id    COLUMN-LABEL "Ship!To"
            oe-rel.i-no       COLUMN-LABEL "Item"
            oe-rel.po-no      COLUMN-LABEL "Customer PO"
            oe-rel.ord-no     COLUMN-LABEL "Order"
            oe-ordl.t-price / oe-ordl.qty COLUMN-LABEL "Unit Pr" FORMAT ">>>9.99<<"
            ls-key-06         COLUMN-LABEL "Customer!Stock" FORMAT "x(8)" /* Inventory*/                         
            itemfg.q-onh       COLUMN-LABEL "On!Hand" FORMAT "->>>>>>9"
            itemfg.q-ono WHEN v-inconh COLUMN-LABEL "PO/Jobs!Qty Due" FORMAT "->>>>>9"
            INT(report.key-04) COLUMN-LABEL "OrderQty!Released" FORMAT "->>>>>>9"
            INT(report.key-05) + INT(report.key-06) COLUMN-LABEL "Avail!able" FORMAT "->>>>>>9"
            v-mon-coverage    COLUMN-LABEL "Monthly!Usage"
            v-coverage        COLUMN-LABEL "Month of!Coverage" FORMAT "->>>>9.99"
            v-reord-qty       COLUMN-LABEL "Short!Qty" FORMAT ">>>>>9"
            itemfg.ord-level  COLUMN-LABEL "Reord!Level" FORMAT ">>>>>9"
            itemfg.ord-min    COLUMN-LABEL "Job!Qty" FORMAT ">>>>>>9"          
            v-new-qty         COLUMN-LABEL "New!Qty" 
         WITH DOWN NO-BOX WIDTH 180 STREAM-IO.

      IF tb_excel THEN 
      DO:
          EXPORT STREAM excel DELIMITER ","
            oe-rel.cust-no    
            oe-rel.ship-id    
            oe-rel.i-no       
            oe-rel.po-no      
            oe-rel.ord-no     
            oe-ordl.t-price / oe-ordl.qty 
            ls-key-06         
            itemfg.q-onh       
            (IF v-inconh THEN itemfg.q-ono ELSE 0)
            INT(report.key-04) 
            INT(report.key-05) + INT(report.key-06)
            v-mon-coverage    
            v-coverage        
            v-reord-qty       
            itemfg.ord-level  
            itemfg.ord-min    
            v-new-qty.         
      END.
      DELETE report.  
   END.
END. /* else */  

PUT "***** End of Report *****" AT 2 .

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE('').

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

