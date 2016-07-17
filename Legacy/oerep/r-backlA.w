&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-backl.w

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

def buffer xoe-ord for oe-ord.

{oe/rep/backlog1.i}

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-last-shipid AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-report LIKE report.

DEFINE STREAM excel.

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
begin_slsmn end_slsmn begin_ord-no end_ord-no begin_i-no end_i-no ~
begin_due-date end_due-date begin_user end_user tb_po-no tb_jobs ~
tb_detailed tb_qohgt0 tb_subt rd_date rd_sort rd_show rd_qoh rd_show2 ~
lv-ornt rd-dest lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_ord-no end_ord-no begin_i-no end_i-no begin_due-date ~
end_due-date begin_user end_user tb_po-no tb_jobs tb_detailed tb_qohgt0 ~
lbl-blank tb_subt rd_date lbl_sort rd_sort lbl_show1 rd_show lbl_qoh rd_qoh ~
lbl_sort-2 rd_show2 lv-ornt rd-dest lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-last-shipto C-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_user AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning Order User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_user AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Order User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-backl.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl-blank AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_qoh AS CHARACTER FORMAT "X(256)":U INITIAL "Qty On-hand Source?" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show1 AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Display?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_date AS CHARACTER INITIAL "Rel" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Release Date", "Rel",
"Last Ship Date", "Ship"
     SIZE 36.2 BY 1 NO-UNDO.

DEFINE VARIABLE rd_qoh AS CHARACTER INITIAL "FG" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"Job", "Job"
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Status" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Price/Sales$", "Price/Sales$",
"Status", "Status",
"PO Receipt Qty", "PO Receipt Qty"
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show2 AS CHARACTER INITIAL "Order#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order#", "Order#",
"Job#", "Job#"
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Due Date", "Due Date",
"Sales Rep#", "Sales Rep#"
     SIZE 51 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 14.52.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_jobs AS LOGICAL INITIAL yes 
     LABEL "Include Jobs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_po-no AS LOGICAL INITIAL yes 
     LABEL "Print PO#" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_qohgt0 AS LOGICAL INITIAL no 
     LABEL "Include all Items w/QOH>0?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_subt AS LOGICAL INITIAL no 
     LABEL "Item Subtotals? (Summary Only)" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 1.95 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 1.95 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 2.91 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 2.91 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_ord-no AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 3.86 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 4.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.81 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_due-date AT ROW 5.76 COL 27 COLON-ALIGNED
     end_due-date AT ROW 5.76 COL 70 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_user AT ROW 6.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning User ID"
     end_user AT ROW 6.71 COL 70 COLON-ALIGNED HELP
          "Enter Ending User ID"
     tb_po-no AT ROW 8.14 COL 8
     tb_jobs AT ROW 8.14 COL 51
     tb_detailed AT ROW 9.1 COL 22 RIGHT-ALIGNED
     tb_qohgt0 AT ROW 9.1 COL 51
     lbl-blank AT ROW 10 COL 41.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     tb_subt AT ROW 10.05 COL 8
     rd_date AT ROW 10.05 COL 51 NO-LABEL
     lbl_sort AT ROW 11.24 COL 27 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 11.24 COL 36 NO-LABEL
     lbl_show1 AT ROW 12.19 COL 26 COLON-ALIGNED NO-LABEL
     rd_show AT ROW 12.19 COL 36 NO-LABEL
     lbl_qoh AT ROW 13.14 COL 11 COLON-ALIGNED NO-LABEL
     rd_qoh AT ROW 13.14 COL 36 NO-LABEL
     lbl_sort-2 AT ROW 14.1 COL 23 COLON-ALIGNED NO-LABEL
     rd_show2 AT ROW 14.1 COL 36 NO-LABEL
     lv-ornt AT ROW 16.1 COL 25.4 NO-LABEL
     rd-dest AT ROW 16.71 COL 4 NO-LABEL
     lines-per-page AT ROW 17.1 COL 80.8 COLON-ALIGNED
     lv-font-no AT ROW 17.14 COL 29 COLON-ALIGNED
     lv-font-name AT ROW 18.14 COL 23 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.57 COL 24.8
     tb_excel AT ROW 20.71 COL 41.6
     tb_runExcel AT ROW 20.71 COL 83.6 RIGHT-ALIGNED
     fi_file AT ROW 21.67 COL 39.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.33 COL 24
     btn-cancel AT ROW 24.33 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.76 COL 3
          BGCOLOR 8 FGCOLOR 1 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 15.52 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.24.


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
         TITLE              = "Order Backlog"
         HEIGHT             = 25.57
         WIDTH              = 96
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = 9
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl-blank IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl-blank:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl-blank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_date".

/* SETTINGS FOR FILL-IN lbl_qoh IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qoh".

/* SETTINGS FOR FILL-IN lbl_show1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_sort-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show2".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_detailed IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_jobs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_qohgt0:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_subt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order Backlog */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Backlog */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user C-Win
ON LEAVE OF begin_user IN FRAME FRAME-A /* Beginning Order User ID */
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
  STATUS DEFAULT "Processing Complete". 
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user C-Win
ON LEAVE OF end_user IN FRAME FRAME-A /* Ending Order User ID */
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


&Scoped-define SELF-NAME rd_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_date C-Win
ON VALUE-CHANGED OF rd_date IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qoh C-Win
ON VALUE-CHANGED OF rd_qoh IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show2 C-Win
ON VALUE-CHANGED OF rd_show2 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
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


&Scoped-define SELF-NAME tb_jobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_jobs C-Win
ON VALUE-CHANGED OF tb_jobs IN FRAME FRAME-A /* Include Jobs? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_po-no C-Win
ON VALUE-CHANGED OF tb_po-no IN FRAME FRAME-A /* Print PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qohgt0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qohgt0 C-Win
ON VALUE-CHANGED OF tb_qohgt0 IN FRAME FRAME-A /* Include all Items w/QOH>0? */
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


&Scoped-define SELF-NAME tb_subt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subt C-Win
ON VALUE-CHANGED OF tb_subt IN FRAME FRAME-A /* Item Subtotals? (Summary Only) */
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
   begin_due-date = date(1,1,year(today))
   end_due-date   = today.

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_ord-no 
          end_ord-no begin_i-no end_i-no begin_due-date end_due-date begin_user 
          end_user tb_po-no tb_jobs tb_detailed tb_qohgt0 lbl-blank tb_subt 
          rd_date lbl_sort rd_sort lbl_show1 rd_show lbl_qoh rd_qoh lbl_sort-2 
          rd_show2 lv-ornt rd-dest lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn 
         begin_ord-no end_ord-no begin_i-no end_i-no begin_due-date 
         end_due-date begin_user end_user tb_po-no tb_jobs tb_detailed 
         tb_qohgt0 tb_subt rd_date rd_sort rd_show rd_qoh rd_show2 lv-ornt 
         rd-dest lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
      /*   CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.
    */
    
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
/* Order Backlog Summary / Detail Report                                      */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}

def var v-fcust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-fslsm like oe-ord.sman extent 2 init ["","zzz"].
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/01,today].
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ponum as log init yes.
def var v-sort as char format "!" init "C".
def var v-sumdet as log format "Summary/Detail" init yes.
def var v-sub-item as log.
def var v-price AS INT.
def var v-jobs as log init yes.
def var v-all as log init no.
def var v-fg-qty as log init yes format "FG/Job".
def var v-ord-job as log init yes format "Order/Job".

def var v-profit as log init yes.
def var v-password as char no-undo.
def var security-flag as log no-undo.

def var v-qty as int extent 2.
def var v-cost as dec.
def var v-tot-qty as int format "->>>,>>>,>>9" extent 2.
def var v-tot-cost as dec format  "->>>,>>>,>>9.99" extent 2.
def var v-tot-sales as dec format "->>>,>>>,>>9.99" extent 2.
def var v-tot-pct as dec format "->>,>>9.99".
def var v-head as char format "x(145)" extent 4.
def var v-gpdollar as dec format  "->>>,>>>,>>9.99".
def var v-gp as dec.
def var v-uom as char format "x(4)".
def var v-qty-pal as int.
def var fstat like oe-ord.stat.
def var tstat like fstat init "".
def var v-job-no as char format "x(9)".
DEF VAR li-qty AS INT EXTENT 2 NO-UNDO.
DEF VAR v-date AS CHAR NO-UNDO.
DEF VAR lv-tmp-string AS CHAR NO-UNDO.

DEFINE VARIABLE chrDummy AS CHARACTER  NO-UNDO INIT "".
DEFINE VARIABLE chrSalesCustAndName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

format header
  v-head[1] skip
  v-head[2]
  fill("_",145) format "x(145)"
  with no-labels no-box no-underline stream-io width 147 frame f-top page-top.

format header
  fill("_",135) format "x(133)"
  with no-labels no-box no-underline stream-io width 135 frame f-topd page-top.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no    
  w-ord.po-num
  w-ord.i-name  format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price SPACE(3) v-last-shipid skip
  with frame ordhead-po no-labels no-box no-underline stream-io width 160.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price SPACE(3) v-last-shipid skip
  with frame ordhead no-labels no-box no-underline stream-io width 142.
  
format
  w-ord.due-date
  w-ord.ord-date
  v-job-no           format "x(9)"
  w-ord.po-num
  w-ord.i-name       format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  space(3)
  w-ord.stat
  space(3)
  w-ord.rel-date
  space(3)
  w-ord.rel-stat
  SPACE(3)
  v-last-shipid
  skip
  with frame ordhead-po-s no-labels no-box no-underline stream-io width 160.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was w-ord.ord-no */
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  space(3)
  w-ord.stat
  space(3)
  w-ord.rel-date
  space(3)
  w-ord.rel-stat
  SPACE(3)
  v-last-shipid
  skip
  with frame ordhead-s no-labels no-box no-underline stream-io width 142.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was v-job-no */          format "x(9)"
  w-ord.po-num
  w-ord.i-name       format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  space(6)
  w-ord.po-received
  SPACE(3)
  v-last-shipid
  skip
  with frame ordhead-po-q no-labels no-box no-underline stream-io width 160.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was w-ord.ord-no */
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  space(6)
  w-ord.po-received
  SPACE(3)
  v-last-shipid
  skip
  with frame ordhead-q no-labels no-box no-underline stream-io width 142.

format
  skip(1)
  v-job-no label "Order No." colon 10 space(1)
  w-ord.due-date label "Date" space(1)
  w-ord.cust-no label "Customer" "-"
  w-ord.cust-name no-labels format "x(25)" space(1)
  w-ord.sman label "Sales Rep" skip(1)
 with frame detailhead side-labels no-box no-underline stream-io width 135.

format
  skip(1)
  v-job-no label "Order No." colon 10 space(1)
  w-ord.due-date label "Date" space(1)
  w-ord.cust-no label "Customer" "-"
  w-ord.cust-name no-labels format "x(25)" space(1)
  w-ord.sman label "Sales Rep" space(1)
  w-ord.po-num label "PO #" skip(1)
  with frame detailhead-po side-labels no-box no-underline stream-io width 135.

format
  w-ord.i-name label "Description" format "x(25)"
  w-ord.est-no label "Estimate#" format "x(8)"
  w-ord.qty-due label "Qty Due"
  w-ord.stat label "Status"
  w-ord.cost label "Std. Cost"
  w-ord.t-price label "Sales"
  v-gpdollar label "GP Dollars"
  v-gp label "GP%"
  with frame ordline no-box no-underline down stream-io width 125.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BACKLOG"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BACKLOG"
   sys-ctrl.descrip = "Print Cost & Profit on Order Backlog Report?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-profit = sys-ctrl.log-fld.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-fcust[1]   = begin_cust-no
 v-fcust[2]   = end_cust-no
 v-fslsm[1]   = begin_slsmn
 v-fslsm[2]   = end_slsmn
 v-ford-no[1] = begin_ord-no
 v-ford-no[2] = end_ord-no
 v-fitem[1]   = begin_i-no
 v-fitem[2]   = end_i-no
 v-fdate[1]   = begin_due-date
 v-fdate[2]   = end_due-date
 v-ponum      = tb_po-no
 v-sort       = if rd_sort eq "Customer#" then "C" else
                if rd_sort eq "Due Date"  then "D" else "S"
 v-sumdet     = not tb_detailed
 v-sub-item   = tb_subt
 v-price      = lookup(rd_show,"Price/Sales$,Status,PO Receipt Qty")
 v-jobs       = tb_jobs
 v-all        = tb_qohgt0
 v-fg-qty     = rd_qoh eq "FG"
 v-ord-job    = rd_show2 EQ "order#"
 v-date       = rd_date.

if v-price EQ 1 then do:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
end.
SESSION:SET-WAIT-STATE ("general").

IF v-date EQ "Rel" THEN
   lv-tmp-string = "     Price          Sales $,  Status  Rel Date & Stat,   PO Qty Received".
ELSE
   lv-tmp-string = "     Price          Sales $,  Status  L Shp Dt & Stat,   PO Qty Received".

assign
 v-head[4] = if v-ord-job then "Order" else " Job "
 v-head[3] = ENTRY(v-price,lv-tmp-string).
     
if not v-ord-job then
  assign
   v-job-no:label in frame detailhead    = "Job#"
   v-job-no:label in frame detailhead-po = "Job#".

if v-sumdet and v-ponum then
  assign v-head[1] =
    "Due      Order    " + v-head[4] + "     PO                                "
 + "      Item                   Qty          Qty                              "
           v-head[2] =
    "Date     Date     Number    Number          Description             Number"
 + "             On-hand          Due" + v-head[3] + " Lst ShipTo".

else if v-sumdet and not v-ponum then
  assign v-head[1] =
    "Due      Order    " + v-head[4] + "                                  Item "
    + "                    Qty          Qty                         "
           v-head[2] =
    "Date     Date     Number       Description               Number "
    + "              On-hand          Due" + v-head[3] + " Lst ShipTo".

ELSE
   ASSIGN v-head[1] = ""
   v-head[2] = "".

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
if td-show-parm then run show-param.

display "" with frame r-top.
if v-sumdet then 
  display "" with frame f-top.
else 
  display "" with frame f-topd.

/* 
FOR EACH tt-report:
  DELETE tt-report.
END.
 */

EMPTY TEMP-TABLE tt-report.

IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).

{oerep/r-backl.i} 

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel CLOSE.

  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-last-shipto C-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Task 09111201 
Add new field called  "Last Shipto" on  O-Q-1.
This will print the last or next shipto code form the Release Folder.

When multiple release lines exist for the item, the program will look for a release in the following priority:
Release Status C for Completed.
Release Status Z for Invoiced.
Release Status P for posted release that is in the bill of lading file.
Release Status B for back ordered release
Release Status A for Actual Release.
Release Status I for Invoicable / past warehouse terms.
Release Status L for late.
Release Status S for released.

The logic is to print the history of the shipment first back to the release status.
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE BUFFER buf-oe-rel FOR oe-rel.

  FOR EACH buf-oe-rel NO-LOCK WHERE 
    buf-oe-rel.company EQ cocode AND
    buf-oe-rel.ord-no  EQ oe-ordl.ord-no AND
    buf-oe-rel.i-no    EQ oe-ordl.i-no   AND
    buf-oe-rel.line    EQ oe-ordl.LINE
    BY (IF buf-oe-rel.stat = "C" THEN 1 
        ELSE IF buf-oe-rel.stat = "Z" THEN 2 
        ELSE IF buf-oe-rel.stat = "P" THEN 3 
        ELSE IF buf-oe-rel.stat = "B" THEN 4 
        ELSE IF buf-oe-rel.stat = "A" THEN 5 
        ELSE IF buf-oe-rel.stat = "I" THEN 6 
        ELSE IF buf-oe-rel.stat = "L" THEN 7 
        ELSE IF buf-oe-rel.stat = "S" THEN 8         
        ELSE 12)
    :
 
        RETURN buf-oe-rel.ship-id.
        LEAVE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

