&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-inctsh.w

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

def var fcust as ch init "" no-undo.
def var tcust like fcust init "zzzzzzzz" no-undo.
def var fship as ch init "" no-undo.
def var tship like fcust init "zzzzzzzz" no-undo.
def var fshpz as ch format "x(10)" init "" no-undo.
def var tshpz like fcust init "zzzzzzzzzz" no-undo.
def var fitem like itemfg.i-no init " " no-undo.
def var titem like fitem init "zzzzzzzzzzzzzzzzzzz" no-undo.
def var fsman as char format "x(3)" init "" no-undo.
def var tsman like fsman init "zzz" no-undo.
def var fdate as date format "99/99/9999" no-undo.
def var tdate like fdate no-undo.
def var v-sort1 as char format "!" init "I" no-undo.
def var v-disc-p as log init NO no-undo.
def var v-freight as log init NO no-undo.
def var v-inc-fc as log init NO no-undo.

def var v-date like ar-inv.inv-date column-label "Invoice!Date" no-undo.
def var v-ord  like ar-invl.ord-no column-label "Order!Number" no-undo.
def var v-pric like ar-invl.unit-pr column-label "Unit Price" no-undo.
def var v-uom  like ar-invl.pr-uom column-label "UOM" no-undo.

def var v-sman-no as   char format "x(3)" no-undo.

def var v-exc     as   LOG no-undo.
def var v-name    like cust.name format "x(21)" no-undo.

def var v-pct as dec format "99.99" no-undo.
def var v-fac as int no-undo.
def var v-ship like ar-inv.ship-id no-undo.
def var v-shpz like ar-inv.sold-zip no-undo.
def var v-disc like ar-invl.disc no-undo.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF BUFFER xreport FOR tt-report.

def TEMP-TABLE w-data NO-UNDO
  field i-no      like ar-invl.i-no column-label "FG Item"
  field inv-no    like ar-invl.inv-no column-label "Invoice!Number"
  field rec-id    as recid.
 
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF STREAM st-excel.

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
begin_cust-type end_cust-type begin_ship-to end_ship-to begin_slsmn ~
end_slsmn begin_i-no end_i-no begin_inv-date end_inv-date rd_sort ~
tb_disprice tb_fin-chg lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_cust-type ~
end_cust-type begin_ship-to end_ship-to begin_slsmn end_slsmn begin_i-no ~
end_i-no begin_inv-date end_inv-date lbl_sort rd_sort tb_disprice ~
tb_fin-chg lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Ship-to#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Ship-to #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-inctsh.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     FGCOLOR 9 .

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
     SIZE 31 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Ship-to#", "Ship-to#",
"Item#", "Item#",
"Order#", "Order#"
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 12.62.

DEFINE VARIABLE tb_disprice AS LOGICAL INITIAL no 
     LABEL "Show Discounted Prices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 3.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     begin_ship-to AT ROW 4.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Ship-to Number"
     end_ship-to AT ROW 4.33 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_slsmn AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.29 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_i-no AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 6.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_inv-date AT ROW 7.19 COL 27 COLON-ALIGNED
     end_inv-date AT ROW 7.19 COL 70 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     lbl_sort AT ROW 9.1 COL 20 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 9.1 COL 31 NO-LABEL
     tb_disprice AT ROW 11.24 COL 29
     tb_fin-chg AT ROW 11.24 COL 61
     lv-ornt AT ROW 14.33 COL 31 NO-LABEL
     lines-per-page AT ROW 14.33 COL 85 COLON-ALIGNED
     rd-dest AT ROW 14.57 COL 5 NO-LABEL
     lv-font-no AT ROW 15.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.91 COL 30
     tb_excel AT ROW 19.1 COL 67 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.1 COL 91 RIGHT-ALIGNED
     fi_file AT ROW 20.29 COL 45 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.67 COL 19
     btn-cancel AT ROW 22.67 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.86 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 13.62 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.24
         SIZE 96.4 BY 23.86.


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
         TITLE              = "Sales Analysis - By Inv/Cat/Shipto"
         HEIGHT             = 24.14
         WIDTH              = 96.4
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
       begin_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_disprice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Analysis - By Inv/Cat/Shipto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - By Inv/Cat/Shipto */
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


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type C-Win
ON LEAVE OF begin_cust-type IN FRAME FRAME-A /* Beginning Cust Type */
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


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to C-Win
ON LEAVE OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-to# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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

  RUN run-report.
  STATUS DEFAULT "Processing Complete".
  IF tb_excel AND tb_runExcel THEN
  OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:TITLE 
                            &fax-file=list-name }
       END. 
       when 5 then do:
           
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject= c-win:TITLE   
                                  &mail-body=  c-win:TITLE  
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


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type C-Win
ON LEAVE OF end_cust-type IN FRAME FRAME-A /* Ending Cust Type */
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


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to C-Win
ON LEAVE OF end_ship-to IN FRAME FRAME-A /* Ending Ship-to # */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select File to Save "
                 filters "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 initial-dir "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME tb_disprice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disprice C-Win
ON VALUE-CHANGED OF tb_disprice IN FRAME FRAME-A /* Show Discounted Prices? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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
   begin_inv-date = DATE(1,1,YEAR(TODAY))
   end_inv-date   = TODAY.

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
   
    /* skb - 1/3/07 - defauting the values */
/*     ASSIGN                                            */
/*         tb_excel:CHECKED     = TRUE                   */
/*         tb_runexcel:CHECKED  = TRUE                   */
/*         fi_file:SCREEN-VALUE = "c:\tmp\r-inctsh.csv". */
  END.



  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ip-recid  as   recid            no-undo.
def input parameter ip-key-03 like tt-report.key-03 no-undo.
def input parameter ip-key-04 like tt-report.key-04 no-undo.
def input parameter ip-key-10 like tt-report.key-10 no-undo.


create xreport.
  
assign
 v-exc           = no
 xreport.term-id = ""
 xreport.rec-id  = ip-recid
 xreport.key-01  = trim(if v-sort1 eq "Z" then cust.zip else "") +
                   tt-report.key-09
 xreport.key-02  = if v-sort1 eq "H" then (v-shpz + v-ship) else
                   if v-sort1 eq "S" then v-ship else
                   if v-sort1 eq "O" then
                     string(ar-invl.ord-no,"999999") else ""
 xreport.key-03  = ip-key-03
 xreport.key-04  = ip-key-04
 xreport.key-05  = v-ship
 xreport.key-06  = v-sman-no
 xreport.key-07  = xreport.key-03
 xreport.key-09  = tt-report.key-09
 xreport.key-10  = ip-key-10.

if xreport.key-02 eq "" and v-sort1 ne "I" then
  if v-sort1 eq "C" then
    xreport.key-02 = xreport.key-05.
  else
    assign
     xreport.key-02 = xreport.key-03
     xreport.key-03 = xreport.key-05.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report1 C-Win 
PROCEDURE create-report1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ip-recid  as   recid         no-undo.
def input parameter ip-key-03 like report.key-03 no-undo.
def input parameter ip-key-04 like report.key-04 no-undo.
def input parameter ip-key-10 like report.key-10 no-undo.


do i = 1 to 3:
  v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
              else ar-invl.sman[i].
              
  if v-sman-no   lt fsman                         or
     v-sman-no   gt tsman                         or
     (i ne 1 and
      (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

  run create-report (ip-recid, ip-key-03, ip-key-04, ip-key-10).
  LEAVE.
end.

 

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
  DISPLAY begin_cust-no end_cust-no begin_cust-type end_cust-type begin_ship-to 
          end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no begin_inv-date 
          end_inv-date lbl_sort rd_sort tb_disprice tb_fin-chg lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_cust-type end_cust-type 
         begin_ship-to end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no 
         begin_inv-date end_inv-date rd_sort tb_disprice tb_fin-chg lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
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
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF VAR v-qty     AS   INT EXTENT 2 COLUMN-LABEL "Qty Shipped".
DEF VAR v-amt     AS   DEC EXTENT 2 COLUMN-LABEL "Invoice Amt".
DEF VAR lv-r-no   LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type   AS   CHAR NO-UNDO.
DEF VAR v-year    AS   INT EXTENT 2 NO-UNDO.
DEF VAR v-month   AS   INT NO-UNDO.
DEF VAR v-cust    LIKE shipto.cust-no NO-UNDO.
/* DEF VAR v-ship    LIKE shipto.ship-id NO-UNDO. */
DEF VAR v-city    LIKE shipto.ship-city NO-UNDO.
DEF VAR v-state   LIKE shipto.ship-state NO-UNDO.
DEF VAR v-sname   LIKE sman.sname NO-UNDO.
DEF VAR v-fgcat   LIKE fgcat.procat NO-UNDO.
DEF VAR v-delimiter AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR li-seq AS INT NO-UNDO.

{sys/form/r-topw.f}

FORM cust.cust-no       COLUMN-LABEL "!Customer!    #"
     v-year[1]          COLUMN-LABEL "! !Year"
                        FORMAT "9999"
     v-name             COLUMN-LABEL "!Customer Buying!    Location"
                        FORMAT "x(25)"
     tt-report.key-05   COLUMN-LABEL "! !Ship-to"
                        FORMAT "x(8)"
     v-city             COLUMN-LABEL "!Cust Buy!Loc City"
     v-state            COLUMN-LABEL "Cust Buy!  Loc! State"
     v-sname            COLUMN-LABEL "! !Rep"
     w-data.inv-no      COLUMN-LABEL "!  Inv!Number"
     v-month            COLUMN-LABEL "! !Month"
                        FORMAT "99"
     v-year[2]          COLUMN-LABEL "! !Year"
                        FORMAT "9999"
     v-date             COLUMN-LABEL "! !Inv Date"
                        FORMAT "99/99/9999"
     w-data.i-no        COLUMN-LABEL "! !FG Item#"
     v-fgcat            COLUMN-LABEL " House!Product! Code"
     v-ord              COLUMN-LABEL "!  SO!Number"
                        FORMAT ">>>>>>"
     v-qty[1]           COLUMN-LABEL "!  Qty!Shipped"
                        FORMAT "->>>,>>>,>>>"
     v-pric             COLUMN-LABEL "! Unit!Price"
                        FORMAT "->>>,>>>,>>9.99<<"
     v-uom              COLUMN-LABEL "! !UOM"
     v-amt[1]           COLUMN-LABEL "!Invoice!  Amt"
                        FORMAT "->,>>>,>>>,>>9.99"

   WITH NO-BOX FRAME itemx DOWN STREAM-IO WIDTH 210.


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 fcust      = begin_cust-no
 tcust      = end_cust-no
 fship      = begin_ship-to
 tship      = end_ship-to
 fsman      = begin_slsmn
 tsman      = END_slsmn
 fitem      = begin_i-no
 titem      = end_i-no
 fdate      = begin_inv-date
 tdate      = end_inv-date
 v-sort1    = SUBSTR(rd_sort,1,1) 
 v-disc-p   = tb_disprice              
 v-freight  = NO
 v-inc-fc   = tb_fin-chg.
 
{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel AND fi_file NE '' THEN DO:
  OUTPUT STREAM st-excel TO VALUE(fi_file).
  PUT STREAM st-excel
      ",,,,,Cust Buy,,,,,,,House,,,,,"
      SKIP
      "Customer,,Customer Buying,,Cust Buy,Loc,,Inv,,,,,Product,SO,Qty,Unit,,Invoice"
      SKIP
      "#,Year,Location,Ship To,Loc City,State,Rep,Number,Month,Year,Inv Date,FG Item#,Code,Number,Shipped,Price,UOM,Amt"
      SKIP.
END.

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

FOR EACH tt-report:
  DELETE tt-report.
END.

  for each cust
      where cust.company eq cocode
        and cust.cust-no ge fcust
        and cust.cust-no le tcust
        and cust.type    ge begin_cust-type
        and cust.type    le end_cust-type
      use-index cust no-lock:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
   {sa/sa-sls03.i "fdate" "tdate"}    
  end.
 
  for each tt-report
      where tt-report.term-id eq ""
        and tt-report.key-01  eq ""
        and tt-report.key-02  eq ""
        and tt-report.key-03  eq ""
        and tt-report.key-04  eq ""
        and tt-report.key-05  eq ""
        and tt-report.key-06  eq ""
        and tt-report.key-07  eq ""
        and tt-report.key-08  eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      transaction:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    if tt-report.key-10 eq "ar-inv" then do:
      find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

      run ship-data.

      if v-ship ge fship and
         v-ship le tship and
         v-shpz ge fshpz and
         v-shpz le tshpz then do:
         
        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
              and ar-invl.i-no    ge fitem
              and ar-invl.i-no    le titem
              and (ar-invl.billable or not ar-invl.misc)
            use-index x-no no-lock:
            
          run create-report1 (recid(ar-invl),
                              if ar-invl.misc then ar-invl.i-name else
                              if ar-invl.i-no ne "" then ar-invl.i-no else
                              "AR SALE",
                              string(ar-inv.inv-no,"999999"), "").
        end.
         
        if v-freight and ar-inv.f-bill then do:
          find first ar-invl where ar-invl.x-no eq ar-inv.x-no
              use-index x-no no-lock no-error.
              
          if avail ar-invl then do:
            v-sman-no = "".
                      
            do i = 1 to 3:
              if ar-invl.sman[i] ne "" then do:
                v-sman-no = ar-invl.sman[i].
                leave.
              end.
            end.
          
            if v-sman-no eq "" then v-sman-no = cust.sman.
          
            if "freight" ge fitem                      and
               "freight" le titem                      and
               v-sman-no ge fsman                      and
               v-sman-no le tsman                      then
               
              run create-report (recid(ar-invl), "FREIGHT",
                                 string(ar-inv.inv-no,"999999"), "FREIGHT").
          end.
        end.
      end.

      delete tt-report.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      assign
       v-exc            = yes
       tt-report.key-01 = trim(if v-sort1 eq "Z" then cust.zip else "") +
                          tt-report.key-09
       tt-report.key-02 = if v-sort1 ne "I" then
                            (if v-sort1 eq "H" then cust.zip else "") +
                             tt-report.key-09
                          else ""
       tt-report.key-03 = "MEMO"
       tt-report.key-04 = string(ar-cashl.inv-no,"999999")
       tt-report.key-05 = tt-report.key-09
       tt-report.key-06 = cust.sman
       tt-report.key-07 = tt-report.key-03.

      release ar-inv.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      ASSIGN
       lv-r-no = 0
       lv-type = "".
          
      IF AVAIL reftable THEN
        ASSIGN
         lv-r-no = reftable.val[1]
         lv-type = reftable.dscr.
      ELSE
      IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
        ASSIGN
         lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
         lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

      IF lv-r-no NE 0 THEN DO:
        find first oe-reth
            where oe-reth.company eq cocode
              and oe-reth.r-no    eq lv-r-no
            no-lock no-error.
        if avail oe-reth then
        find first ar-inv
             where ar-inv.company eq cocode
               and ar-inv.cust-no eq oe-reth.cust-no
               and ar-inv.inv-no  eq oe-reth.inv-no
             no-lock no-error.
      end.       

      if avail ar-inv then do:
        run ship-data.

        if v-ship ge fship and
           v-ship le tship and
           v-shpz ge fshpz and
           v-shpz le tshpz then
        if lv-type eq "items" then do:
          release ar-invl.
          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq oe-reth.r-no
                and oe-retl.line    eq ar-cashl.line
                and oe-retl.i-no    ge fitem
                and oe-retl.i-no    le titem
              no-lock no-error.
          if avail oe-retl then
          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
                and (ar-invl.billable or not ar-invl.misc)
              no-lock no-error.
          if avail ar-invl then do:
            run create-report1 (recid(ar-cashl), oe-retl.i-no,
                                tt-report.key-04, "").
                
            delete tt-report.
          end.
        end.

        else
        if lv-type   eq "freight"                  and
           "freight" ge fitem                      and
           "freight" le titem                      and
           cust.sman ge fsman                      and
           cust.sman le tsman                      and
           v-freight                               then
          assign
           v-exc            = no
           tt-report.key-02 = if v-sort1 ne "I" then v-ship else ""
           tt-report.key-03 = "FREIGHT"
           tt-report.key-05 = v-ship.

        else
        if lv-type   eq "tax"                  and
           "tax"     ge fitem                  and
           "tax"     le titem                  and
           cust.sman ge fsman                  and
           cust.sman le tsman                  then
          assign
           v-exc            = no
           tt-report.key-02 = if v-sort1 ne "I" then v-ship else ""
           tt-report.key-03 = "TAX"
           tt-report.key-05 = v-ship.

        else
        if ""        ge fitem and
           ""        le titem and
           cust.sman ge fsman and
           cust.sman le tsman then v-exc = no.
      end.

      else
      if ""               ge fitem and
         ""               le titem and
         cust.sman        ge fsman and
         cust.sman        le tsman and
         ar-cashl.cust-no ge fship and
         ar-cashl.cust-no le tship and
         cust.zip         ge fshpz and
         cust.zip         le tshpz then v-exc = no.

      if avail tt-report then do:
        tt-report.key-07 = tt-report.key-03.
        
        if v-exc then delete tt-report.

        else
        if tt-report.key-02 eq "" and v-sort1 ne "I" then
          if v-sort1 eq "C" then
            tt-report.key-02 = tt-report.key-05.
          else
            assign
             tt-report.key-02 = tt-report.key-03
             tt-report.key-03 = tt-report.key-05.
      end.     
    end.
  end.

  for each tt-report where tt-report.term-id eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05

      with frame itemx down

      transaction:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    create w-data.
    assign
     w-data.i-no   = tt-report.key-07
     w-data.inv-no = int(tt-report.key-04)
     w-data.rec-id = tt-report.rec-id.

    find first ar-invl
        where recid(ar-invl) eq w-data.rec-id
        no-lock no-error.

    if avail ar-invl then do:
      find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
      assign
       v-date   = ar-inv.inv-date
       v-ord    = ar-invl.ord-no
       v-pric   = ar-invl.unit-pr
       v-uom    = ar-invl.pr-uom
       v-qty[1] = ar-invl.ship-qty
       v-amt[1] = ar-invl.amt
       v-disc   = ar-invl.disc
       v-pct    = 1.

      if tt-report.key-10 eq "FREIGHT" then
        assign
         v-pric   = ar-inv.freight
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-inv.freight
         v-disc   = 0.
      
      else do:
        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report.key-06 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

        if v-pct eq 0 then
        do i = 1 to 3:
          if i eq 1 then j = 0.
          if ar-invl.sman[i] ne "" then j = j + 1.
          if i eq 3 then v-pct = 1 / j.
        end.

        if v-pct le 0 or v-pct eq ? then v-pct = 1.
      end.

      v-amt[1] = v-amt[1] * v-pct.
    end.

    else do:
      find first ar-cashl
          where recid(ar-cashl) eq w-data.rec-id
          no-lock no-error.

      if avail ar-cashl then do:
        find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

        assign
         v-date   = ar-cash.check-date
         v-ord    = 0
         v-pric   = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-disc   = 0.

        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        if avail oe-retl then do:
          assign
           v-ord    = oe-retl.ord-no
           v-pric   = oe-retl.unit-pr
           v-uom    = oe-retl.uom
           v-qty[1] = - oe-retl.tot-qty-return.

          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
              no-lock no-error.

          if avail ar-invl then do:
            /* Added for decimal problem */
            assign v-pric   = ar-invl.unit-pr.

            do i = 1 to 3:
              if ar-invl.sman[i] eq tt-report.key-06 then
                assign
                 v-pct = ar-invl.s-pct[i] / 100
                 i     = 3.
            end.

            if v-pct eq 0 then
            do i = 1 to 3:
              if i eq 1 then j = 0.
              if ar-invl.sman[i] ne "" then j = j + 1.
              if i eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.
              assign
               v-amt[1] = v-amt[1] * v-pct
               v-disc   = ar-invl.disc.
          end.
        end.
      end.
    end.

    IF v-disc-p AND v-disc ne 0 THEN v-pric = v-pric * (100 - v-disc) / 100.

    FIND FIRST shipto
        WHERE shipto.company EQ cust.company
          AND shipto.cust-no EQ cust.cust-no
          AND shipto.ship-id EQ tt-report.key-05
        NO-LOCK NO-ERROR.

    IF AVAIL shipto THEN
      ASSIGN
       v-name  = shipto.ship-name
       v-city  = shipto.ship-city
       v-state = shipto.ship-state.
    ELSE
      ASSIGN
       v-name  = cust.name
       v-city  = cust.city
       v-state = cust.state.

    FIND FIRST itemfg
        WHERE itemfg.company EQ cust.company
          AND itemfg.i-no    EQ w-data.i-no
        NO-LOCK NO-ERROR.

    FIND FIRST sman
        WHERE sman.company EQ cust.company
          AND sman.sman    EQ tt-report.key-06
        NO-LOCK NO-ERROR.

    ASSIGN
     v-fgcat = IF AVAIL itemfg THEN itemfg.procat ELSE ""
     v-sname = IF AVAIL sman THEN sman.sname ELSE tt-report.key-06
     v-year  = YEAR(v-date)
     v-month = MONTH(v-date).


    
    DISPLAY cust.cust-no
            v-year[1]
            v-name
            tt-report.key-05
            v-city
            v-state
            v-sname
            w-data.inv-no
            v-month
            v-year[2]
            v-date
            w-data.i-no
            v-fgcat
            v-ord
            v-qty[1]
            v-pric
            v-uom
            v-amt[1].
    DOWN.

    IF tb_excel THEN DO:
      ASSIGN
       v-cust = cust.cust-no
       v-ship = tt-report.key-05.

      RUN uncomma (INPUT-OUTPUT v-cust).
      RUN uncomma (INPUT-OUTPUT v-name).
      RUN uncomma (INPUT-OUTPUT v-ship).
      RUN uncomma (INPUT-OUTPUT v-city).
      RUN uncomma (INPUT-OUTPUT v-state).
      RUN uncomma (INPUT-OUTPUT v-sname).
      RUN uncomma (INPUT-OUTPUT w-data.i-no).
      RUN uncomma (INPUT-OUTPUT v-fgcat).
      RUN uncomma (INPUT-OUTPUT v-uom).

      PUT STREAM st-excel
          '"' REPLACE(v-cust, '"', "")                '",'
          '"' v-year[1]                FORMAT "9999"  '",'
          '"' REPLACE(v-name, '"', "") FORMAT "x(25)" '",'
          '"' v-ship                                  '",'
          '"' REPLACE(v-city, '"', "")                '",'
          '"' REPLACE(v-state, '"', "")               '",'
          '"' REPLACE(v-sname, '"', "")               '",'
          '"' w-data.inv-no                                    '",'
          '"' v-month               FORMAT "99"                '",'
          '"' v-year[2]             FORMAT "9999"              '",'
          '"' v-date                FORMAT "99/99/9999"        '",'
          '"' REPLACE(w-data.i-no, '"', "")  FORMAT "X(15)"                  '",'
          '"' REPLACE(v-fgcat, '"', "")                        '",'
          '"' v-ord                 FORMAT ">>>>>>"            '",'
          '"' v-qty[1]              FORMAT "->>>>>>>>>"        '",'
          '"' v-pric                FORMAT "->>>>>>>>9.99<<"   '",'
          '"' REPLACE(v-uom, '"', "")                          '",'
          '"' v-amt[1]              FORMAT "->>>>>>>>>>9.99"   '",'
          SKIP.
    END.

    ASSIGN
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1].

    DELETE w-data.
  END.

  /* display final totals */
  PUT SKIP(1).

  UNDERLINE v-name v-qty[1] v-amt[1] WITH FRAME itemx.

  DISPLAY "         GRAND TOTALS" @ v-name
          v-qty[2] @ v-qty[1]
          v-amt[2] @ v-amt[1]

      WITH FRAME itemx.

  IF tb_excel AND fi_file NE '' THEN
  OUTPUT STREAM st-excel CLOSE.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-data C-Win 
PROCEDURE ship-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
release shipto.
if ar-inv.ship-id ne "" then
find first shipto
    where shipto.company eq cocode
      and shipto.cust-no eq ar-inv.cust-no
      and shipto.ship-id eq ar-inv.ship-id
    no-lock no-error.
if avail shipto then
  assign
   v-ship = ar-inv.ship-id
   v-shpz = shipto.ship-zip.
         
else
if ar-inv.sold-id ne "" then
  assign
   v-ship = ar-inv.sold-id
   v-shpz = ar-inv.sold-zip.
         
else
  assign
   v-ship = ar-inv.cust-no
   v-shpz = cust.zip.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uncomma C-Win 
PROCEDURE uncomma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM ip-char AS CHAR NO-UNDO.

ip-char = REPLACE(ip-char,',',' ').
/*
DEF VAR li AS INT NO-UNDO.

DO li = 1 TO LENGTH(ip-char):
  IF SUBSTR(ip-char,li,1) EQ "," THEN SUBSTR(ip-char,li,1) = " ".
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

