&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-booked.w

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
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def TEMP-TABLE w-data no-undo
  field ord-no like oe-ord.ord-no
  field line   like oe-ordl.line
  field sman   as char format "x(3)"
  field item-n like itemfg.i-name column-label "Item Description"
        format "x(27)"
  field procat like itemfg.procat column-label "Prod!Code"
  field qty like oe-ordl.qty column-label "Quantity!Ordered/EA"
        format ">,>>>,>>>"
  field sqft like itemfg.t-sqft column-label "Sq Ft" format ">>,>>>.999"
  field t-sqft like itemfg.t-sqft column-label "Total!Sq Ft/M" format "->,>>>.999"
  field t-tons as dec column-label "Total!  Tons" format "->,>>>.9"
  field price like oe-ordl.price format ">>>,>>9.99<<<<"
  field revenue like oe-ordl.t-price column-label "Order!Amount"
  field misc as log
  field cost as dec
  field comm as dec label "Comm %"
  FIELD margin AS DEC
  FIELD shp-qty LIKE oe-ordl.ship-qty .

def TEMP-TABLE wkrecap no-undo    /* recap by product category */
  field procat like itemfg.procat column-label "Cat"
  field t-sqft like itemfg.t-sqft  extent 2 column-label "Sq Ft" format ">>,>>>.999"
  field t-tons as dec column-label "Tons" extent 2 format "->,>>>.9"
  field revenue like oe-ordl.t-price   extent 2 column-label "Amount"
  field price-per-m  as dec column-label "$/MSF" extent 2
  field price-per-t  as dec column-label "$/TON" extent 2
  field num-of-ord as int column-label "#Orders".

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.
DEF VAR v-code AS CHAR NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

DEF STREAM excel.

DO TRANSACTION:
   {sys/inc/selrptcol.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_cust-no end_cust-no ~
begin_ord-date end_ord-date begin_slsmn end_slsmn begin_fg-cat end_fg-cat ~
rd_sqft tb_smn-no tb_sortby tb_prft tb_ton tb_comm tb_prepmisc ~
tb_exclude-set-comps tb_margin tb_exclude-transfer rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
tb_batch btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-date ~
end_ord-date begin_slsmn end_slsmn begin_fg-cat end_fg-cat lbl_sqft rd_sqft ~
tb_smn-no tb_sortby tb_prft tb_ton tb_comm tb_prepmisc tb_exclude-set-comps ~
tb_margin tb_exclude-transfer rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file tb_batch 

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

DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-booked.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sqft AS CHARACTER FORMAT "X(256)":U INITIAL "Print SqFt or Part#?" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_sqft AS CHARACTER INITIAL "Square Ft" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Square Ft", "Square Ft",
"Part#", "Part#"
     SIZE 26 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 15.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_comm AS LOGICAL INITIAL yes 
     LABEL "Print Commission?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_exclude-set-comps AS LOGICAL INITIAL no 
     LABEL "Exclude Set Components" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_exclude-transfer AS LOGICAL INITIAL no 
     LABEL "Exclude Transfer Releases/Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE tb_margin AS LOGICAL INITIAL no 
     LABEL "Print Avail Margin?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_prepmisc AS LOGICAL INITIAL no 
     LABEL "Include Prep / Misc Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_prft AS LOGICAL INITIAL yes 
     LABEL "Print Profit?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_smn-no AS LOGICAL INITIAL no 
     LABEL "Page By SalesRep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_sortby AS LOGICAL INITIAL no 
     LABEL "Sort by Order#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.43 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 3.38 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 3.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     begin_slsmn AT ROW 4.33 COL 30 COLON-ALIGNED HELP
          "Enter Beginning SalesRep Number"
     end_slsmn AT ROW 4.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending SalesRep Number"
     begin_fg-cat AT ROW 5.29 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_fg-cat AT ROW 5.29 COL 73 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     lbl_sqft AT ROW 6.95 COL 24 COLON-ALIGNED NO-LABEL
     rd_sqft AT ROW 6.95 COL 47 NO-LABEL
     tb_smn-no AT ROW 8.14 COL 15
     tb_sortby AT ROW 8.14 COL 50
     tb_prft AT ROW 9.1 COL 15
     tb_ton AT ROW 9.1 COL 50
     tb_comm AT ROW 10.05 COL 15
     tb_prepmisc AT ROW 10.05 COL 50
     tb_exclude-set-comps AT ROW 11 COL 15 WIDGET-ID 4
     tb_margin AT ROW 11 COL 50 WIDGET-ID 2
     tb_exclude-transfer AT ROW 11.95 COL 15 WIDGET-ID 6
     rd-dest AT ROW 17.43 COL 5 NO-LABEL
     lv-ornt AT ROW 18.14 COL 31 NO-LABEL
     lines-per-page AT ROW 18.14 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 20.05 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.19 COL 30
     tb_excel AT ROW 23.38 COL 72 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.38 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 24.19 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     tb_batch AT ROW 24.33 COL 6
     btn-ok AT ROW 26 COL 27
     btn-cancel AT ROW 26 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.71 COL 2
     "(Prep / Misc Charges will Display 'P' or 'M' for Product Code)" VIEW-AS TEXT
          SIZE 57 BY .95 AT ROW 13.86 COL 18
     "Note: Profit Includes Estimate Markups and Commissions." VIEW-AS TEXT
          SIZE 55 BY .95 AT ROW 14.81 COL 20
          FGCOLOR 1 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 16.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.8 BY 26.43.


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
         TITLE              = "Orders Booked"
         HEIGHT             = 26.43
         WIDTH              = 94.8
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
                "parm".

ASSIGN 
       begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sqft IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sqft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sqft".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sqft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_comm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude-set-comps:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude-transfer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_margin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prepmisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_smn-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sortby:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ton:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders Booked */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders Booked */
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


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  RUN run-report.
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer "
                                &begin_cust= "begin_cust-no"
                                &END_cust= "begin_cust-no"
                                &mail-subject=c-win:title
                                &mail-body=c-win:title
                                &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust= "begin_cust-no"
                                  &END_cust= "begin_cust-no"
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


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
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


&Scoped-define SELF-NAME rd_sqft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sqft C-Win
ON VALUE-CHANGED OF rd_sqft IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print Commission? */
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


&Scoped-define SELF-NAME tb_exclude-set-comps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-set-comps C-Win
ON VALUE-CHANGED OF tb_exclude-set-comps IN FRAME FRAME-A /* Exclude Set Components */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude-transfer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-transfer C-Win
ON VALUE-CHANGED OF tb_exclude-transfer IN FRAME FRAME-A /* Exclude Transfer Releases/Orders */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_margin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_margin C-Win
ON VALUE-CHANGED OF tb_margin IN FRAME FRAME-A /* Print Avail Margin? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prepmisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prepmisc C-Win
ON VALUE-CHANGED OF tb_prepmisc IN FRAME FRAME-A /* Include Prep / Misc Charges? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prft C-Win
ON VALUE-CHANGED OF tb_prft IN FRAME FRAME-A /* Print Profit? */
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


&Scoped-define SELF-NAME tb_smn-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_smn-no C-Win
ON VALUE-CHANGED OF tb_smn-no IN FRAME FRAME-A /* Page By SalesRep? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sortby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sortby C-Win
ON VALUE-CHANGED OF tb_sortby IN FRAME FRAME-A /* Sort by Order#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ton C-Win
ON VALUE-CHANGED OF tb_ton IN FRAME FRAME-A /* Print $/Ton? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
    lv-ornt:SCREEN-VALUE = "L".
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
   begin_ord-date = today
   end_ord-date   = today.

  IF g_batch THEN tb_batch = YES.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_ord-date.
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
  DISPLAY begin_cust-no end_cust-no begin_ord-date end_ord-date begin_slsmn 
          end_slsmn begin_fg-cat end_fg-cat lbl_sqft rd_sqft tb_smn-no tb_sortby 
          tb_prft tb_ton tb_comm tb_prepmisc tb_exclude-set-comps tb_margin 
          tb_exclude-transfer rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file tb_batch 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 begin_cust-no end_cust-no begin_ord-date end_ord-date 
         begin_slsmn end_slsmn begin_fg-cat end_fg-cat rd_sqft tb_smn-no 
         tb_sortby tb_prft tb_ton tb_comm tb_prepmisc tb_exclude-set-comps 
         tb_margin tb_exclude-transfer rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file tb_batch btn-ok 
         btn-cancel 
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* DEF VAR printok AS LOG.

  SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.

  RUN custom/usrprtb.p
      ("oerep\s-booked.r", FRAME {&FRAME-NAME}:HANDLE, SESSION:PRINTER-NAME, SESSION:PRINTER-PORT, CURRENT-WINDOW:TITLE).
  */

  {BATCH/runbatch.i "oerep\s-booked.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: SalesRep Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

def var fdate as date format "99/99/9999" init 01/01/0001 no-undo.
def var tdate like fdate init 12/31/9999 no-undo.
def var v-break as log init no no-undo.
def var prt-sqft as log init yes format "SqFt/PartNo" no-undo.
def var p-m-chg as log init no no-undo.
def var prt-profit as log init yes no-undo.
def var item-dscr as log init no no-undo.
def var mdate as date no-undo.
def var lo_trandate like fdate no-undo.
def var v-per-days as int extent 2 no-undo init 0.
def var v-n-lines  as int no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" init "zzz" no-undo.
def var v-sman like w-data.sman no-undo.
def var v-exclude as log no-undo.
def var v-misc as LOG NO-UNDO.
def var v-amt  like oe-ord.t-revenue NO-UNDO.
def var v-pct as dec format "99.99" NO-UNDO.
def var v-sqft like itemfg.t-sqft  format ">,>>9.999" NO-UNDO.
def var v-tons as DEC NO-UNDO.
def var v-qty like oe-ordl.qty format "->>>,>>9.99" NO-UNDO.
def var v-price-per-m as dec column-label "$/MSF" no-undo.
def var v-price-per-t as dec column-label "$/TON" no-undo.
def var v-msf like v-price-per-m extent 2 no-undo.
def var v-ton like v-price-per-t extent 2 no-undo.

def var v-revenue like oe-ordl.t-price format "->,>>>,>>9.99" no-undo
  column-label "Order!Amount".
def var v-profit as dec format "->>,>>9.9" no-undo
  column-label "% Profit".
DEF VAR v-margin AS DEC FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".
def var v-sname like sman.sname.

def var v as INT NO-UNDO.
def var qm as DEC NO-UNDO.
def var mat as DEC NO-UNDO.
def var lab as DEC NO-UNDO.

def var ii like i no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

find first w-data no-error.

form header "Sales Rep:"
            w-data.sman
            "-"
            v-sname
    with frame r-top1 no-box no-attr-space page-top stream-io width 180.


assign
 str-tit2 = c-win:TITLE + "   (O-R-5)"
 {sys/inc/ctrtext.i str-tit2 112}

 fdate      = begin_ord-date
 tdate      = end_ord-date
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-break    = tb_smn-no
 prt-sqft   = rd_sqft eq "Square Ft"
 /* item-dscr  = tb_desc */
 prt-profit = tb_prft
 p-m-chg    = tb_prepmisc.

IF tb_margin THEN
   prt-profit = NO.

if prt-profit then do:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  prt-profit = security-flag.
end.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Sales Rep,Sales Name,".

 /* if item-dscr then do:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Item Description,Quantity Ordered/EA,"
                + "Total Sq Ft/M,$/MSF,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE */
  IF prt-sqft THEN DO:

    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Item Description,Quantity Ordered/EA,Sq Ft,"
                + "Total Sq Ft/M,$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE DO:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Item Description,Quantity Ordered/EA,Customer Part Number,"
                + "$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.

EMPTY TEMP-TABLE w-data.

EMPTY TEMP-TABLE wkrecap.

{oerep/r-booked.i} 

SESSION:SET-WAIT-STATE ("").

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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

