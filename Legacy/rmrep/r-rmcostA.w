&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-rmcost.w

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
DEF BUFFER b-qty FOR reftable.

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

def var fco as ch NO-UNDO.
def var tco like fco NO-UNDO.
def var floc LIKE ITEM.loc NO-UNDO.
def var tloc like ITEM.loc NO-UNDO.
def var fcat as ch initial "000000" NO-UNDO.
def var tcat like fcat initial "ZZZZZZ" NO-UNDO.
def var doe    as logical initial TRUE NO-UNDO.
def var dor    as logical initial TRUE NO-UNDO.
def var detail as logical initial FALSE NO-UNDO.

def var head5 as ch format "x(39)" NO-UNDO.
def var head21 as ch format "x(76)" NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.


head21 =
"            On Hand      On Order     Committed   Backordered     Available".
head5 = "===== Board/Paper Speed Reduction =====".

form
  " Vendor 1:" item.vend-no  "Item #:" item.vend-item
    "Auto Alloc.?" item.alloc   skip
  " Vendor 2:" item.vend2-no "Item #:" item.vend2-item
    "    Stocked?" item.stocked skip
  " Reorder Policy:" item.ord-policy
        "Purchased or Manf?" to 65 item.pur-man skip
  " Reorder Level :" item.ord-level
        "Purchased UOM:" to 65 item.pur-uom skip
  " Minimum Order :" item.ord-min
        "Lead Time (Days):" to 65 item.lead-days skip
  " Maximum Order :" item.ord-max
        "Beg Balance :" to 60 item.beg-bal skip
        "Beg Bal Date:" to 60 item.beg-date
  "  Warehouse:" item.loc loc.dscr format "x(25)"
      "Last Count:" to 60 item.last-count skip
  "        Bin:" item.loc-bin "Count Date:" to 60 item.last-date  skip
  " Cycle Code:" item.cc-code space(10) "  Cons. UOM:" item.cons-uom
      "Last Cost :" to 60 item.last-cost  skip
  " Purch. Rpt Code:" item.pur-rcode "Prod.Code:" item.pic-code
      "Avg. Cost :" to 60 item.avg-cost skip
  head21 skip
  " QTY:" item.q-onh item.q-ono item.q-comm item.q-back item.q-avail skip
  "****************" skip
with frame item2 stream-io width 80 overlay no-labels no-underline.

{sys/form/r-topw.f}

find first ce-ctrl
    where ce-ctrl.company eq cocode
      AND ce-ctrl.loc EQ locode
    no-lock no-error.

DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEF TEMP-TABLE tt-e-i-v NO-UNDO
    FIELD vend-no AS CHAR
    FIELD std-uom AS CHAR
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD roll-w AS DEC DECIMALS 4 EXTENT 30.

def stream s-temp.

DEF VAR v-export     AS LOGICAL.
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-rmcost.csv".
DEF VAR v-acct-descr AS CHAR.
DEF VAR v-procat     AS CHAR NO-UNDO.
DEF VAR v-last-cost  LIKE item.last-cost NO-UNDO.
DEF VAR v-avg-cost   LIKE item.avg-cost NO-UNDO.
DEF VAR v-loc-descr  AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_mat-type begin_procat ~
end_procat begin_vend end_vend begin_whs end_whs tb_real tb_est tb_detailed ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS rd_mat-type lbl_mat-type begin_procat ~
end_procat begin_vend end_vend begin_whs end_whs tb_real tb_est tb_detailed ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-rmcost.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE lbl_mat-type AS CHARACTER FORMAT "X(256)":U INITIAL "Material Type?" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_mat-type AS CHARACTER INITIAL "Board - Paper" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Board - Paper", "Board - Paper",
"Ink - Coating", "Ink - Coating",
"Film - Leaf", "Film - Leaf",
"Adhesives", "Adhesives",
"Case", "Case",
"Miscellaneous - Adders", "Miscellaneous - Adders"
     SIZE 26 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 10.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 12.38.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detail Real Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_est AS LOGICAL INITIAL yes 
     LABEL "Estimated Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Output to Excel File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_real AS LOGICAL INITIAL yes 
     LABEL "Real Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

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
     rd_mat-type AT ROW 1.38 COL 43 NO-LABEL
     lbl_mat-type AT ROW 3.05 COL 24 COLON-ALIGNED NO-LABEL
     begin_procat AT ROW 6.62 COL 29 COLON-ALIGNED
     end_procat AT ROW 6.62 COL 67 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_vend AT ROW 7.81 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 7.81 COL 67 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_whs AT ROW 9 COL 29 COLON-ALIGNED HELP
          "Enter Beginng Warehouse" WIDGET-ID 62
     end_whs AT ROW 9 COL 67 COLON-ALIGNED HELP
          "Enter Ending Warehouse" WIDGET-ID 64
     tb_real AT ROW 10.24 COL 43
     tb_est AT ROW 11.19 COL 43
     tb_detailed AT ROW 12.14 COL 43
     rd-dest AT ROW 14.91 COL 6 NO-LABEL
     lv-ornt AT ROW 15.14 COL 30 NO-LABEL
     lines-per-page AT ROW 15.14 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 16.86 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.86 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.1 COL 30
     tb_excel AT ROW 20.05 COL 30 WIDGET-ID 2
     tb_runExcel AT ROW 20.1 COL 55.4 WIDGET-ID 4
     fi_file AT ROW 21.48 COL 28 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 24.38 COL 18
     btn-cancel AT ROW 24.38 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.71 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 13.33 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 25.14.


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
         TITLE              = "Raw Materials Cost"
         HEIGHT             = 25.14
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
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_mat-type IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_mat-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_mat-type".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_mat-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_real:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Raw Materials Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raw Materials Cost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  SESSION:SET-WAIT-STATE ("general").

  run run-report.
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= 'Raw Materials Cost'
                            &begin_cust= "begin_procat"
                            &END_cust= "end_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Raw Materials Cost"
                             &begin_cust= "begin_procat"
                             &END_cust= "end_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Raw Materials Cost"
                                  &begin_cust="end_procat"
                                  &END_cust="begin_procat"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
           END.
       END.
       WHEN 6 THEN RUN output-to-port.

  end case. 

  SESSION:SET-WAIT-STATE ("").
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


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_mat-type C-Win
ON VALUE-CHANGED OF rd_mat-type IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detail Real Items? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_est C-Win
ON VALUE-CHANGED OF tb_est IN FRAME FRAME-A /* Estimated Materials? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Output to Excel File? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_real
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_real C-Win
ON VALUE-CHANGED OF tb_real IN FRAME FRAME-A /* Real Materials? */
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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}     
    APPLY "entry" TO begin_procat.
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
  DISPLAY rd_mat-type lbl_mat-type begin_procat end_procat begin_vend end_vend 
          begin_whs end_whs tb_real tb_est tb_detailed rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_mat-type begin_procat end_procat begin_vend end_vend 
         begin_whs end_whs tb_real tb_est tb_detailed rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-board C-Win 
PROCEDURE est-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR i AS INT NO-UNDO.

RUN print-estimated (ip-rowid).

FOR EACH tt-e-i-v BREAK BY tt-e-i-v.vend-no:
  IF v-export THEN
     PUT STREAM s-temp 
         '"' tt-e-i-v.vend-no '",'
         '"' tt-e-i-v.std-uom '",'.

  DO i = 1 to 20:
     IF i = 1 THEN DO:
        PUT SPACE(3) "Vendor: " tt-e-i-v.vend-no
            SPACE(1) "Cost--> Up To " tt-e-i-v.run-qty[1] FORMAT ">,>>>,>>9.9<<" TO 45
            SPACE(1) tt-e-i-v.std-uom to 49
            SPACE(1) "$" tt-e-i-v.run-cost[1] FORMAT ">>>,>>9.9999"
            SPACE(3).

        IF v-export THEN 
           PUT STREAM s-temp 
               '"' STRING(tt-e-i-v.run-qty[1],">>>>>>9.9<<")     '",'
               '"' "$" STRING(tt-e-i-v.run-cost[1],">>>>>9.9999") '",'.

        DO j = 1 to 5:
           IF tt-e-i-v.roll-w[j] NE 0 THEN
              PUT tt-e-i-v.roll-w[j] SPACE(3).

           IF v-export THEN 
              PUT STREAM s-temp 
                  '"' STRING(tt-e-i-v.roll-w[j],">>9.9999") '",'.
        END.
        PUT SKIP.
     END.

     ELSE 
     IF tt-e-i-v.run-qty[i] NE 0 THEN DO:

        PUT tt-e-i-v.run-qty[i] FORMAT ">,>>>,>>9.9<<" TO 45
            SPACE(5) "$" tt-e-i-v.run-cost[i] FORMAT ">>>,>>9.9999" " " AT 66.

        IF v-export THEN 
           PUT STREAM s-temp 
               '"' STRING(tt-e-i-v.run-qty[i],">>>>>>9.9<<")     '",'
               '"' "$" STRING(tt-e-i-v.run-cost[i],">>>>>9.9999") '",'.

     END.
     ELSE DO:
        PUT SPACE(66).
        IF v-export THEN 
           PUT STREAM s-temp ",,".
     END.

     IF i GT 1 AND i LT 7 THEN
     DO:
        DO j = 1 TO 5:
           IF ((i - 1) * 5) + j LE 26 THEN
           DO:
              IF tt-e-i-v.roll-w[((i - 1) * 5) + j] ne 0 THEN
                 PUT tt-e-i-v.roll-w[((i - 1) * 5) + j] SPACE(3).

              IF v-export THEN 
                 PUT STREAM s-temp '"' STRING(tt-e-i-v.roll-w[((i - 1) * 5) + j],">>9.9999") '",'.
           END.
        END.
        PUT SKIP.
     END.
  END.

  PUT SKIP(1).
  DOWN.
END. 
IF v-export THEN PUT STREAM s-temp SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-not-board C-Win 
PROCEDURE est-not-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR i AS INT NO-UNDO.

RUN print-estimated (ip-rowid).

FOR EACH tt-e-i-v BREAK BY tt-e-i-v.vend-no:

  IF v-export THEN 
     PUT STREAM s-temp 
        '"' tt-e-i-v.vend-no '",'
        '"' tt-e-i-v.std-uom '",'.

  DO i = 1 to 20:
    IF i EQ 1 THEN DO:
      PUT SPACE(3) "Vendor: " tt-e-i-v.vend-no
          SPACE(1) "Cost--> Up To " tt-e-i-v.run-qty[1] FORMAT ">,>>>,>>9.9<<" TO 45
          SPACE(1) tt-e-i-v.std-uom to 49
          SPACE(1) "$" tt-e-i-v.run-cost[1] FORMAT ">>>,>>9.9999"
          SPACE(4) SKIP.

      IF v-export THEN 
        PUT STREAM s-temp 
            '"' STRING(tt-e-i-v.run-qty[1],">,>>>,>>9.9<<")  '",'
            '"' "$" STRING(tt-e-i-v.run-cost[1],">>>>>9.9999") '",'.
    END.

    ELSE 
    IF tt-e-i-v.run-qty[i] NE 0 THEN
    DO:
       PUT tt-e-i-v.run-qty[i] FORMAT ">,>>>,>>9.9<<" TO 45
           SPACE(5) "$" tt-e-i-v.run-cost[i] FORMAT ">>>,>>9.9999" SKIP.

       IF v-export THEN 
          PUT STREAM s-temp 
              '"' STRING(tt-e-i-v.run-qty[i],">,>>>,>>9.9<<")    '",'
              '"' "$" STRING(tt-e-i-v.run-cost[i],">>>>>9.9999") '",'.
    END.
  END.
  PUT SKIP(1).
  DOWN.
END.
IF v-export THEN PUT STREAM s-temp SKIP.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-estimated C-Win 
PROCEDURE print-estimated :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR v-index AS INT NO-UNDO.

EMPTY TEMP-TABLE tt-e-i-v.

FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

RELEASE e-item.

IF AVAIL item THEN
FIND FIRST e-item
     WHERE e-item.company EQ item.company
       AND e-item.i-no    EQ item.i-no
     NO-LOCK NO-ERROR.

IF AVAIL e-item THEN DO:
  FOR EACH e-item-vend OF e-item
      WHERE e-item-vend.item-type EQ YES
        AND e-item-vend.vend-no   GE begin_vend
        AND e-item-vend.vend-no   LE end_vend
      NO-LOCK:
    CREATE tt-e-i-v.
    ASSIGN tt-e-i-v.vend-no = e-item-vend.vend-no
           tt-e-i-v.std-uom = e-item.std-uom.

    DO v-index = 1 TO 10:
       ASSIGN
          tt-e-i-v.run-cost[v-index] = e-item-vend.run-cost[v-index]
          tt-e-i-v.run-qty[v-index] = e-item-vend.run-qty[v-index]
          tt-e-i-v.roll-w[v-index] = e-item-vend.roll-w[v-index].
    END.

    FIND FIRST b-qty WHERE
         b-qty.reftable = "vend-qty" AND
         b-qty.company = e-item-vend.company AND
             b-qty.CODE    = e-item-vend.i-no AND
         b-qty.code2   = e-item-vend.vend-no
         NO-LOCK NO-ERROR.

    IF AVAIL b-qty THEN
    DO:

       DO v-index = 1 TO 10:
          ASSIGN
             tt-e-i-v.run-qty[v-index + 10] = e-item-vend.runQtyXtra[v-index]
             tt-e-i-v.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index].
       END.
    END.

    DO v-index = 11 TO 30:
       tt-e-i-v.roll-w[v-index] = e-item-vend.roll-w[v-index].
    END.

    RELEASE tt-e-i-v.
  END.

  IF "" GE begin_vend AND "" LE end_vend              AND
     NOT CAN-FIND(FIRST tt-e-i-v WHERE vend-no EQ "") THEN DO:
    CREATE tt-e-i-v.
    tt-e-i-v.std-uom = e-item.std-uom.

    DO v-index = 1 TO 10:
       ASSIGN
          tt-e-i-v.run-cost[v-index] = e-item.run-cost[v-index]
          tt-e-i-v.run-qty[v-index] = e-item.run-qty[v-index]
          tt-e-i-v.roll-w[v-index] = e-item.roll-w[v-index].
    END.

       DO v-index = 1 TO 10:
          ASSIGN
             tt-e-i-v.run-qty[v-index + 10] = e-item.run-qty[v-index]
             tt-e-i-v.run-cost[v-index + 10] = e-item.run-cost[v-index].
       END.



    RELEASE tt-e-i-v.

  END.
END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-board C-Win 
PROCEDURE run-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var rm-cst-amt like item.last-cost.
def var v-printed as log init no no-undo.

FORM
   SKIP
   "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
   "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
   "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
   " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
   "  Caliper:" item.cal     TO 20 " Sheet Len:" TO 45 item.s-len SKIP
   " Basis Wt:" item.basis-w TO 20 " Sheet Wid:" TO 45 item.s-wid SKIP
   "    Reg.#:" item.reg-no  TO 20 "  Roll Wid:" TO 45 item.r-wid SKIP
   " Shrink %:" item.shrink  TO 20  SKIP
   SPACE(21) head5 SKIP
   "   Department Name: "
   SPACE(2) item.dept-name[1] SPACE(2) item.dept-name[2]
   SPACE(2) item.dept-name[3] SPACE(2) item.dept-name[4]
   SPACE(2) item.dept-name[5] SPACE(2) item.dept-name[6]
   SPACE(2) item.dept-name[7] SPACE(2) item.dept-name[8]
   SPACE(2) item.dept-name[9] SPACE(2) item.dept-name[10] 
   SKIP
   "       Reduction %: " item.speed%[1 FOR 10]
   SKIP
   WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

FORM
    item.procat
    item.i-no
    item.i-name    FORMAT "x(27)"
    item.cal
    item.basis-w
    item.last-cost
    item.cons-uom
    item.q-onh     FORMAT "->>>>,>>9.999"
    item.q-ono     FORMAT "->>>>,>>9.999"
    item.q-comm    FORMAT  ">>>>,>>9.999"
    item.q-avail   FORMAT "->>>>,>>9.999"
    SKIP
HEADER
   "CAT   ITEM       DESCRIPTION                 CALIPER WEIGHT     COST     UOM       On Hand      On Order     Allocated     Available"
    WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

FORM
   item.procat item.i-no item.i-name item.cal item.basis-w

HEADER
   "CAT   ITEM       DESCRIPTION                    CALIPER WEIGHT   <               Valid   Roll   Widths               >"
   WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


SESSION:SET-WAIT-STATE ("general").

ASSIGN 
   str-tit2 = c-win:title + " - Board/Paper List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:
   IF NOT detail THEN DO:
      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            "CAT,ITEM,DESCRIPTION,CALIPER,WEIGHT,COST,UOM,On Hand,On Order,Allocated,Available"
            SKIP.

      FOR EACH ITEM NO-LOCK WHERE 
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc
           AND (item.i-code = "R") 
           AND LOOKUP(item.mat-type,"B,P") GT 0 
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME itemx:
                 {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         CLEAR FRAME itemx ALL NO-PAUSE.                 
         v-printed = YES.

         FIND FIRST rm-ctrl WHERE rm-ctrl.company = cocode NO-LOCK.

         IF rm-ctrl.avg-lst-cst = TRUE THEN  
            rm-cst-amt = item.avg-cost.
         ELSE
            rm-cst-amt = item.last-cost.

         DISPLAY 
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.cal
            item.basis-w
            rm-cst-amt @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.

         DOWN.

         IF v-export THEN DO:
            ASSIGN v-procat = IF FIRST-OF(item.procat) THEN item.procat ELSE "".
            PUT STREAM s-temp 
               '"' v-procat      '",'
               '"' item.i-no     '",'
               '"' item.i-name   '",'
               '"' item.cal      '",'
               '"' item.basis-w  '",'
               '"' rm-cst-amt    FORMAT ">>>>>9.9999"       '",'
               '"' item.cons-uom '",'
               '"' item.q-onh    FORMAT "->>>>>>>>9.9<<<<<" '",'
               '"' item.q-ono    FORMAT "->>>>>>>>9.9<<<<<" '",'
               '"' item.q-comm   FORMAT "->>>>>>>>9.9<<<<<" '",'
               '"' item.q-avail  FORMAT "->>>>>>>>9.9<<<<<" '"'
               SKIP. 
         END.
      END. /* end for for each */

   END. /* non detail ends here */
   ELSE DO:

      IF v-export THEN 
         PUT STREAM s-temp 
           "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est.Desc,Category,
            Caliper,Sheet Len,Sheet Wid,Basis Wt,Reg.#,Roll Wid,Shrink %,
            Dept , % ,Dept , % ,Dept , % ,Dept , % ,
            Dept , % ,Dept , % ,Dept , % ,Dept , % ,
            Dept , % ,Dept , % ,
            Vendor 1,Vend 1 Item #,Auto Alloc,Vendor 2,Vend 2 Item #,
            Stocked,Re-Ord Pol,Re-Ord Level,Ord Min,Ord MAX,Pur/Manf,
            Pur UOM,Lead Days,Warehouse,Bin Loc,Cyc CD,Cons Uom,Pur Rpt Cd,
            Prod Cd,Beg Bal,Beg Date,last Cnt,Cnt Date,Last Cost,Avg Cost,
            On Hand,On Order,Committed,Backordered,Available,
            Vend 1,UOM,
            Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
            Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
            Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
            Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
            Vend 2,UOM,
            Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
            Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
            Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
            Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
            Vend 3,UOM,
            Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
            Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
            Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
            Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
            Vend 4,UOM,
            Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
            Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
            Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
            Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
            Vend 5,UOM,
            Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
            Run Qty 6,Run Cost 6,Est Roll WRun Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
            Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
            Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
            Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," 
            SKIP.

      FOR EACH ITEM NO-LOCK WHERE 
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc  
           AND (item.i-code = "R") 
           AND LOOKUP(item.mat-type,"B,P") GT 0 
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat  
               BY item.i-no WITH FRAME item:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         CLEAR FRAME item ALL NO-PAUSE.
         v-printed = YES.

         DISPLAY head5
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            head5
            item.cal     WHEN item.cal     NE 0
            item.basis-w WHEN item.basis-w NE 0
            item.reg-no  WHEN item.reg-no  NE ""
            item.shrink  WHEN item.shrink  NE 0
            item.s-len   WHEN item.s-len   NE 0
            item.s-wid   WHEN item.s-wid   NE 0
            item.r-wid   WHEN item.r-wid   NE 0
            item.dept-name[1 ]   WHEN item.dept-name[1 ] NE ""
            item.speed%[1 ]      WHEN item.speed[1 ]     NE 0
            item.dept-name[2 ]   WHEN item.dept-name[2 ] NE ""
            item.speed%[2 ]      WHEN item.speed[2 ]     NE 0
            item.dept-name[3 ]   WHEN item.dept-name[3 ] NE ""
            item.speed%[3 ]      WHEN item.speed[3 ]     NE 0
            item.dept-name[4 ]   WHEN item.dept-name[4 ] NE ""
            item.speed%[4 ]      WHEN item.speed[4 ]     NE 0
            item.dept-name[5 ]   WHEN item.dept-name[5 ] NE ""
            item.speed%[5 ]      WHEN item.speed[5 ]     NE 0
            item.dept-name[6 ]   WHEN item.dept-name[6]  NE ""
            item.speed%[6 ]      WHEN item.speed[6 ]     NE 0
            item.dept-name[7 ]   WHEN item.dept-name[7 ] NE ""
            item.speed%[7 ]      WHEN item.speed[7 ]     NE 0
            item.dept-name[8 ]   WHEN item.dept-name[8 ] NE ""
            item.speed%[8 ]      WHEN item.speed[8 ]     NE 0
            item.dept-name[9 ]   WHEN item.dept-name[9 ] NE ""
            item.speed%[9 ]      WHEN item.speed[9 ]     NE 0
            item.dept-name[10]   WHEN item.dept-name[10] NE ""
            item.speed%[10]      WHEN item.speed[10]     NE 0.

         IF v-export THEN DO:
            PUT STREAM s-temp
               '"' item.i-no        '",'          
               '"' item.i-code      '",'        
               '"' item.i-name      '",'        
               '"' item.mat-type    '",'        
               '"' item.i-dscr      '",'        
               '"' item.cost-type   '",'        
               '"' item.est-dscr    '",'        
               '"' item.procat      '",'        
               '"' item.cal         '",'        
               '"' item.s-len       '",'        
               '"' item.s-wid       '",'        
               '"' item.basis-w     '",'        
               '"' item.reg-no      '",'      
               '"' item.r-wid       '",'        
               '"' item.shrink      '",'        
               '"' item.dept-name[1 ] '",'       
               '"' item.speed%[1 ]     '",'   
               '"' item.dept-name[2 ]  '",'   
               '"' item.speed%[2 ]     '",'   
               '"' item.dept-name[3 ]  '",'   
               '"' item.speed%[3 ]     '",'   
               '"' item.dept-name[4 ]  '",'   
               '"' item.speed%[4 ]     '",'   
               '"' item.dept-name[5 ]  '",'   
               '"' item.speed%[5 ]     '",'   
               '"' item.dept-name[6 ]  '",'   
               '"' item.speed%[6 ]     '",'   
               '"' item.dept-name[7 ]  '",'   
               '"' item.speed%[7 ]     '",'   
               '"' item.dept-name[8 ]  '",'   
               '"' item.speed%[8 ]     '",'   
               '"' item.dept-name[9 ]  '",'   
               '"' item.speed%[9 ]     '",'   
               '"' item.dept-name[10]  '",'   
               '"' item.speed%[10 ]    '",'
                .              

         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE 
                       loc.company = cocode
                  AND loc.loc = item.loc NO-LOCK NO-ERROR.

            /* CTS */
            CLEAR FRAME item2 ALL NO-PAUSE.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level   NE 0
               item.ord-min     WHEN item.ord-min     NE 0
               item.ord-max     WHEN item.ord-max     NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days   NE 0
               item.loc
               loc.dscr         WHEN available loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal    NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh      NE 0
               item.q-ono        WHEN item.q-ono      NE 0
               item.q-comm       WHEN item.q-comm     NE 0
               item.q-back       WHEN item.q-back     NE 0
               item.q-avail      WHEN item.q-avail    NE 0.

           IF v-export THEN
            PUT STREAM s-temp  
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.ord-level   '",'
               '"' item.ord-min     '",'
               '"' item.ord-max     '",'
               '"' item.pur-man     '",'
               '"' item.pur-uom     '",'
               '"' item.lead-days   '",'
               '"' item.loc " " v-loc-descr       '",'
               '"' item.loc-bin     '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.beg-bal     '",'
               '"' item.beg-date    '",'
               '"' item.last-count  '",'
               '"' item.last-date   '",'
               '"' item.last-cost   '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",'
               '"' item.q-ono       '",'
               '"' item.q-comm      '",'
               '"' item.q-back      '",'
               '"' item.q-avail     '",'
               .
         END. /* end for frame item2 */

         RUN est-board (ROWID(item)).

      END. /* end for each */
   END. /* end for detail */
END. /* end for dor */


IF doe THEN DO:
   IF v-export THEN DO:
      PUT STREAM s-temp 
         "CAT,"
         "ITEM,"       
         "DESCRIPTION,"                    
         "CALIPER WEIGHT,"   
         "<               Valid   Roll   Widths               >"

         SKIP.
    END.

    FOR EACH ITEM NO-LOCK WHERE 
             item.company = cocode 
         AND item.loc >= floc
         AND item.loc <= tloc
         AND (item.i-code = "E")                             
         AND LOOKUP(item.mat-type,"B,P") GT 0     
         AND item.procat >= fcat
         AND item.procat <= tcat
      BREAK BY item.company 
            BY item.loc 
            BY item.i-code
            BY item.mat-type 
            BY item.procat 
            BY item.i-no WITH FRAME iteme:
            {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY
         "" @ item.procat
         item.procat when first-of(item.procat)
         item.i-no
         item.i-name
         item.cal
         item.basis-w.
      PUT SKIP.

      IF v-export THEN DO:
         v-procat = IF first-of(item.procat) THEN item.procat
                 ELSE "".

         PUT STREAM s-temp  
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' item.cal      '",'
            '"' item.basis-w  '"'
         SKIP.

      END.

      RUN est-board (ROWID(item)).
    END.
END.

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-case C-Win 
PROCEDURE run-case :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-printed AS LOG INIT NO NO-UNDO.

FORM
   SKIP
   "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
   "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
   "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
   " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
   "   Length:" TO 10 item.case-l     "Lbs/Case:" TO 60 item.avg-w     SKIP
   "    Width:" TO 10 item.case-w  "Pieces/Case:" TO 60 item.box-case  SKIP
   "    Depth:" TO 10 item.case-d "Cases/Pallet:" TO 60 item.case-pall SKIP
   WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

FORM
    item.procat item.i-no item.i-name FORMAT "x(27)"
    item.avg-w item.case-pall item.last-cost item.cons-uom
    /* CTS */
    item.q-onh    /* CTS */ FORMAT "->>>>>,>>9.9<<"
    item.q-ono    /* CTS */ FORMAT "->>>>>,>>9.9<<"
    item.q-comm   /* CTS */ FORMAT ">>>>>,>>9.9<<"
    item.q-avail  /* CTS */ FORMAT "->>>>>,>>9.9<<"
    /* CTS end */
    SKIP
HEADER
"CAT   ITEM       DESCRIPTION                 Lbs/Cas Cas/Pal    COST   UOM       On Hand      On Order     Allocated     Available"
    WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO width 132.

FORM
     item.procat item.i-no item.i-name space(3) item.avg-w SPACE(5)
     item.case-pall FORMAT ">>9"
HEADER
   "CAT   ITEM       DESCRIPTION                      Lbs/Cas Cas/Pal"
    WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


ASSIGN
   str-tit2 = c-win:title + " - Corrugated Case List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:
   IF NOT detail THEN DO:

      IF v-export THEN 
         PUT STREAM s-temp UNFORMATTED 
            "CAT,ITEM,DESCRIPTION,Lbs/Cas Cas/Pal,COST,UOM,On Hand,On Order,Allocated,Available"                 
         SKIP. 

      FOR EACH item NO-LOCK WHERE 
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc   
           AND item.i-code = "R"
           AND item.mat-type = "C"
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.i-code 
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME itemx:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = YES.

         DISPLAY
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.avg-w
            item.case-pall
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF v-export THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
                  v-procat = ITEM.procat.
               ELSE
                  v-procat = "".

               IF ce-ctrl.r-cost = NO THEN
                  v-last-cost = ITEM.last-cost.
               ELSE
                  v-last-cost = item.avg-cost.

               PUT STREAM s-temp UNFORMATTED 
                  '"' v-procat      '",'
                  '"' item.i-no     '",'
                  '"' item.i-name   '",'
                  '"' item.avg-w    '",'
                  '"' item.case-pall    '",'
                  '"' v-last-cost   '",' 
                  '"' item.cons-uom '",'
                  '"' item.q-onh    '",'
                  '"' item.q-ono    '",'
                  '"' item.q-comm   '",'
                  '"' item.q-avail  '"'
                  SKIP.
         END.
      END.
   END.
   ELSE DO:
      IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
            "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Length,Width,Depth,Lbs/Case,Pieces/Case,Cases/Pallet,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.

      FOR EACH item NO-LOCK WHERE 
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc
           AND item.i-code = "R" 
           AND item.mat-type = "C"
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.i-code 
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME item:
                {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = yes.

         DISPLAY
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            item.case-l
            item.case-w
            item.case-d
            item.avg-w
            item.box-case
            item.case-pall.

         IF v-export THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'
               '"' item.case-l      '",'
               '"' item.case-w      '",'
               '"' item.case-d      '",'
               '"' item.avg-w       '",'
               '"' item.box-case    '",'
               '"' item.case-pall   '",'.
         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE loc.company = cocode 
                   AND loc.loc = item.loc NO-LOCK NO-ERROR.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN ITEM.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN AVAILABLE loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date      
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0 
               SKIP
               /*"*****************"*/.
         END.
         IF v-export THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
     END.
   END.
END.

IF doe THEN DO:
   IF v-printed THEN 
      page.

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         'CAT,ITEM,DESCRIPTION,Lbs/Cas Cas/Pal'
         SKIP. 

   FOR EACH ITEM NO-LOCK WHERE 
            item.company = cocode 
        AND item.loc >= floc
        AND item.loc <= tloc   
        AND item.i-code = "E"
        AND item.mat-type = "C"
        AND item.procat >= fcat
        AND item.procat <= tcat
      BREAK BY item.i-code 
            BY item.mat-type 
            BY item.procat 
            BY item.i-no WITH FRAME iteme:
           {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY 
         item.procat when first-of(item.procat)
         item.i-no
         item.i-name
         item.avg-w
         item.case-pall.
      PUT SKIP.

      IF v-export THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' sqin-lb       '",'
            '"' linin-lb      '"'  
            SKIP.
      END.

      RUN est-not-board (ROWID(item)).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-film C-Win 
PROCEDURE run-film :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var v-printed as log init no no-undo.

form
  "  Item No:" to 10 item.i-no     " Item Code:" to 60 item.i-code    skip
  "     Name:" to 10 item.i-name   "Mat'l Type:" to 60 item.mat-type  skip
  "     Desc:" to 10 item.i-dscr   " Cost Type:" to 60 item.cost-type skip
  " Est.Desc:" to 10 item.est-dscr "  Category:" to 60 item.procat    skip
  "Sq. In/Lb:" to 10 item.sqin-lb   skip
  with frame item stream-io width 80 overlay no-labels no-underline.

form
    item.procat item.i-no item.i-name format "x(30)"
    space(3) item.sqin-lb format ">>>>>9"
    item.last-cost item.cons-uom
    item.q-onh item.q-ono item.q-comm item.q-avail skip
header
"CAT   ITEM       DESCRIPTION                      Sq~"/Lb     COST     UOM       On Hand      On Order     Allocated     Available"
    with frame itemx no-box no-labels down stream-io width 132.

form
     item.procat item.i-no item.i-name space(10) item.sqin-lb format ">>>>>9"
header
"CAT   ITEM       DESCRIPTION                             Sq~"/Lb "
    with frame iteme no-box no-labels down stream-io width 119.

ASSIGN
   str-tit2 = c-win:TITLE + " - Film/Leaf List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:

   /* VIEW FRAME r-top. */

   IF NOT detail THEN DO:
      IF v-export THEN 
         PUT STREAM s-temp UNFORMATTED 
            'CAT,ITEM,DESCRIPTION,Sq~"/Lb,COST,UOM,On Hand,On Order,Allocated,Available'                 
         SKIP.

      FOR EACH ITEM NO-LOCK WHERE
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc 
           AND item.i-code = "R"
           AND index("FLW",item.mat-type) > 0
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no 
         WITH FRAME itemx:
           {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = yes.

         DISPLAY
            item.procat    WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.sqin-lb
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost  WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF v-export THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
               v-procat = ITEM.procat.
            ELSE
               v-procat = "".

            IF ce-ctrl.r-cost = NO THEN
               v-last-cost = ITEM.last-cost.
            ELSE
               v-last-cost = item.avg-cost.

            PUT STREAM s-temp UNFORMATTED 
               '"' v-procat      '",'
               '"' item.i-no     '",'
               '"' item.i-name   '",'
               '"' item.sqin-lb  '",'
               '"' v-last-cost   '",' 
               '"' item.cons-uom '",'
               '"' item.q-onh    '",'
               '"' item.q-ono    '",'
               '"' item.q-comm   '",'
               '"' item.q-avail  '"'
               SKIP.
         END.
      END.
   END.
   ELSE DO:
      IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
           "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est.Desc,Category,Sq. In/Lb,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.

      FOR EACH item NO-LOCK WHERE
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc 
           AND item.i-code = "R"
           AND index("FLW",item.mat-type) > 0
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no 
         WITH FRAME item:
         {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = YES.

         DISPLAY
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            item.sqin-lb      WHEN item.sqin-lb  NE 0.

         IF v-export THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'
               '"' item.sqin-lb     '",'. 
         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE 
                       loc.company = cocode 
                   AND loc.loc = item.loc no-lock no-error.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN available loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0.
         END.

         IF v-export THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
      END.
   END.
END.

IF doe THEN DO:
   IF v-printed THEN PAGE.

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         "CAT,ITEM,DESCRIPTION,Sq~"/Lb"
         SKIP. 

   FOR EACH ITEM NO-LOCK WHERE 
            item.company = cocode 
        AND item.loc >= floc
        AND item.loc <= tloc
        AND item.i-code = "E"
        AND index("FLW",item.mat-type) > 0
        AND item.procat >= fcat
        AND item.procat <= tcat
      BREAK BY item.company 
            BY item.loc 
            BY item.i-code
            BY item.mat-type 
            BY item.procat 
            BY item.i-no 
      WITH FRAME iteme:
      {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY
         item.procat WHEN FIRST-OF(item.procat)
         item.i-no
         item.i-name
         item.sqin-lb.
      PUT SKIP.

      IF v-export THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' item.sqin-lb  '"'  
            SKIP.
      END.

      RUN est-not-board (ROWID(item)).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-glue C-Win 
PROCEDURE run-glue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-printed AS LOG INIT NO NO-UNDO.

FORM
   "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code     SKIP
   "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type   SKIP
   "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type  SKIP
   " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat     SKIP
   "Sq. In/Lb:" TO 10 item.sqin-lb                                      SKIP
   "Lin.In/Lb:" TO 10 item.linin-lb                                     SKIP
   WITH FRAME ITEM STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

FORM
   item.procat item.i-no item.i-name            FORMAT "x(27)"
   item.sqin-lb format ">>>>>9" item.linin-lb   FORMAT ">>>>>9"
   item.last-cost item.cons-uom
   item.q-onh item.q-ono item.q-comm item.q-avail SKIP
HEADER
   'CAT   ITEM       DESCRIPTION                 Sq"/Lb Lin"/Lb    COST     UOM       On Hand      On Order     Allocated     Available'
   WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

FORM
   item.procat item.i-no item.i-name SPACE(10)
   item.sqin-lb  FORMAT ">>>>>9" SPACE(2) item.linin-lb FORMAT ">>>>>9"
HEADER
   'CAT   ITEM       DESCRIPTION                             Sq"/Lb Lin"/Lb'
   WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.

ASSIGN
   str-tit2 = c-win:title + " - Adhesives List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:
   IF NOT detail THEN DO: 
      IF v-export THEN 
         PUT STREAM s-temp UNFORMATTED 
            "CAT,ITEM,DESCRIPTION,Sq'"'/Lb,Lin'"'/Lb,COST,UOM,On Hand,On Order,Allocated,Available"                 
         SKIP. 

      FOR EACH item NO-LOCK WHERE
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc 
           AND item.i-code = "R"
           AND item.mat-type EQ "G" 
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME itemx:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = YES.

         DISPLAY
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.sqin-lb
            item.linin-lb
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF v-export THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
                  v-procat = ITEM.procat.
               ELSE
                  v-procat = "".

               IF ce-ctrl.r-cost = NO THEN
                  v-last-cost = ITEM.last-cost.
               ELSE
                  v-last-cost = item.avg-cost.

               PUT STREAM s-temp UNFORMATTED 
                  '"' v-procat      '",'
                  '"' item.i-no     '",'
                  '"' item.i-name   '",'
                  '"' item.sqin-lb  '",'
                  '"' item.linin-lb    '",'
                  '"' v-last-cost   '",' 
                  '"' item.cons-uom '",'
                  '"' item.q-onh    '",'
                  '"' item.q-ono    '",'
                  '"' item.q-comm   '",'
                  '"' item.q-avail  '"'
                  SKIP.
         END.
      END.
   END.
   ELSE DO: 
      IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
            "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Sq. In/Lb,Lin.In/Lb,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.
      FOR EACH item NO-LOCK WHERE
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc 
           AND item.i-code = "R"
           AND item.mat-type = "G"
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc  
               BY item.i-code
               BY item.mat-type 
               BY item.procat  
               BY item.i-no WITH FRAME item:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = YES.

         DISPLAY
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            item.sqin-lb      WHEN item.sqin-lb   NE 0
            item.linin-lb     WHEN item.linin-lb  NE 0.

         IF v-export THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'
               '"' item.sqin-lb     '",'
               '"' item.linin-lb    '",'.
         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE loc.company = cocode 
                             AND loc.loc = item.loc NO-LOCK NO-ERROR.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN AVAILABLE loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0.
         END.

         IF v-export THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
      END.
   END.
END.

IF doe THEN DO:
   IF v-printed THEN 
      page.

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         'CAT,ITEM,DESCRIPTION,Sq"/Lb Lin"/Lb'
         SKIP.  

   FOR EACH ITEM NO-LOCK WHERE
            item.company = cocode 
        AND item.loc >= floc
        AND item.loc <= tloc 
        AND item.i-code = "E"
        AND item.mat-type EQ "G"
        AND item.procat >= fcat
        AND item.procat <= tcat
      BREAK BY item.company 
            BY item.loc 
            BY item.i-code
            BY item.mat-type 
            BY item.procat 
            BY item.i-no WITH FRAME iteme:
            {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY
         item.procat WHEN FIRST-OF(item.procat)
         item.i-no
         item.i-name
         item.sqin-lb
         item.linin-lb.
      PUT SKIP.

      IF v-export THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' sqin-lb       '",'
            '"' linin-lb      '"'  
            SKIP.
      END.

      RUN est-not-board (ROWID(item)).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-ink C-Win 
PROCEDURE run-ink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var v-printed as log init no no-undo.

form
  "  Item No:" to 10 item.i-no     " Item Code:" to 60 item.i-code    skip
  "     Name:" to 10 item.i-name   "Mat'l Type:" to 60 item.mat-type  skip
  "     Desc:" to 10 item.i-dscr   " Cost Type:" to 60 item.cost-type skip
  " Est.Desc:" to 10 item.est-dscr "  Category:" to 60 item.procat    skip
  "   Ink Type:" item.ink-type   to 20 "     MSI/Lb:" to 60 item.yield   skip
  " Press Type:" item.press-type to 20 "Min Lbs/Job:" to 60 item.min-lbs skip
  with frame item stream-io width 80 overlay no-labels no-underline.

form
    item.procat item.i-no item.i-name format "x(27)"
    space(3) item.min-lbs space(1) item.yield format ">>>>>9" item.last-cost
    item.cons-uom item.q-onh item.q-ono item.q-comm item.q-avail skip
header
"CAT   ITEM       DESCRIPTION                 Min.Lbs MSI/Lb     COST     UOM       On Hand      On Order     Allocated     Available"
    with frame itemx no-box no-labels down stream-io width 132.

form
     item.procat item.i-no item.i-name space(3) item.min-lbs space(4)
     item.yield format ">>>>>9"
header
"CAT   ITEM       DESCRIPTION                    Min.Lbs MSI/Lb "
    with frame iteme no-box no-labels down stream-io width 119.

ASSIGN
   str-tit2 = c-win:title + " - Ink/Coating List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:
   IF NOT detail THEN DO: 
      IF v-export THEN 
         PUT STREAM s-temp UNFORMATTED 
            "CAT,ITEM,DESCRIPTION,Min.Lbs,MSI/Lb,COST,UOM,On Hand,On Order,Allocated,Available"                 
         SKIP. 

      FOR EACH ITEM NO-LOCK WHERE
               item.company = cocode 
          AND item.loc >= floc
          AND item.loc <= tloc 
          AND item.i-code = "R"
          AND LOOKUP(ITEM.mat-type,"I,V") GT 0
          AND item.procat >= fcat
          AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME itemx:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = yes.

         DISPLAY 
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.min-lbs
            item.yield
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF v-export THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
               v-procat = ITEM.procat.
            ELSE
               v-procat = "".

            IF ce-ctrl.r-cost = NO THEN
               v-last-cost = ITEM.last-cost.
            ELSE
               v-last-cost = item.avg-cost.

            PUT STREAM s-temp UNFORMATTED 
               '"' v-procat      '",'
               '"' item.i-no     '",'
               '"' item.i-name   '",'
               '"' item.min-lbs  '",'
               '"' item.yield    '",'
               '"' v-last-cost   '",' 
               '"' item.cons-uom '",'
               '"' item.q-onh    '",'
               '"' item.q-ono    '",'
               '"' item.q-comm   '",'
               '"' item.q-avail  '"'
               SKIP.
         END.
      END.
   END.
   ELSE DO:
      IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
            "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Ink Type,MSI/Lb,Press Type,Min Lbs/Job,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.

      FOR EACH item NO-LOCK WHERE
               item.company = cocode 
           AND item.loc >= floc
           AND item.loc <= tloc 
           AND item.i-code = "R"
           AND LOOKUP(ITEM.mat-type,"I,V") GT 0      
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type  
               BY item.procat  
               BY item.i-no WITH FRAME item:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = yes.

         DISPLAY
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            item.ink-type   WHEN item.ink-type   NE ""
            item.press-type WHEN item.press-type NE ""
            item.yield      WHEN item.yield      NE 0
            item.min-lbs    WHEN item.min-lbs    NE 0.

         IF v-export THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'
               '"' item.ink-type    '",'
               '"' item.yield       '",'
               '"' item.press-type  '",'
               '"' item.min-lbs     '",'.
         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE 
                       loc.company = cocode 
                   AND loc.loc = item.loc NO-LOCK NO-ERROR.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN AVAILABLE loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21 
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0.
         END.

         IF v-export THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
      END.
   END.
END.

IF doe THEN DO:
   IF v-printed THEN PAGE.

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         "CAT,ITEM,DESCRIPTION,Min.Lbs,MSI/Lb"
         SKIP.  

   FOR EACH ITEM NO-LOCK WHERE 
            item.company = cocode 
       AND item.loc >= floc
       AND item.loc <= tloc
       AND  item.i-code = "E"
       AND  LOOKUP(ITEM.mat-type,"I,V") GT 0      
       AND  item.procat >= fcat
       AND  item.procat <= tcat
      BREAK BY item.company 
            BY item.loc  
            BY item.i-code
            BY item.mat-type 
            BY item.procat 
            BY item.i-no WITH FRAME iteme:
            {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY
         "" @ item.procat
         item.procat when first-of(item.procat)
         item.i-no
         item.i-name
         item.min-lbs
         item.yield.
         PUT SKIP.

      IF v-export THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' item.min-lbs  '",'
            '"' item.yield    '"'  
            SKIP.
      END.

      RUN est-not-board(ROWID(item)).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-misc C-Win 
PROCEDURE run-misc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-printed AS LOG INIT NO NO-UNDO.

FORM
   SKIP
   "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
   "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
   "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
   " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
   WITH FRAME ITEM STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

FORM
   item.procat item.i-no item.i-name FORMAT "x(30)" SPACE(11)
   item.last-cost item.cons-uom
   item.q-onh   FORMAT "->>>>,>>9.999"
   item.q-ono   FORMAT "->>>>,>>9.999"
   item.q-comm  FORMAT "->>>>,>>9.999"
   item.q-avail FORMAT "->>>>,>>9.999"
   SKIP
HEADER
"CAT   ITEM       DESCRIPTION                                    COST   UOM       On Hand      On Order     Allocated     Available"
    WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

FORM
     item.procat item.i-no item.i-name
HEADER
"CAT   ITEM       DESCRIPTION   "
    WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


ASSIGN
   str-tit2 = c-win:title + " - Miscellaneous List"
   {sys/inc/ctrtext.i str-tit2 112}.

DISPLAY "" WITH FRAME r-top.

IF dor THEN DO:
   IF NOT detail THEN DO:

      IF v-export THEN 
         PUT STREAM s-temp UNFORMATTED 
            "CAT,ITEM,DESCRIPTION,COST,UOM,On Hand,On Order,Allocated,Available"                 
         SKIP. 

      FOR EACH item NO-LOCK WHERE 
               item.company EQ cocode
           AND item.i-code  EQ "R"
           AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
           AND item.procat  GE fcat
           AND item.procat  LE tcat
         BREAK BY item.i-code
               BY item.mat-type
               BY item.procat
               BY item.i-no WITH FRAME itemx:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "} 
         v-printed = YES.

         DISPLAY
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF v-export THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
                  v-procat = ITEM.procat.
               ELSE
                  v-procat = "".

               IF ce-ctrl.r-cost = NO THEN
                  v-last-cost = ITEM.last-cost.
               ELSE
                  v-last-cost = item.avg-cost.

               PUT STREAM s-temp UNFORMATTED 
                  '"' v-procat      '",'
                  '"' item.i-no     '",'
                  '"' item.i-name   '",'
                  '"' v-last-cost   '",' 
                  '"' item.cons-uom '",'
                  '"' item.q-onh    '",'
                  '"' item.q-ono    '",'
                  '"' item.q-comm   '",'
                  '"' item.q-avail  '"'
                  SKIP.
         END.
      END.
   END.
   ELSE DO:
      IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
            "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.

      FOR EACH item NO-LOCK WHERE 
               item.company EQ cocode
          AND item.i-code  EQ "R"
          AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
          AND item.procat  GE fcat
          AND item.procat  LE tcat
         BREAK BY item.i-code
               BY item.mat-type
               BY item.procat
               BY item.i-no WITH FRAME item:
               {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
         v-printed = YES. 

         DISPLAY 
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat.

         IF v-export THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'.
         END.         

         DO WITH FRAME item2:
            FIND FIRST loc WHERE 
                       loc.company = cocode 
               AND loc.loc = item.loc NO-LOCK NO-ERROR.

            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN available loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date       
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0 
               SKIP.
         END.

         IF v-export THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
      END.
   END.
END.
IF doe THEN DO:
   IF v-printed THEN
      page.

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         'CAT,ITEM,DESCRIPTION'
         SKIP.

   FOR EACH item NO-LOCK WHERE 
            item.company EQ cocode
        AND item.i-code  EQ "E"
        AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
        AND item.procat  GE fcat
        AND item.procat  LE tcat
       BREAK BY item.i-code
             BY item.mat-type
             BY item.procat
             BY item.i-no WITH FRAME iteme:
             {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}
      DISPLAY 
         item.procat WHEN FIRST-OF(item.procat)
         item.i-no
         item.i-name.
      PUT SKIP.

      IF v-export THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '"'
            SKIP.
      END.

      RUN est-not-board (ROWID(item)).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- rm/menurep1.p 9/92 cd */
/*                                                                            */
/* raw materials costs - category sub menu                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

assign
 fco    = cocode
 tco    = cocode
 floc   = begin_whs
 tloc   = end_whs
 fcat   = begin_procat
 tcat   = end_procat
 doe    = tb_est
 dor    = tb_real
 detail = tb_detailed
 v-export = tb_excel
 v-exp-name = fi_file.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF v-export THEN
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).

if td-show-parm then run show-param.

if rd_mat-type eq "Board - Paper" then
   run run-board. 
else
if rd_mat-type eq "Ink - Coating" then 
   run run-ink.
else
if rd_mat-type eq "Film - Leaf"   then 
   run run-film. 
else
if rd_mat-type eq "Adhesives"     then 
   run run-glue.
else
if rd_mat-type eq "Case"          then 
   run run-case. 
else
if rd_mat-type eq "Miscellaneous - Adders" then 
   run run-misc.

IF v-export THEN DO:
   OUTPUT STREAM s-temp close.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-exp-name)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

OUTPUT CLOSE.
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
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

