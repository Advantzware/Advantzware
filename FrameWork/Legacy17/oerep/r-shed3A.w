&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-sched3.w

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

def var v-fcust like cust.cust-no extent 2 init ["","zzzzzzzz"] no-undo.
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999] no-undo.
def var v-fdate as date extent 2 format "99/99/9999" init [today, 12/31/9999] no-undo.
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"] no-undo.
def var v-fsman as char format "xxx" extent 2 init ["","zzz"] no-undo.
def var v-fcarr as char format "x(5)" extent 2 init ["","zzzzz"] no-undo.
DEF VAR v-floc AS CHAR FORMAT 'X(5)' EXTENT 2 INIT ['','zzzzz'] NO-UNDO.
def var v-ponum as log init yes no-undo.
def var v-sort as char format "!" init "C" no-undo.
def var v-print as char format "!" init "I" no-undo.
def var v-types as char format "x(7)" init "PALSBIC" no-undo.
def var v-comps as log init no no-undo.
def var v-by-job as log init no no-undo.
def var chosen as int init 3 no-undo.
def var v-qty like oe-rel.qty no-undo.
def var v-date like oe-rel.rel-date no-undo.
def var v-po-no like oe-rel.po-no no-undo.
def var v-rel-no like oe-rel.rel-no no-undo.
def var v-ship-id like oe-rel.ship-id no-undo.
def var v-carrier like oe-rel.carrier no-undo.  
def var v-type as char no-undo.
def var v-tot-qty as int format "->>>,>>>,>>9" EXTENT 2 no-undo.
def var v-tot-val as DEC format "->>>,>>>,>>9" extent 2 no-undo.
def var v-tot-msf as dec format "->>>,>>9.999" extent 2 no-undo.
DEF VAR ld-qty-ord AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ld-qty-rec AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ll-po AS LOG NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR v-qty-opt AS CHAR NO-UNDO.

def TEMP-TABLE w-ord
  field ord-no like oe-ord.ord-no
  field est-no like oe-ord.est-no
  field onh-qty like itemfg.q-onh
  field cust-no like oe-ord.cust-no
  field cust-name like oe-ord.cust-name
  field part-no like oe-ordl.part-no
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price
  field t-price like oe-ordl.t-price format "->>,>>>,>>9"
  field rel-qty like oe-rel.qty
  field rel-date as char format "x(9)"
  field job as char format "x(9)"
  field job-no like oe-ordl.job-no
  field job-no2 like oe-ordl.job-no2
  field rel-no like oe-rel.rel-no
  field ship-id like oe-rel.ship-id
  field po-num like oe-ordl.po-no
  field ord-qty like oe-ordl.qty
  field shp-qty like oe-ordl.ship-qty
  field msf as dec format "->>9.999"
  field component as int
  field prom-code like oe-ordl.prom-code FORMAT 'X(5)'
  field last-date like oe-ord.last-date format "99/99/99"
  field carrier like oe-relh.carrier
  field is-a-component like oe-ordl.is-a-component
  field palls as int format "->>,>>>,>>9"
  FIELD xls-rel-date  LIKE oe-rel.rel-date format "99/99/99"
  FIELD xls-status    AS CHAR.

def buffer b-w-ord for w-ord.

{fg/fullset.i new}

{custom/formtext.i NEW}

DEF VAR tb_prt-qoh AS LOG NO-UNDO.
DEF VAR tb_prt-last AS LOG NO-UNDO.
DEF VAR rd_print2 AS CHAR NO-UNDO.
DEF VAR rd_print3 AS CHAR NO-UNDO.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report FIELD qty LIKE oe-rell.qty
                                     FIELD onh LIKE fg-bin.qty.
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
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_loc end_loc begin_slsmn end_slsmn ~
begin_date end_date begin_carr end_carr tb_actual tb_backordered rd_sort ~
tb_show-only tb_show-val tg-print-due rs-item-option rd-dest lv-ornt ~
lv-font-no lines-per-page td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel RECT-6 RECT-7 RECT-10 RECT-11 RECT-12 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_loc end_loc begin_slsmn end_slsmn ~
begin_date end_date begin_carr end_carr tb_actual tb_backordered rd_sort ~
tb_show-only tb_show-val tg-print-due rs-item-option rd-dest lv-ornt ~
lv-font-no lines-per-page lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

DEFINE VARIABLE begin_carr AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
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

DEFINE VARIABLE end_carr AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-sched3.csv" 
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
     SIZE 55.2 BY 1 NO-UNDO.

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
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 99 BY 1.19 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Release Date", "Release Date",
"Item#", "Item#",
"Item Name", "Item Name",
"Territory", "Territory",
"Carrier", "Carrier",
"Credit Rating", "Credit Rating"
     SIZE 27 BY 5.62 NO-UNDO.

DEFINE VARIABLE rs-item-option AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Print Item #", "#",
"Print Item Name", "Name"
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.2 BY 6.19.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 6.19.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 6.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 6.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 13.81.

DEFINE VARIABLE tb_actual AS LOGICAL INITIAL yes 
     LABEL "Actual" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.2 BY 1.33 NO-UNDO.

DEFINE VARIABLE tb_backordered AS LOGICAL INITIAL yes 
     LABEL "Backorder" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.2 BY 1.33 NO-UNDO.

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

DEFINE VARIABLE tb_show-only AS LOGICAL INITIAL no 
     LABEL "Show only releases with Qty > On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-val AS LOGICAL INITIAL yes 
     LABEL "Show Sales Value?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-print-due AS LOGICAL INITIAL yes 
     LABEL "Print Due Alert?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 1.76 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 1.76 COL 90 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-no AT ROW 2.71 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 2.71 COL 90 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 3.67 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.67 COL 90 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_loc AT ROW 4.62 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc AT ROW 4.62 COL 90 COLON-ALIGNED HELP
          "Enter Ending Warehouse"
     begin_slsmn AT ROW 5.57 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.57 COL 90 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_date AT ROW 6.52 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6.52 COL 90 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_carr AT ROW 7.48 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Carrier Number"
     end_carr AT ROW 7.48 COL 90 COLON-ALIGNED HELP
          "Enter Ending Carrier Number"
     tb_actual AT ROW 10.62 COL 9.8
     tb_backordered AT ROW 12.29 COL 9.8
     rd_sort AT ROW 9.05 COL 48.4 NO-LABEL
     tb_show-only AT ROW 9.81 COL 79
     tb_show-val AT ROW 11 COL 79
     tg-print-due AT ROW 12.19 COL 79
     rs-item-option AT ROW 13.33 COL 79 NO-LABEL
     rd-dest AT ROW 16 COL 3.6 NO-LABEL
     lv-ornt AT ROW 17.48 COL 3 NO-LABEL
     lv-font-no AT ROW 18.76 COL 6.6 COLON-ALIGNED
     lines-per-page AT ROW 18.76 COL 30.8 COLON-ALIGNED
     lv-font-name AT ROW 20.05 COL 2.8 NO-LABEL
     td-show-parm AT ROW 18.76 COL 41
     tb_excel AT ROW 18.81 COL 99.6 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.81 COL 121.6 RIGHT-ALIGNED
     fi_file AT ROW 19.91 COL 77.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.38 COL 29.4
     btn-cancel AT ROW 21.38 COL 77.4
     "Release Types:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 8.81 COL 3.4
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.14 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Sort Options:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 8.81 COL 35.2 WIDGET-ID 12
     "Print Options" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 8.81 COL 77.8 WIDGET-ID 14
     RECT-6 AT ROW 14.81 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-10 AT ROW 8.62 COL 1 WIDGET-ID 6
     RECT-11 AT ROW 8.62 COL 35 WIDGET-ID 8
     RECT-12 AT ROW 8.62 COL 76.4 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.4 BY 21.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Actual Releases"
         HEIGHT             = 22
         WIDTH              = 123.4
         MAX-HEIGHT         = 24.95
         MAX-WIDTH          = 123.4
         VIRTUAL-HEIGHT     = 24.95
         VIRTUAL-WIDTH      = 123.4
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
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_actual:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_backordered:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_show-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-val:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Actual Releases */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Actual Releases */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Beginning Warehouse */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  RUN run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
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
      WHEN 6 THEN RUN output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_carr C-Win
ON LEAVE OF end_carr IN FRAME FRAME-A /* Ending Carrier# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME tb_backordered
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_backordered C-Win
ON VALUE-CHANGED OF tb_backordered IN FRAME FRAME-A /* Backorder */
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


&Scoped-define SELF-NAME tb_show-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-only C-Win
ON VALUE-CHANGED OF tb_show-only IN FRAME FRAME-A /* Show only releases with Qty > On Hand? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-val
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-val C-Win
ON VALUE-CHANGED OF tb_show-val IN FRAME FRAME-A /* Show Sales Value? */
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

  begin_date = today.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
          begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
          end_carr tb_actual tb_backordered rd_sort tb_show-only tb_show-val 
          tg-print-due rs-item-option rd-dest lv-ornt lv-font-no lines-per-page 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
         begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
         end_carr tb_actual tb_backordered rd_sort tb_show-only tb_show-val 
         tg-print-due rs-item-option rd-dest lv-ornt lv-font-no lines-per-page 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel RECT-6 
         RECT-7 RECT-10 RECT-11 RECT-12 
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-subt AS CHAR NO-UNDO.
  DEF VAR lv-cr-rating LIKE cust.cr-rating NO-UNDO.
  DEF VAR ll-show-top-only AS LOG NO-UNDO.
  DEF VAR ld-palls AS DEC NO-UNDO.
  DEF VAR excelheader AS CHAR NO-UNDO.
  DEF VAR tb_notes AS LOG NO-UNDO.
  DEF VAR begin_spec AS CHAR NO-UNDO.
  DEF VAR end_spec AS CHAR NO-UNDO.
  DEF VAR tb_stats AS LOG INIT NO NO-UNDO.
  DEF VAR tb_subt AS LOG INIT YES NO-UNDO.
  DEF VAR v-tot-pal LIKE v-tot-qty NO-UNDO.

  {sys/form/r-top.i}

  {sys/inc/ctrtext.i str-tit 152}.

  FORM HEADER SKIP(1)
       day_str str-tit FORMAT "x(152)" "Page" AT 164 PAGE-NUMBER FORMAT ">>9"
       SKIP
       tim_str str-tit2 FORMAT "x(152)" "{1}" AT 164 SKIP(1) 
       str-tit3 FORMAT "x(172)"
       SKIP(1)

       WITH FRAME r-top ROW 1 COLUMN 1 STREAM-IO WIDTH 200
            NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

  FORM HEADER
       "Credit Rating:"
       lv-cr-rating
       SKIP(1)

      WITH FRAME r-top2 PAGE-TOP NO-ATTR-SPACE NO-BOX WIDTH 200 STREAM-IO.

  ASSIGN v-tot-qty = 0
         v-tot-msf = 0
         v-tot-val = 0.

  ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 152}

   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-fsman[1]   = begin_slsmn
   v-fsman[2]   = end_slsmn
   v-ford-no[1] = begin_ord-no
   v-ford-no[2] = end_ord-no
   v-fitem[1]   = begin_i-no
   v-fitem[2]   = end_i-no
   v-floc[1]    = begin_loc
   v-floc[2]    = end_loc
   v-fdate[1]   = begin_date
   v-fdate[2]   = end_date
   v-fcarr[1]   = begin_carr
   v-fcarr[2]   = end_carr
   v-sort       = IF rd_sort EQ "Customer#"     THEN "C"  ELSE
                  IF rd_sort EQ "Release Date"  THEN "R"  ELSE
                  IF rd_sort EQ "Item#"         THEN "I"  ELSE
                  IF rd_sort EQ "Item Name"     THEN "N"  ELSE
                  IF rd_sort EQ "Territory"     THEN "T"  ELSE
                  IF rd_sort EQ "Credit Rating" THEN "CR" ELSE "A"
   v-types      = STRING(tb_actual,"A/")      + STRING(tb_backordered,"B/")

   str-tit3 = (IF v-sort EQ "C"  THEN "By Customer By Date"      ELSE
               IF v-sort EQ "R"  THEN "By Date By Customer"      ELSE
               IF v-sort EQ "I"  THEN "By Item By Date"          ELSE
               IF v-sort EQ "N"  THEN "By Item Name By Date"     ELSE
               IF v-sort EQ "A"  THEN "By Carrier By Date"       ELSE
               IF v-sort EQ "CR" THEN "By Credit Rating By Date" ELSE
                                      "By Territory By Date")    + "  "   +
               STRING(v-fdate[1],"99/99/9999")                   + " to " +
               STRING(v-fdate[2],"99/99/9999")

   {sys/inc/ctrtext.i str-tit3 172}.

  IF tb_show-val THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
    tb_show-val = ll-secure.
    DISPLAY tb_show-val.
  END.

  IF tb_excel THEN DO:
    excelheader = "Customer Name,Release Date,Rel Num,Ship To,Carrier," +
                  "Order Number,Customer Part#,Description,FG Item#,"   +
                  "Po Number,Quantity On Hand,release Qty,Sales Value," +
                  "No. of Pallets,Status".

    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
  END.

  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  SESSION:SET-WAIT-STATE ("general").

  IF td-show-parm THEN RUN show-param.

  VIEW FRAME r-top.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  FOR EACH w-ord:
    DELETE w-ord.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.ord-no  GE v-ford-no[1]
        AND oe-ordl.ord-no  LE v-ford-no[2]
        AND oe-ordl.i-no    GE v-fitem[1]
        AND oe-ordl.i-no    LE v-fitem[2]
        AND ((oe-ordl.s-man[1] GE v-fsman[1] AND
              oe-ordl.s-man[1] LE v-fsman[2]) OR
             (oe-ordl.s-man[2] GE v-fsman[1] AND
              oe-ordl.s-man[2] LE v-fsman[2]) OR
             (oe-ordl.s-man[3] GE v-fsman[1] AND
              oe-ordl.s-man[3] LE v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,

      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2]
      NO-LOCK,

      FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK:

    STATUS DEFAULT "Processing Order#/FG#: " +
                   TRIM(STRING(oe-ordl.ord-no,">>>>>>>>")) + "/" +
                   TRIM(oe-ordl.i-no).

    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]
          AND oe-relh.carrier  GE v-fcarr[1]
          AND oe-relh.carrier  LE v-fcarr[2]
        USE-INDEX r-no

        BREAK BY oe-rell.r-no
              BY oe-rell.ord-no
              BY oe-rell.i-no
              BY oe-rell.line
              BY oe-rell.rel-no
              BY oe-rell.b-ord-no
              BY oe-rell.po-no:

      IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

      lv-qty = lv-qty + oe-rell.qty.

      IF LAST-OF(oe-rell.po-no) THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = IF v-sort EQ "R" THEN
                               (STRING(YEAR(oe-relh.rel-date),"9999") +
                                STRING(MONTH(oe-relh.rel-date),"99")  +
                                STRING(DAY(oe-relh.rel-date),"99"))
                             ELSE
                             IF v-sort EQ "N" THEN oe-ordl.i-name
                             ELSE ""
         tt-report.key-02  = IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rell.i-no
                             ELSE
                             IF v-sort EQ "T" THEN cust.terr
                             ELSE
                             IF v-sort EQ "A" THEN oe-relh.carrier
                             ELSE
                             IF v-sort EQ "CR" THEN cust.cr-rating
                             ELSE oe-relh.cust-no
         tt-report.key-03  = IF v-sort NE "R" THEN
                               (STRING(YEAR(oe-relh.rel-date),"9999") +
                                STRING(MONTH(oe-relh.rel-date),"99")  +
                                STRING(DAY(oe-relh.rel-date),"99"))
                             ELSE ""
         tt-report.key-04  = STRING(IF v-sort EQ "A" THEN oe-relh.cust-no
                                                     ELSE " ","x(10)") +
                             STRING(oe-ord.ord-no,"9999999999")
         tt-report.key-05  = STRING(INDEX(v-types,v-type),"99")
         tt-report.key-06  = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
         tt-report.qty     = lv-qty
         tt-report.rec-id  = RECID(oe-rell).

        FOR EACH fg-bin
            WHERE fg-bin.company EQ oe-ordl.company
              AND fg-bin.i-no    EQ oe-ordl.i-no
              AND fg-bin.job-no  EQ oe-ordl.job-no
              AND fg-bin.job-no2 EQ oe-ordl.job-no2
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            USE-INDEX job no-lock:
          tt-report.onh = tt-report.onh + fg-bin.qty.
        END.
      END.
    END.
  END.

  STATUS DEFAULT "Printing...".

  IF tb_show-only THEN
  FOR EACH tt-report
      WHERE tt-report.term-id EQ ""
        AND tt-report.qty     LE tt-report.onh:
    DELETE tt-report.
  END.

  IF NOT CAN-FIND(FIRST tt-report WHERE tt-report.term-id EQ "") THEN DO:
    CREATE tt-report.
    ASSIGN
     tt-report.term-id = ""
     ll-show-top-only  = YES.
  END.

  RELEASE tt-report.

  for each tt-report where tt-report.term-id eq "",
      FIRST oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock,
      first oe-relh
      where oe-relh.company eq oe-rell.company
        and oe-relh.r-no    eq oe-rell.r-no
      use-index r-no NO-LOCK,
      first oe-ordl
      where oe-ordl.company eq oe-rell.company
        and oe-ordl.ord-no  eq oe-rell.ord-no
        and oe-ordl.i-no    eq oe-rell.i-no
        and oe-ordl.line    eq oe-rell.line
      NO-LOCK,
      first oe-ord of oe-ordl no-lock,
      first cust
      where cust.company eq oe-ord.company
        and cust.cust-no eq oe-ord.cust-no
      no-lock
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04:

    IF v-sort EQ "CR" AND FIRST-OF(tt-report.key-02) THEN DO:
      lv-cr-rating = tt-report.key-02.
      IF FIRST(tt-report.key-02) THEN VIEW FRAME r-top2.
      PAGE.
    END.

    ELSE
      IF FIRST(tt-report.key-01) THEN PAGE.

    v-qty = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty.

    create w-ord.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-ordl.i-no
        no-lock.

    assign
     w-ord.ord-no    = oe-ord.ord-no
     w-ord.cust-no   = oe-ord.cust-no
     w-ord.cust-name = oe-ord.cust-name
     w-ord.part-no   = oe-ordl.part-no
     w-ord.i-no      = oe-ordl.i-no
     w-ord.i-name    = oe-ordl.i-name
     w-ord.qty       = oe-ordl.qty
     w-ord.cost      = oe-ordl.cost
     w-ord.price     = oe-ordl.t-price / oe-ordl.qty
     w-ord.rel-qty   = v-qty
     w-ord.onh-qty   = tt-report.onh
     w-ord.t-price   = w-ord.price * w-ord.rel-qty
     w-ord.rel-date  = STRING(oe-relh.rel-date) + tt-report.key-06
     w-ord.rel-no    = oe-relh.release#
     w-ord.ship-id   = oe-relh.ship-id
     w-ord.job-no    = oe-ordl.job-no
     w-ord.job-no2   = oe-ordl.job-no2
     w-ord.job       = IF w-ord.job-no eq "" then "" ELSE
                       (trim(w-ord.job-no) + "-" +
                        STRING(w-ord.job-no2,"99"))
     w-ord.po-num    = oe-rell.po-no
     w-ord.ord-qty   = oe-ordl.qty
     w-ord.shp-qty   = oe-ordl.ship-qty
     w-ord.msf       = w-ord.rel-qty * itemfg.t-sqft / 1000
     w-ord.prom-code = oe-ordl.prom-code
     w-ord.last-date = oe-ord.last-date
     w-ord.carrier   = oe-relh.carrier
     w-ord.is-a-component = oe-ordl.is-a-component
     ld-palls        = w-ord.rel-qty /
                       ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                        (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit)).

    {sys/inc/roundup.i ld-palls}

    IF ld-palls LT 0 THEN ld-palls = ld-palls * -1.

    w-ord.palls = w-ord.palls + ld-palls.

    IF NOT FIRST-OF(tt-report.key-02) AND v-sort EQ "C" THEN w-ord.cust-name = "".

    IF v-comps AND itemfg.isaset THEN DO:
      RUN fg/fullset.p (ROWID(itemfg)).

      FOR EACH tt-fg-set,
          FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:

        CREATE b-w-ord.
        BUFFER-COPY w-ord TO b-w-ord
        ASSIGN
         b-w-ord.component = 1
         b-w-ord.cust-name = ""
         b-w-ord.part-no   = itemfg.part-no
         b-w-ord.i-no      = tt-fg-set.part-no
         b-w-ord.i-name    = itemfg.i-name
         b-w-ord.price     = 0
         b-w-ord.cost      = 0
         b-w-ord.t-price   = 0
         b-w-ord.job       = ""
         b-w-ord.po-num    = ""
         b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
         b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
         b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
         b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
         b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
      END.
    END.

    FOR EACH w-ord
        BREAK BY w-ord.component
              BY w-ord.i-no:

      IF NOT tb_show-val THEN w-ord.t-price = 0.

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg OR ll-show-top-only THEN DO:
        {oe/rep/schdrel.i}
      END.
    END.

    IF NOT ll-show-top-only THEN DO:
      FIND FIRST w-ord.

      ASSIGN
       v-tot-qty[1] = v-tot-qty[1] + 1
       v-tot-msf[1] = v-tot-msf[1] + w-ord.msf
       v-tot-val[1] = v-tot-val[1] + w-ord.t-price
       v-tot-pal[1] = v-tot-pal[1] + w-ord.palls.

      IF LAST-OF(tt-report.key-02) THEN DO:
        IF v-sort EQ "C" THEN
          PUT "Customer Totals:" TO 140.
        ELSE
          PUT "      Subtotals:" TO 140.

        PUT v-tot-val[1] TO 156 FORMAT "$->>,>>>,>>9.99"
            v-tot-pal[1] TO 164 FORMAT ">>>,>>9"
            SKIP(2).

        ASSIGN
         v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
         v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
         v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
         v-tot-pal[2] = v-tot-pal[2] + v-tot-pal[1]
         v-tot-qty[1] = 0
         v-tot-val[1] = 0
         v-tot-msf[1] = 0
         v-tot-pal[1] = 0.
      END.

      IF LAST(tt-report.key-01) THEN DO:
        PUT SKIP(1)
            "Report Totals:" TO 140
            v-tot-val[2]     TO 156 FORMAT "$->>,>>>,>>9.99"
            v-tot-pal[2]     TO 164 FORMAT ">>>,>>9".
      END.
    END.

    FOR EACH w-ord:
      DELETE w-ord.
    END.
  END. /* each tt-report */

  STATUS DEFAULT "".

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

