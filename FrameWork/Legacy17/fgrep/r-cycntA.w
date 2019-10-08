&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-cycnt.w

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
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
{custom/xprint.i}

DEF VAR lv-multi-faxout AS LOG NO-UNDO.  /*for faxing to multiple receipents */
DEF VAR lv-fax-image AS cha NO-UNDO.  /* fax imge file */
DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

DEF VAR lv-date      AS DATE                 NO-UNDO.
DEF VAR lv-job-no    AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR li-palls     AS DEC FORMAT "->>,>>9" NO-UNDO.
DEF VAR v-cnt        AS INT                  NO-UNDO.
DEF VAR v-item-no    LIKE fg-bin.i-no        NO-UNDO.
DEF VAR v-i-name     LIKE itemfg.i-name      NO-UNDO.
DEF VAR v-itemfg     LIKE itemfg.cust-no     NO-UNDO.
DEF VAR v-tag        LIKE fg-bin.tag         NO-UNDO.
DEF VAR v-qty        AS CHAR                 NO-UNDO.
DEF VAR v-li-palls   AS CHAR                 NO-UNDO.
DEF VAR v-prnt-onh   AS LOG INIT "N"         NO-UNDO.
DEF VAR v-writein    AS CHAR FORMAT "X(21)" INIT "    _________________"   NO-UNDO.


DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR is-xprint AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF STREAM excel.

FORM
   fg-bin.i-no     LABEL "ITEM"
   itemfg.i-name   FORMAT "x(25)" LABEL "DESCRIPTION"
   itemfg.cust-no  LABEL "CUSTOMER"
   fg-bin.loc      LABEL "WHSE"
   fg-bin.loc-bin  LABEL "BIN"
   fg-bin.tag      LABEL "TAG" FORMAT "x(8)"
   lv-job-no       LABEL "JOB#"
   lv-date         FORMAT "99/99/99" LABEL "RCT DATE"
   fg-bin.qty      FORMAT "->>,>>>,>>9" LABEL "ON HAND"
   li-palls        LABEL "PALLETS"
   v-writein       LABEL "     QUANTITY COUNTED"
   SKIP         
WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 200.

FORM
   fg-bin.i-no     LABEL "ITEM"
   itemfg.i-name   FORMAT "x(25)" LABEL "DESCRIPTION"
   itemfg.cust-no  LABEL "CUSTOMER"
   fg-bin.loc      LABEL "WHSE"
   fg-bin.loc-bin  LABEL "BIN"
   fg-bin.tag      LABEL "TAG" FORMAT "x(8)"
   lv-job-no       LABEL "JOB#"
   lv-date         FORMAT "99/99/99" LABEL "RCT DATE"
   fg-bin.qty      FORMAT "->>,>>>,>>9" LABEL "ON HAND"
   /*li-palls        LABEL "PALLETS"*/
   v-writein       LABEL "     QUANTITY COUNTED"
   SKIP         
WITH FRAME itemx2 NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 200.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_loc end_loc begin_i-no ~
end_i-no begin_code end_code begin_bin end_bin begin_cat end_cat ~
begin_cust-no end_cust-no rd_sort tb_qty-oh tb_zero tb_prt-pallet ~
tb_prt-cust-owned rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_loc end_loc begin_i-no end_i-no ~
begin_code end_code begin_bin end_bin begin_cat end_cat begin_cust-no ~
end_cust-no lbl_sort rd_sort tb_qty-oh tb_zero tb_prt-pallet ~
tb_prt-cust-owned rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_bin AS CHARACTER FORMAT "X(8)" 
     LABEL "From Bin" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Category" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_code AS CHARACTER FORMAT "XX" 
     LABEL "From Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)" 
     LABEL "From Location" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_bin AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Bin" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "To Category" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_code AS CHARACTER FORMAT "XX" INITIAL "zz" 
     LABEL "To Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To Location" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cycnt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=6 (20 cpi for 150 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
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
     SIZE 23 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item", "Item",
"Bin", "Bin",
"Name", "Name",
"Customer", "Cust"
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 13.33.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prt-cust-owned AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-pallet AS LOGICAL INITIAL no 
     LABEL "Print Pallets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_qty-oh AS LOGICAL INITIAL no 
     LABEL "Print Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL no 
     LABEL "Print Zero Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_loc AT ROW 2.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Location"
     end_loc AT ROW 2.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Location"
     begin_i-no AT ROW 3.62 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_code AT ROW 4.57 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Cycle Count Code"
     end_code AT ROW 4.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Cycle Count Code"
     begin_bin AT ROW 5.52 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Bin"
     end_bin AT ROW 5.52 COL 70 COLON-ALIGNED HELP
          "Enter Ending Bin"
     begin_cat AT ROW 6.48 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 6.48 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cust-no AT ROW 7.43 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 7.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     lbl_sort AT ROW 8.86 COL 23 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.86 COL 35 HELP
          "Sort By Item, Bin, or Item Name" NO-LABEL
     tb_qty-oh AT ROW 10.05 COL 35
     tb_zero AT ROW 11 COL 35
     tb_prt-pallet AT ROW 11.95 COL 35
     tb_prt-cust-owned AT ROW 12.91 COL 35 WIDGET-ID 8
     rd-dest AT ROW 15.62 COL 5 NO-LABEL
     lv-ornt AT ROW 15.62 COL 32 NO-LABEL
     lines-per-page AT ROW 15.62 COL 86 COLON-ALIGNED
     lv-font-no AT ROW 16.76 COL 36 COLON-ALIGNED
     lv-font-name AT ROW 17.71 COL 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19 COL 32
     tb_excel AT ROW 20.14 COL 67 RIGHT-ALIGNED WIDGET-ID 2
     tb_runExcel AT ROW 20.14 COL 88.6 RIGHT-ALIGNED WIDGET-ID 4
     fi_file AT ROW 21.1 COL 45 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 23.62 COL 24
     btn-cancel AT ROW 23.62 COL 51.2
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.67 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     RECT-6 AT ROW 14.1 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.6 BY 24.29.


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
         TITLE              = "Finished Goods Cycle Count Report"
         HEIGHT             = 24.29
         WIDTH              = 96.6
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_prt-cust-owned:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Cycle Count Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Cycle Count Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bin C-Win
ON LEAVE OF begin_bin IN FRAME FRAME-A /* From Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* From Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON HELP OF begin_code IN FRAME FRAME-A /* From Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeFG.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON LEAVE OF begin_code IN FRAME FRAME-A /* From Cycle Count Code */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* From Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* From Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* From Location */
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
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_loc
                            &END_cust=END_loc
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_loc
                             &END_cust=end_loc
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_loc
                                  &END_cust=end_loc
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bin C-Win
ON LEAVE OF end_bin IN FRAME FRAME-A /* To Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* To Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON HELP OF end_code IN FRAME FRAME-A /* To Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeFG.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON LEAVE OF end_code IN FRAME FRAME-A /* To Cycle Count Code */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* To Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* To Location */
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


&Scoped-define SELF-NAME tb_prt-cust-owned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust-owned C-Win
ON VALUE-CHANGED OF tb_prt-cust-owned IN FRAME FRAME-A /* Include Customer Owned? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-pallet C-Win
ON VALUE-CHANGED OF tb_prt-pallet IN FRAME FRAME-A /* Print Pallets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qty-oh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-oh C-Win
ON VALUE-CHANGED OF tb_qty-oh IN FRAME FRAME-A /* Print Quantity On Hand? */
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


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Print Zero Balance Items? */
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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
     APPLY "entry" TO begin_loc.
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
  DISPLAY begin_loc end_loc begin_i-no end_i-no begin_code end_code begin_bin 
          end_bin begin_cat end_cat begin_cust-no end_cust-no lbl_sort rd_sort 
          tb_qty-oh tb_zero tb_prt-pallet tb_prt-cust-owned rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_loc end_loc begin_i-no end_i-no begin_code 
         end_code begin_bin end_bin begin_cat end_cat begin_cust-no end_cust-no 
         rd_sort tb_qty-oh tb_zero tb_prt-pallet tb_prt-cust-owned rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-date C-Win 
PROCEDURE get-first-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-date AS DATE NO-UNDO.


  op-date = fg-bin.aging-date.

  IF fg-bin.tag EQ "" THEN
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ fg-bin.company
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2
      USE-INDEX tran NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND (TRIM(fg-bin.job-no) NE "" OR
             (fg-rdtlh.loc     EQ fg-bin.loc     AND
              fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND
              fg-rdtlh.tag     EQ fg-bin.tag AND
              fg-rdtlh.cust-no EQ fg-bin.cust-no))
      USE-INDEX rm-rdtl NO-LOCK

      BY fg-rcpth.trans-date
      BY fg-rdtlh.trans-time:

    op-date = fg-rcpth.trans-date.
    LEAVE.
  END.

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company   EQ fg-bin.company
        AND fg-rdtlh.loc       EQ fg-bin.loc
        AND fg-rdtlh.tag       EQ fg-bin.tag
        AND fg-rdtlh.cust-no   EQ fg-bin.cust-no
        AND (TRIM(fg-bin.job-no) NE "" OR
             (fg-rdtlh.loc     EQ fg-bin.loc     AND
              fg-rdtlh.loc-bin EQ fg-bin.loc-bin))
      USE-INDEX tag NO-LOCK,

      EACH fg-rcpth
      WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
        AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2
      USE-INDEX r-no NO-LOCK

      BY fg-rcpth.trans-date
      BY fg-rdtlh.trans-time:

    op-date = fg-rcpth.trans-date.
    LEAVE.
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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- fg/rep/fg-cyclc.p 10/93 cd */
/* Finished Goods - Cycle Count Code List                                     */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

DEF VAR v-floc       AS CHAR FORMAT "x(5)" INIT ""                         NO-UNDO.
DEF VAR v-tloc       LIKE v-floc INIT "zzzzz"                              NO-UNDO.
DEF VAR v-cat        AS CHAR FORMAT "x(5)" EXTENT 2 INIT ["","zzzzz"]      NO-UNDO.
DEF VAR v-cust       AS CHAR FORMAT "x(8)" EXTENT 2 INIT ["","zzzzzzzz"]   NO-UNDO.
DEF VAR v-i-no       AS CHAR FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-code       AS CHAR FORMAT "XX" EXTENT 2 INIT ["","zz"]           NO-UNDO.
DEF VAR v-loc-bin    AS CHAR FORMAT "x(8)" EXTENT 2 INIT ["", "zzzzzzzz"]  NO-UNDO.
DEF VAR v-item-bin   AS CHAR FORMAT "!" INIT "I" NO-UNDO.
DEF VAR v-prnt-zer   AS LOG INIT "N"         NO-UNDO.
DEF VAR v-include-cust-owned AS LOG NO-UNDO.
DEF VAR excelheader  AS CHAR                 NO-UNDO.


IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).

   IF tb_prt-pallet = TRUE THEN
      excelheader = "ITEM,DESCRIPTION,CUSTOMER,WHSE,BIN,TAG,JOB#,RCT DATE,ON HAND,PALLETS,QUANTITY COUNTED".
   ELSE
      excelheader = "ITEM,DESCRIPTION,CUSTOMER,WHSE,BIN,TAG,JOB#,RCT DATE,ON HAND,QUANTITY COUNTED".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}
   v-floc        = begin_loc
   v-tloc        = end_loc
   v-i-no[1]     = begin_i-no
   v-i-no[2]     = end_i-no
   v-code[1]     = begin_code
   v-code[2]     = END_code
   v-loc-bin[1]  = begin_bin
   v-loc-bin [2] = END_bin
   v-cat[1]      = begin_cat
   v-cat[2]      = END_cat
   v-cust[1]     = begin_cust-no
   v-cust[2]     = end_cust-no
   v-item-bin    = SUBSTR(rd_sort,1,1)
   v-prnt-onh    = tb_qty-oh
   v-prnt-zer    = tb_zero
   v-include-cust-owned = tb_prt-cust-owned.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

EMPTY TEMP-TABLE tt-report.

FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                          AND fg-bin.i-no    GE v-i-no[1]
                          AND fg-bin.i-no    LE v-i-no[2]
                          AND fg-bin.loc     GE v-floc
                          AND fg-bin.loc     LE v-tloc
                          AND fg-bin.loc-bin GE v-loc-bin[1]
                          AND fg-bin.loc-bin LE v-loc-bin[2]
                          AND (fg-bin.qty    NE 0 OR v-prnt-zer) 
                          AND (IF fg-bin.cust-no GT "" THEN v-include-cust-owned
                               ELSE TRUE)
                          USE-INDEX co-ino,
   FIRST itemfg FIELDS(i-name cust-no) WHERE itemfg.company EQ fg-bin.company
                  AND itemfg.i-no    EQ fg-bin.i-no
                  AND itemfg.cc-code GE v-code[1]
                  AND itemfg.cc-code LE v-code[2]
                  AND itemfg.cust-no GE v-cust[1]
                  AND itemfg.cust-no LE v-cust[2]
                  AND itemfg.procat  GE v-cat[1]
                  AND itemfg.procat  LE v-cat[2] NO-LOCK:

   CREATE tt-report.
   ASSIGN
      tt-report.rec-id = RECID(fg-bin)
      tt-report.key-01 = IF v-item-bin EQ "B" OR v-item-bin EQ "C"
                           THEN 
                            STRING(fg-bin.loc,"x(10)") +
                            STRING(fg-bin.loc-bin,"x(10)") +
                            STRING(fg-bin.tag,"x(30)") +
                            STRING(fg-bin.cust-no,"x(10)")
                           ELSE IF v-item-bin EQ "N" THEN itemfg.i-name  ELSE ""
      tt-report.key-02 = fg-bin.i-no
     /* gdm - 10160901 */
      tt-report.key-03 = itemfg.cust-no
       /* this should always come from itemfg
                         IF fg-bin.cust-no NE ""
                          THEN fg-bin.cust-no 
                           ELSE itemfg.cust-no
       */
      tt-report.key-04 = TRIM(STRING(fg-bin.loc,"x(10)"))
      tt-report.key-05 = TRIM(STRING(fg-bin.loc-bin,"x(10)"))
      .
END.

ASSIGN lv-date    = ?
       lv-job-no  = ""
       li-palls   = 0
       v-cnt      = 0
       v-item-no  = ""
       v-i-name   = ""
       v-itemfg   = ""
       v-tag      = ""
       v-qty      = ""
       v-li-palls = "".

IF v-item-bin EQ "C" THEN RUN run-reportCust.
ELSE DO:

 FOR EACH tt-report WHERE tt-report.term-id EQ "",
   FIRST fg-bin NO-LOCK WHERE RECID(fg-bin) EQ tt-report.rec-id,
   FIRST itemfg NO-LOCK WHERE itemfg.company EQ fg-bin.company
                          AND itemfg.i-no    EQ fg-bin.i-no
   BREAK BY tt-report.key-01
         BY tt-report.key-02
         BY tt-report.key-03:

   RUN get-first-date (OUTPUT lv-date).

   lv-job-no = TRIM(fg-bin.job-no).

   IF lv-job-no NE "" THEN 
      lv-job-no = lv-job-no + STRING(fg-bin.job-no2,"99").

   ASSIGN
      li-palls = (IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)   *
                 (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                 (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)
      li-palls = fg-bin.qty / li-palls.

   {sys/inc/roundup.i li-palls}

   IF FIRST-OF(tt-report.key-02) THEN DO:
      ASSIGN
         v-cnt = 1
         v-item-no   = fg-bin.i-no   
         v-i-name    = itemfg.i-name.
      IF fg-bin.cust-no NE "" THEN
         v-itemfg = fg-bin.cust-no.
      ELSE
         v-itemfg = itemfg.cust-no.
   END.
   ELSE DO:
      ASSIGN
         v-item-no = ""
         v-itemfg  = "".
      IF v-cnt = 2 THEN
         v-i-name = itemfg.part-dscr1.
      ELSE 
         v-i-name = "".
   END.

   v-cnt = v-cnt + 1.

   IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN
      v-tag = SUBSTR(fg-bin.tag,16,8).
   ELSE
      v-tag = fg-bin.tag.

   IF v-prnt-onh = TRUE THEN
      ASSIGN
         v-qty       = STRING(fg-bin.qty)
         v-li-palls  = STRING(li-palls).
   ELSE
      ASSIGN
         v-qty       = ""
         v-li-palls  = "".

   IF tb_prt-pallet THEN DO :
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-02)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         li-palls       WHEN v-prnt-onh
         v-writein
      WITH FRAME itemx.            
      DOWN WITH FRAME itemx.

      DISPLAY 
         itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
      WITH FRAME itemx.           
      DOWN WITH FRAME itemx.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
             '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF first-of(tt-report.key-02) THEN itemfg.cust-no ELSE "")  '",'
             '"' fg-bin.loc       '",'
             '"' fg-bin.loc-bin   '",'
             '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
             '"' lv-job-no '",'
             '"' lv-date   '",'
             '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
             '"' (IF v-prnt-onh THEN STRING(li-palls) ELSE "")  '",'
             '"' v-writein '",'
             SKIP
             '"' "" '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
             SKIP.
      END.
   END.
   ELSE DO:
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-02)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         /* li-palls       WHEN v-prnt-onh*/
         v-writein
      WITH FRAME itemx2.            
      DOWN WITH FRAME itemx2.

      DISPLAY itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
         WITH FRAME itemx2.           
      DOWN WITH FRAME itemx2.

      IF tb_excel THEN 
         PUT STREAM excel UNFORMATTED
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
             '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF first-of(tt-report.key-02) THEN itemfg.cust-no ELSE "")  '",'
             '"' fg-bin.loc       '",'
             '"' fg-bin.loc-bin   '",'
             '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
             '"' lv-job-no '",'
             '"' lv-date          '",'
             '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
             '"' v-writein '",'
             SKIP
             '"' "" '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
             SKIP.
   END.
   IF LAST-OF(tt-report.key-02) THEN PUT SKIP(1).

 END.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-reportCust C-Win 
PROCEDURE run-reportCust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-report 
  WHERE tt-report.term-id EQ "",
 FIRST fg-bin NO-LOCK 
  WHERE RECID(fg-bin) EQ tt-report.rec-id,
 FIRST itemfg NO-LOCK 
  WHERE itemfg.company EQ fg-bin.company
    AND itemfg.i-no    EQ fg-bin.i-no
 BREAK BY tt-report.key-03
       BY tt-report.key-02
       BY tt-report.key-04
       BY tt-report.key-05:

  RUN get-first-date (OUTPUT lv-date).

  ASSIGN lv-job-no = TRIM(fg-bin.job-no).

  IF lv-job-no NE "" THEN
     ASSIGN lv-job-no = lv-job-no + STRING(fg-bin.job-no2,"99").

  ASSIGN li-palls = (IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count) 
                  * (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   
                  * (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)
         li-palls = fg-bin.qty / li-palls.

  {sys/inc/roundup.i li-palls}

  IF FIRST-OF(tt-report.key-03) THEN DO:

      ASSIGN
         v-cnt = 1
         v-item-no   = fg-bin.i-no   
         v-i-name    = itemfg.i-name.
      IF fg-bin.cust-no NE "" THEN
         v-itemfg = fg-bin.cust-no.
      ELSE
         v-itemfg = itemfg.cust-no.
  END.
  ELSE DO:
     ASSIGN
        v-item-no = ""
        v-itemfg  = "".
     IF v-cnt = 2 THEN
        v-i-name = itemfg.part-dscr1.
     ELSE 
        v-i-name = "".
  END.

  ASSIGN v-cnt = v-cnt + 1.

  IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN
     ASSIGN v-tag = SUBSTR(fg-bin.tag,16,8).
  ELSE
     ASSIGN v-tag = fg-bin.tag.

  IF v-prnt-onh = TRUE THEN
     ASSIGN v-qty       = STRING(fg-bin.qty)
            v-li-palls  = STRING(li-palls).
  ELSE
     ASSIGN v-qty       = ""
            v-li-palls  = "".

  IF tb_prt-pallet THEN DO:
     DISPLAY 
        fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
        itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
        itemfg.cust-no WHEN FIRST-OF(tt-report.key-03)
         v-itemfg      WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
        lv-job-no
        fg-bin.loc
        fg-bin.loc-bin
        fg-bin.tag
        SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
        lv-date
        fg-bin.qty     WHEN v-prnt-onh
        li-palls       WHEN v-prnt-onh
        v-writein
     WITH FRAME itemx.            
     DOWN WITH FRAME itemx.

     DISPLAY 
        itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
     WITH FRAME itemx.           
     DOWN WITH FRAME itemx.

     IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
            '"' (IF fg-bin.cust-no NE "" THEN v-itemfg ELSE IF FIRST-OF(tt-report.key-03) THEN itemfg.cust-no ELSE "")  '",'
            '"' fg-bin.loc       '",'
            '"' fg-bin.loc-bin   '",'
            '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
            '"' lv-job-no '",'
            '"' lv-date   '",'
            '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
            '"' (IF v-prnt-onh THEN STRING(li-palls) ELSE "")  '",'
            '"' v-writein '",'
            SKIP
            '"' "" '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
            SKIP.
  END.
  ELSE DO:
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-03)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         /* li-palls       WHEN v-prnt-onh*/
         v-writein
      WITH FRAME itemx2.            
      DOWN WITH FRAME itemx2.

      DISPLAY itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
         WITH FRAME itemx2.           
      DOWN WITH FRAME itemx2.

      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
            '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF FIRST-OF(tt-report.key-03) THEN itemfg.cust-no ELSE "")  '",'
            '"' fg-bin.loc       '",'
            '"' fg-bin.loc-bin   '",'
            '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
            '"' lv-job-no '",'
            '"' lv-date   '",'
            '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
            '"' v-writein '",'
            SKIP
            '"' "" '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
            SKIP.
   END.
   IF LAST-OF(tt-report.key-02) THEN PUT SKIP(1).
END.

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

