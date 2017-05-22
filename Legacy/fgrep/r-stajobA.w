&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-stajob.w

  Description: Finished Goods Inventory Status by Job

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

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""IL8"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-sales-rep AS CHAR NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEFINE TEMP-TABLE tt-oe-rel NO-UNDO
    FIELD rel-no AS INT 
    FIELD rel-date AS CHAR
    FIELD tot-qty AS DECIMAL
    FIELD bl-ank AS CHAR .


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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-8 tb_cust-list btnCustList ~
begin_cust end_cust begin_cust-po end_cust-po begin_slm end_slm rd_itm-code ~
rd_ostat rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust tb_rcpt-date tb_part ~
rd-dest lv-ornt td-show-parm lines-per-page lv-font-no tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_cust-po end_cust-po begin_slm end_slm lbl_itm-code rd_itm-code ~
lbl_ostat rd_ostat lbl_print rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust ~
tb_rcpt-date tb_part rd-dest lv-ornt td-show-parm lines-per-page lv-font-no ~
lv-font-name tb_excel tb_runExcel fi_file 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-stajob.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_itm-code AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_print AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

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
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_itm-code AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Stocked", "Stocked",
"Custom", "Custom",
"Both", "Both"
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE rd_ostat AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"Both", "Both"
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_smry-dtl AS CHARACTER INITIAL "S" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Summary", "S",
"Detail", "D"
     SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.62.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inc-zero AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_part AS LOGICAL INITIAL yes 
     LABEL "Print Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rcpt-date AS LOGICAL INITIAL no 
     LABEL "Receipt Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort By Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.76 COL 32.2 WIDGET-ID 6
     btnCustList AT ROW 1.81 COL 64.2 WIDGET-ID 8
     begin_cust AT ROW 2.95 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.95 COL 72 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-po AT ROW 3.91 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer PO Number"
     end_cust-po AT ROW 3.91 COL 72 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_slm AT ROW 4.86 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 4.86 COL 72 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     lbl_itm-code AT ROW 6.29 COL 23 COLON-ALIGNED NO-LABEL
     rd_itm-code AT ROW 6.29 COL 38 NO-LABEL
     lbl_ostat AT ROW 7.24 COL 21 COLON-ALIGNED NO-LABEL
     rd_ostat AT ROW 7.24 COL 38 NO-LABEL
     lbl_print AT ROW 8.14 COL 28.8 COLON-ALIGNED NO-LABEL
     rd_smry-dtl AT ROW 8.19 COL 38 NO-LABEL
     tb_sort AT ROW 9 COL 59 RIGHT-ALIGNED
     tb_inc-zero AT ROW 9.95 COL 38
     tb_inc-cust AT ROW 10.81 COL 79 RIGHT-ALIGNED
     tb_rcpt-date AT ROW 11.62 COL 58 RIGHT-ALIGNED
     tb_part AT ROW 12.38 COL 38
     rd-dest AT ROW 14.86 COL 4 NO-LABEL
     lv-ornt AT ROW 14.86 COL 31 NO-LABEL
     td-show-parm AT ROW 15.81 COL 31
     lines-per-page AT ROW 17.33 COL 83.4 COLON-ALIGNED
     lv-font-no AT ROW 17.38 COL 28.6 COLON-ALIGNED
     lv-font-name AT ROW 18.38 COL 28.4 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 19.91 COL 62.6 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.91 COL 84.6 RIGHT-ALIGNED
     fi_file AT ROW 20.81 COL 40.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.57 COL 21
     btn-cancel AT ROW 23.57 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.91 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 13.67 COL 1
     RECT-8 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.29.


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
         TITLE              = "Finished Goods Inventory Status By Job"
         HEIGHT             = 24.52
         WIDTH              = 95.4
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_itm-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_itm-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_itm-code".

/* SETTINGS FOR FILL-IN lbl_ostat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ostat".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_smry-dtl".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_itm-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_smry-dtl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_part:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rcpt-date IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_rcpt-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sort IN FRAME FRAME-A
   ALIGN-R                                                              */
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
ON END-ERROR OF C-Win /* Finished Goods Inventory Status By Job */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Inventory Status By Job */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po C-Win
ON LEAVE OF begin_cust-po IN FRAME FRAME-A /* Beginning Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END.

  run run-report.
  STATUS DEFAULT "Processing Complete". 
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_cust 
                            &END_cust=END_cust 
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust 
                             &END_cust=END_cust 
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust 
                                  &END_cust=END_cust 
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po C-Win
ON LEAVE OF end_cust-po IN FRAME FRAME-A /* Ending Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME rd_itm-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_itm-code C-Win
ON VALUE-CHANGED OF rd_itm-code IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ostat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ostat C-Win
ON VALUE-CHANGED OF rd_ostat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
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


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-zero C-Win
ON VALUE-CHANGED OF tb_inc-zero IN FRAME FRAME-A /* Include Zero Quantity On Hand? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_part C-Win
ON VALUE-CHANGED OF tb_part IN FRAME FRAME-A /* Print Customer Part#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rcpt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rcpt-date C-Win
ON VALUE-CHANGED OF tb_rcpt-date IN FRAME FRAME-A /* Receipt Date? */
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
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort By Part#? */
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

  RUN sys/inc/CustListForm.p ( "IL8",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .


  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IL8',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IL8""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'IL8',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'IL8').


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
  DISPLAY tb_cust-list begin_cust end_cust begin_cust-po end_cust-po begin_slm 
          end_slm lbl_itm-code rd_itm-code lbl_ostat rd_ostat lbl_print 
          rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust tb_rcpt-date tb_part 
          rd-dest lv-ornt td-show-parm lines-per-page lv-font-no lv-font-name 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-8 tb_cust-list btnCustList begin_cust end_cust 
         begin_cust-po end_cust-po begin_slm end_slm rd_itm-code rd_ostat 
         rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust tb_rcpt-date tb_part 
         rd-dest lv-ornt td-show-parm lines-per-page lv-font-no tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
/* ------------------------------------------------ fg/rep/fg-xstat.p 3/94 RM */
/* finished goods inventory status by customer report                         */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-sortby as log format "Y/N" init "N".
def var v-job-no as char format "x(9)".
def var v-ext   as dec format "->>>,>>>,>>9.99".
def var fcst as ch init " ".
def var tcst like fcst init "zzzzzzzzz".
def var fpo# as ch init " ".
def var tpo# like fpo# init "zzzzzzzzz".
def var typex as ch format "!" init "A".
def var fslm like cust.sman init " ".
def var tslm like cust.sman init "zzz".
def var zbal as log format "Y/N".
def var v-rec-dat as log format "Y/N" init no.
def var v-prt-cpn like v-rec-dat init yes.
def var v-qty-onh as dec format "->>>,>>>,>>9".
def var v-frst as log.
def var v-frst-ord as log.
def var v-tot-ord  as dec format "->>>,>>>,>>9".
def var v-tot-ship as dec format "->>,>>>,>>9".
def var v-tot-onh as dec format "->>>,>>>,>>9".
def var v-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-grand-tot-ord  as dec format "->>>,>>>,>>9".
def var v-grand-tot-ship as dec format "->>,>>>,>>9".
def var v-grand-tot-onh as dec format "->>>,>>>,>>9".
def var v-grand-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-custown as log format "Y/N" init "N".
def var v-frst-i-no as log.
def var v-print as log.
def var trans-date like fg-rcpts.trans-date.
def var v-job as char format "x(9)".
def var v-rec-found as log.
def var v-qty-job like v-qty-onh.
def var v-ext-job like v-ext.
def buffer xbin for fg-bin.
def buffer xbin2 for fg-bin.
def var v-qty-ord as int.
def var v-qty-ship as int.
def var v-disp-item as log.
def var v-ocb as char format "x" init "B".
DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.
DEF VAR v-rel-no LIKE oe-rel.rel-no NO-UNDO.
DEF VAR v-sched-qty LIKE oe-rel.tot-qty NO-UNDO.
DEF VAR v-rel-date LIKE oe-rel.rel-date NO-UNDO.
def var v-smry-dtl as char format "x(9)".
DEF VAR v-sales-rep LIKE cust.sman NO-UNDO.

DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

IF rd_smry-dtl = "S" THEN DO:
   form
    cust.cust-no label "CUSTOMER"
    oe-ordl.po-no label "PO #"
/*    cust.sman label "SMAN"*/
    oe-ordl.i-no  label "ITEM #"
    oe-ordl.part-no label "CUST PART #" format "x(15)"
    oe-ordl.i-name label "DESCRIPTION" format "x(15)"

/********    NEED JOB-NUMBER HERE!!!!! *************/
    v-job-no LABEL "JOB #" format "x(9)"

/*    fg-bin.loc label "WHSE"*/
    oe-ordl.qty format "->,>>>,>>9" column-label "QUANTITY! ORDERED"
    li-ship-qty format "->,>>>,>>9" column-label "QUANTITY! SHIPPED"
    v-qty-onh  column-label "QUANTITY! ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" column-label "SELLING! PRICE"
    v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx1 no-box down STREAM-IO width 150.

form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #" FORMAT "x(15)"
    v-sales-rep label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.part-no label "CUST PART #" format "x(15)"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "  JOB"
    v-qty-job  column-label "QUANTITY! ON HAND"
    trans-date column-label "RECEIPT!DATE"
    itemfg.sell-price format ">>>,>>9.99" column-label "SELLING! PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx2 no-box down STREAM-IO width 150.

form
    cust.cust-no label "CUSTOMER"
    oe-ordl.po-no label "PO #"
    v-sales-rep label "SREP"
    oe-ordl.i-no  label "ITEM #"
    oe-ordl.i-name label "DESCRIPTION" format "x(15)"
    fg-bin.loc label "WHSE"
    oe-ordl.qty format "->,>>>,>>9" column-label "QUANTITY! ORDERED"
    li-ship-qty format "->,>>>,>>9" column-label "QUANTITY! SHIPPED"
    v-qty-onh  column-label "QUANTITY! ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" column-label "SELLING! PRICE"
    v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx3 no-box down STREAM-IO width 132.

form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #"
    v-sales-rep label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "  JOB"
    v-qty-job  column-label "QUANTITY! ON HAND"
    trans-date column-label "RECEIPT!DATE"
    itemfg.sell-price format ">>>,>>9.99" column-label "SELLING! PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx4 no-box down STREAM-IO width 132.
END.
ELSE DO: 
   form
       cust.cust-no label "CUSTOMER"
       oe-ordl.po-no label "PO #"
   /*    cust.sman label "SMAN"*/
       oe-ordl.i-no  label "ITEM #"
       oe-ordl.part-no label "CUST PART #" format "x(15)"
       oe-ordl.i-name label "DESCRIPTION" format "x(15)"

   /********    NEED JOB-NUMBER HERE!!!!! *************/
       v-job-no COLUMN-LABEL "JOB#!REL#" format "x(9)"

   /*    fg-bin.loc label "WHSE"*/
       oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED!REL DATE"
       li-ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED!SCHED REL QTY"
       v-qty-onh  column-label "QUANTITY! ON HAND"
       oe-ordl.price format ">>,>>>,>>9.99" column-label "SELLING! PRICE"
       v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
       with frame itemx5 no-box down STREAM-IO width 160.
form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #" FORMAT "x(15)"
    v-sales-rep label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.part-no label "CUST PART #" format "x(15)"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "JOB!REL#"
    oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED!REL DATE"
    li-ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED!SCHED REL QTY"
    v-qty-job  column-label "QTY ON HAND"
    trans-date column-label "RECEIPT DATE"
    itemfg.sell-price format ">>>,>>9.99" column-label "SELLING! PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx6 no-box down STREAM-IO width 190.

form
    cust.cust-no label "CUSTOMER"
    oe-ordl.po-no label "PO #"
    v-sales-rep label "SREP"
    oe-ordl.i-no  label "ITEM #"
    oe-ordl.i-name label "DESCRIPTION" format "x(15)"
    fg-bin.loc COLUMN-LABEL "WHSE!REL#"
    oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED!REL DATE"
    li-ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED!SCHED REL QTY"
    v-qty-onh  column-label "QTY ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" column-label "SELLING! PRICE"
    v-ext format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx7 no-box down STREAM-IO width 150.

form
    cust.cust-no label "CUSTOMER"
    itemfg.cust-po-no label "PO #"
    v-sales-rep label "SREP"
    itemfg.i-no  label "ITEM #"
    itemfg.i-name label "DESCRIPTION" format "x(15)"
    v-job column-label "JOB!REL#"
    oe-ordl.qty format "->,>>>,>>9" column-label "QTY ORDERED!REL DATE"
    li-ship-qty format "->,>>>,>>9" column-label "QTY SHIPPED!SCHED REL QTY"
    v-qty-job  column-label "QTY ON HAND"
    trans-date column-label "RECEIPT DATE"
    itemfg.sell-price format ">>>,>>9.99" column-label "SELLING! PRICE"
    v-ext-job format "->>>,>>>,>>9.99"  column-label "TOTAL!VALUE"
    with frame itemx8 no-box down STREAM-IO width 190.
END.
assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fcst       = begin_cust
 tcst       = end_cust
 fpo#       = begin_cust-po
 tpo#       = end_cust-po
 fslm       = begin_slm
 tslm       = END_slm 
 typex      = SUBSTR(rd_itm-code,1,1)
 v-ocb      = SUBSTR(rd_ostat,1,1)
 v-smry-dtl = substr(rd_smry-dtl,1,1)
 v-sortby   = tb_sort
 zbal       = tb_inc-zero
 v-custown  = tb_inc-cust
 v-rec-dat  = tb_rcpt-date
 v-prt-cpn  = tb_part.

IF typex EQ "B" THEN typex = "A".

FIND FIRST fg-bin NO-LOCK NO-ERROR.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN
   DO:
    excelheader = "".
    IF rd_smry-dtl = "s" THEN do:
    IF v-rec-dat THEN
    DO:
        IF v-prt-cpn THEN  /* frame itemx2 */
          excelheader = "CUSTOMER,PO #,SREP,ITEM #,CUST PART #," + 
                        "DESCRIPTION,JOB,QTY ON HAND,RECEIPT DATE," + 
                        "SELLING PRICE,TOTAL VALUE".
        ELSE              /* frame itemx4 */
          excelheader = "CUSTOMER,PO #,SREP,ITEM #,DESCRIPTION," + 
                        "JOB,QTY ON HAND,RECEIPT DATE," + 
                        "SELLING PRICE,TOTAL VALUE".
    END.
    ELSE
    DO:
        if v-prt-cpn then
          excelheader = "CUSTOMER,PO #,ITEM #,CUST PART #," + 
                        "DESCRIPTION,JOB #,QTY ORDERED,QTY SHIPPED," +
                        "QTY ON HAND,SELLING PRICE,TOTAL VALUE".
        ELSE
           excelheader = "CUSTOMER,PO,SREP,ITEM #,DESCRIPTION," + 
                         "WHSE,QTY ORDERED,QTY SHIPPED,QTY ON HAND," +
                         "SELLING PRICE,TOTAL VALUE".
    END.
    END.
    ELSE DO:
        IF v-rec-dat THEN
    DO:
        IF v-prt-cpn THEN  /* frame itemx2 */
          /*excelheader = "CUSTOMER,PO #,SMAN,ITEM #,CUST PART #," + 
                        "DESCRIPTION,JOB# / REL#,QTY ON HAND / REL DATE,RECEIPT DATE / SCHED REL QTY," + 
                        "SELLING PRICE,TOTAL VALUE".*/
           excelheader = "CUSTOMER,PO #,SREP,ITEM #,CUST PART #," + 
                        "DESCRIPTION,JOB#/REL#,QTY ORDERED/REL DATE,QTY SHIPPED/SCHED REL QTY," + 
                        "QTY ON HAND,RECEIPT DATE,SELLING PRICE,TOTAL VALUE".
        ELSE              /* frame itemx4 */
          /*excelheader = "CUSTOMER,PO #,SMAN,ITEM #,DESCRIPTION," + 
                        "JOB# / REL#,QTY ON HAND / REL DATE,RECEIPT DATE / SCHED REL QTY," + 
                        "SELLING PRICE,TOTAL VALUE".*/
          excelheader = "CUSTOMER,PO #,SREP,ITEM #,DESCRIPTION," + 
                        "JOB#/REL#,QTY ORDERED/REL DATE,QTY SHIPPED/SCHED REL QTY," + 
                        "QTY ON HAND,RECEIPT DATE,SELLING PRICE,TOTAL VALUE".
    END.
    ELSE
    DO:
        if v-prt-cpn then
          excelheader = "CUSTOMER,PO #,ITEM #,CUST PART #," + 
                        "DESCRIPTION,JOB#/REL#,QTY ORDERED/REL DATE,QTY SHIPPED/SCHED REL QTY," +
                        "QTY ON HAND,SELLING PRICE,TOTAL VALUE".
        ELSE
           excelheader = "CUSTOMER,PO,SREP,ITEM #,DESCRIPTION," + 
                         "WHSE/REL#,QTY ORDERED/REL DATE,QTY SHIPPED/SCHED REL QTY,QTY ON HAND," +
                         "SELLING PRICE,TOTAL VALUE".
    END.

    END.


    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

display "" with frame r-top.

{fg/rep/fg-xstat.i}

IF tb_excel THEN 
DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
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

