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

  Author: RTC

  Created: 01/27/2009

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
DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR init-dir  AS CHAR NO-UNDO.
DEF VAR tmp-dir   AS CHAR NO-UNDO.

DEFINE STREAM excel.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-invalid    AS LOG NO-UNDO.
DEF VAR v-download   AS LOG INIT NO NO-UNDO.
DEF VAR v-prior      AS LOG INIT NO NO-UNDO.

DEF BUFFER tmp-per FOR period.

DEF STREAM s-temp.

DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-vend-whse-item LIKE vend-whse-item
   FIELD tt-row-id            AS ROWID
   FIELD row-id               AS ROWID
   FIELD has-rec              AS LOG INIT NO
   FIELD seq-no               AS INT
   FIELD est-no               LIKE est.est-no
   FIELD board                LIKE ef.board
   FIELD total-inventory      AS DECI
   FIELD weekly-usage         AS DECI
   FIELD style                LIKE itemfg.style
   FIELD sell-price           LIKE itemfg.sell-price
   FIELD q-onh                LIKE itemfg.q-onh
   FIELD need-to-produce      AS DECI
   FIELD producing            AS DECI
   FIELD no-of-ups            AS INT
   FIELD needed-sheets        AS DECI
   FIELD board-cost           AS DECI
   INDEX seq-no seq-no.

DEF TEMP-TABLE tt-materials
   FIELD fg-item-no        LIKE itemfg.i-no
   FIELD board             LIKE ef.board
   FIELD sheet-width       LIKE ef.gsh-wid
   FIELD sheet-length      LIKE ef.gsh-len
   FIELD need-to-produce   AS DECI
   FIELD no-ups            AS INT
   FIELD colors            LIKE eb.i-coldscr
   FIELD needed-sheets     AS DECI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TG-print-rq-materials FI-number-of-weeks ~
lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel rd-dest ~
tb_excel tb_runExcel fi_file FI-beg-vend-code FI-beg-vend-plant-code ~
FI-beg-fg-item-no FI-beg-cust-part-no FI-end-vend-code ~
FI-end-vend-plant-code FI-end-fg-item-no FI-end-cust-part-no RECT-6 RECT-7 ~
RECT-19 RECT-20 
&Scoped-Define DISPLAYED-OBJECTS TG-print-rq-materials FI-number-of-weeks ~
lv-ornt lines-per-page lv-font-no td-show-parm lv-font-name rd-dest ~
tb_excel tb_runExcel fi_file FI-beg-vend-code FI-beg-vend-plant-code ~
FI-beg-fg-item-no FI-beg-cust-part-no FI-end-vend-code ~
FI-end-vend-plant-code FI-end-fg-item-no FI-end-cust-part-no 

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

DEFINE VARIABLE FI-beg-cust-part-no AS CHARACTER FORMAT "X(12)":U 
     LABEL "Customers Part No" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-fg-item-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Suppliers FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-plant-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Plant Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-cust-part-no AS CHARACTER FORMAT "X(12)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Customers Part No" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-fg-item-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Suppliers FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-vend-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Vendor Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-vend-plant-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Vendor Plant Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-number-of-weeks AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Weeks to Produce to Stock" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vwrdfl.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.91.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.91.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 10.24.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-print-rq-materials AS LOGICAL INITIAL no 
     LABEL "Print Materials Required?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     TG-print-rq-materials AT ROW 3.24 COL 36 WIDGET-ID 10
     FI-number-of-weeks AT ROW 2.19 COL 34 COLON-ALIGNED WIDGET-ID 6
     lv-ornt AT ROW 12.71 COL 29 NO-LABEL
     lines-per-page AT ROW 12.71 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 14.86 COL 33 COLON-ALIGNED
     td-show-parm AT ROW 14.86 COL 52
     btn-ok AT ROW 20.67 COL 23.2
     btn-cancel AT ROW 20.57 COL 61
     lv-font-name AT ROW 15.81 COL 29 COLON-ALIGNED NO-LABEL
     rd-dest AT ROW 12.91 COL 6 NO-LABEL
     tb_excel AT ROW 17.48 COL 48.6
     tb_runExcel AT ROW 17.48 COL 91.4 RIGHT-ALIGNED
     fi_file AT ROW 18.52 COL 46.4 COLON-ALIGNED HELP
          "Enter File Name"
     FI-beg-vend-code AT ROW 5.57 COL 24 COLON-ALIGNED WIDGET-ID 2
     FI-beg-vend-plant-code AT ROW 6.52 COL 24 COLON-ALIGNED WIDGET-ID 36
     FI-beg-fg-item-no AT ROW 7.48 COL 24 COLON-ALIGNED WIDGET-ID 30
     FI-beg-cust-part-no AT ROW 8.52 COL 24 COLON-ALIGNED WIDGET-ID 34
     FI-end-vend-code AT ROW 5.62 COL 67.8 COLON-ALIGNED WIDGET-ID 14
     FI-end-vend-plant-code AT ROW 6.57 COL 67.8 COLON-ALIGNED WIDGET-ID 16
     FI-end-fg-item-no AT ROW 7.52 COL 67.8 COLON-ALIGNED WIDGET-ID 32
     FI-end-cust-part-no AT ROW 8.57 COL 67.8 COLON-ALIGNED WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Beginning:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.43 COL 7.4 WIDGET-ID 26
     "Ending:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.38 COL 50.4 WIDGET-ID 28
     RECT-6 AT ROW 11.48 COL 2
     RECT-7 AT ROW 1.29 COL 2
     RECT-19 AT ROW 4.76 COL 6.4 WIDGET-ID 22
     RECT-20 AT ROW 4.76 COL 49 WIDGET-ID 24
    WITH 1 DOWN NO-HIDE KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 99.6 BY 23.14.


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
         TITLE              = "Red Flag Report"
         HEIGHT             = 21.95
         WIDTH              = 97.6
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
   FRAME-NAME Custom                                                    */
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
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Red Flag Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Red Flag Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

   FOR EACH tt-vend-whse-item.
      DELETE tt-vend-whse-item.
   END.

   FOR EACH tt-materials.
      DELETE tt-materials.
   END.

   RUN create-tt-vend-whse-item.
   RUN run-report. 



   CASE rd-dest:
      WHEN 1 THEN RUN output-to-printer.
      WHEN 2 THEN RUN output-to-screen.
      WHEN 3 THEN RUN output-to-file.
      WHEN 4 THEN DO:
/*            /*run output-to-fax.*/                                */
/*            {custom/asifax.i &type="Machine Transaction"          */
/*                             &begin_cust=begin_machine            */
/*                             &END_cust= begin_machine             */
/*                             &fax-subject="Machine Transaction"   */
/*                             &fax-body="Machine Transaction"      */
/*                             &fax-file=list-name }                */
/*        END.                                                      */
/*        when 5 then do:                                           */
/*            IF is-xprint-form THEN DO:                            */
/*               {custom/asimail.i &TYPE = "Machine Transaction"    */
/*                              &begin_cust= begin_machine          */
/*                              &END_cust=begin_machine             */
/*                              &mail-subject="Machine Transaction" */
/*                              &mail-body="Machine Transaction "   */
/*                              &mail-file=list-name }              */
/*            END.                                                  */
/*            ELSE DO:                                              */
/*                {custom/asimailr.i &TYPE = "Machine Transaction"  */
/*                              &begin_cust= begin_machine          */
/*                              &END_cust=begin_machine             */
/*                              &mail-subject=current-window:title  */
/*                              &mail-body=CURRENT-WINDOW:TITLE     */
/*                              &mail-file=list-name }              */

/*      END. */
      END.
      WHEN 6 THEN RUN OUTPUT-to-port.

   END CASE. 
   SESSION:SET-WAIT-STATE("").
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-cust-part-no C-Win
ON LEAVE OF FI-beg-cust-part-no IN FRAME FRAME-A /* Customers Part No */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-fg-item-no C-Win
ON LEAVE OF FI-beg-fg-item-no IN FRAME FRAME-A /* Suppliers FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-vend-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-vend-code C-Win
ON LEAVE OF FI-beg-vend-code IN FRAME FRAME-A /* Vendor Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-vend-plant-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-vend-plant-code C-Win
ON LEAVE OF FI-beg-vend-plant-code IN FRAME FRAME-A /* Vendor Plant Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-cust-part-no C-Win
ON LEAVE OF FI-end-cust-part-no IN FRAME FRAME-A /* Customers Part No */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-fg-item-no C-Win
ON LEAVE OF FI-end-fg-item-no IN FRAME FRAME-A /* Suppliers FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-vend-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-vend-code C-Win
ON LEAVE OF FI-end-vend-code IN FRAME FRAME-A /* Vendor Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-vend-plant-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-vend-plant-code C-Win
ON LEAVE OF FI-end-vend-plant-code IN FRAME FRAME-A /* Vendor Plant Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-number-of-weeks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-number-of-weeks C-Win
ON LEAVE OF FI-number-of-weeks IN FRAME FRAME-A /* Weeks to Produce to Stock */
DO:
   ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-print-rq-materials
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-print-rq-materials C-Win
ON VALUE-CHANGED OF TG-print-rq-materials IN FRAME FRAME-A /* Print Materials Required? */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/*{sys/inc/f3helpw.i} */
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
     RETURN.
  END.

  RUN init-proc.
  RUN enable_UI.
  {methods/nowait.i}
  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}
    APPLY 'ENTRY' TO FI-number-of-weeks.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-vend-whse-item C-Win 
PROCEDURE create-tt-vend-whse-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-total-inventory  AS DECI NO-UNDO.
DEF VAR v-weekly-usage     AS DECI NO-UNDO.
DEF VAR v-number-of-weeks  AS DECI NO-UNDO.
DEF VAR v-weeks-in-inv     AS DECI NO-UNDO.
DEF VAR v-producing        AS DECI NO-UNDO.
DEF VAR v-board-cost       AS DECI NO-UNDO.

DEF BUFFER b-itemfg  FOR itemfg.
DEF BUFFER b-eb      FOR eb.
DEF BUFFER b-est     FOR est.
DEF BUFFER b-ef      FOR ef.
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER b-job-hdr FOR job-hdr.

FOR EACH vend-whse-item WHERE vend-whse-item.vendor-code >= FI-beg-vend-code
                          AND vend-whse-item.vendor-code <= FI-end-vend-code
                          AND vend-whse-item.vendor-plant-code >= FI-beg-vend-plant-code
                          AND vend-whse-item.vendor-plant-code <= FI-end-vend-plant-code
                          AND vend-whse-item.fg-item-no >= FI-beg-fg-item-no
                          AND vend-whse-item.fg-item-no <= FI-end-fg-item-no
                          AND vend-whse-item.cust-part-no >= FI-beg-cust-part-no
                          AND vend-whse-item.cust-part-no <= FI-end-cust-part-no
                          AND vend-whse-item.obsolete-date = ?
                     BREAK BY vend-whse-item.vendor-code                                            
                           BY vend-whse-item.vendor-plant-code:
   ASSIGN
      v-weekly-usage    = 0
      v-total-inventory = 0
      v-producing       = 0
      v-number-of-weeks = 0
      v-board-cost      = 0.

   FIND FIRST b-itemfg WHERE b-itemfg.company = vend-whse-item.company
                         AND b-itemfg.i-no    = vend-whse-item.fg-item-no
                         AND b-itemfg.part-no = vend-whse-item.cust-part-no NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(b-itemfg) THEN 
      FIND FIRST b-itemfg WHERE b-itemfg.company = vend-whse-item.company
                            AND b-itemfg.i-no    = vend-whse-item.fg-item-no NO-LOCK NO-ERROR.   

   ASSIGN
      v-total-inventory = b-itemfg.q-onh + vend-whse-item.plant-tot-oh-qty
      v-weekly-usage    = vend-whse-item.est-annual-usage / 52
      v-number-of-weeks = ROUND(v-total-inventory / v-weekly-usage, 2).

   IF v-number-of-weeks < FI-number-of-weeks THEN DO:  
      FIND FIRST b-est WHERE b-est.company      = b-itemfg.company
                         AND TRIM(b-est.est-no) = TRIM(b-itemfg.est-no) NO-LOCK NO-ERROR.
      FIND FIRST b-ef WHERE b-ef.company      = b-itemfg.company
                        AND TRIM(b-ef.est-no) = TRIM(b-itemfg.est-no) NO-LOCK NO-ERROR.
      IF AVAILABLE(b-ef) THEN DO:
         FIND FIRST b-eb WHERE b-eb.company      = b-itemfg.company
                           AND TRIM(b-eb.est-no) = TRIM(b-itemfg.est-no)
                           AND b-eb.form-no      = b-ef.form-no 
                           AND b-eb.part-no      = b-itemfg.part-no NO-LOCK NO-ERROR.

         IF b-ef.cost-msh > 0 THEN DO:
            v-board-cost = b-ef.cost-msh.   
         END.
         ELSE DO:
            FIND FIRST b-job-hdr WHERE b-job-hdr.company    = b-eb.company
                                   AND b-job-hdr.est-no     = b-eb.est-no
                                   AND b-job-hdr.frm        = b-eb.form-no
                                   AND b-job-hdr.blank-no   = b-eb.blank-no
                                   AND b-job-hdr.i-no       = vend-whse-item.fg-item-no NO-LOCK NO-ERROR.

            FIND FIRST b-job-mat WHERE b-job-mat.company  EQ b-eb.company
                                   AND b-job-mat.job      EQ b-job-hdr.job
                                   AND b-job-mat.job-no   EQ b-job-hdr.job-no
                                   AND b-job-mat.job-no2  EQ b-job-hdr.job-no2
                                   AND b-job-mat.frm      EQ b-eb.form-no
                                   AND b-job-mat.blank-no EQ b-eb.blank-no
                                   AND b-job-mat.i-no     EQ b-ef.board
                                   AND b-job-mat.rm-i-no  EQ b-ef.board   
                                    USE-INDEX i-no NO-LOCK NO-ERROR.

            v-board-cost = b-job-mat.cost.
         END.

         IF AVAILABLE(b-eb) THEN DO:
/*             RUN custitem/d-vwprod.w(INPUT ROWID(b-itemfg), */
/*                                     OUTPUT v-producing).   */

            CREATE tt-vend-whse-item.
            BUFFER-COPY vend-whse-item TO tt-vend-whse-item
            ASSIGN
               tt-vend-whse-item.row-id            = ROWID(vend-whse-item)
               tt-vend-whse-item.has-rec           = YES
               tt-vend-whse-item.seq-no            = 1
               tt-vend-whse-item.est-no            = b-itemfg.est-no
               tt-vend-whse-item.total-inventory   = v-total-inventory
               tt-vend-whse-item.weekly-usage      = v-weekly-usage
               tt-vend-whse-item.style             = b-itemfg.style
               tt-vend-whse-item.producing         = v-producing
               tt-vend-whse-item.sell-price        = b-itemfg.sell-price
               tt-vend-whse-item.q-onh             = b-itemfg.q-onh
               tt-vend-whse-item.need-to-produce   = (FI-number-of-weeks * v-weekly-usage) - v-total-inventory
               tt-vend-whse-item.board             = b-ef.board
               tt-vend-whse-item.caliper           = STRING(b-ef.cal, ".999")
               tt-vend-whse-item.no-of-ups         = b-eb.num-up
               tt-vend-whse-item.needed-sheets     = tt-vend-whse-item.need-to-produce / b-eb.num-up
               tt-vend-whse-item.board-cost        = v-board-cost.
         END.

         IF TG-print-rq-materials = YES THEN DO:
            CREATE tt-materials.
            ASSIGN
               tt-materials.fg-item-no      = vend-whse-item.fg-item-no
               tt-materials.board           = b-ef.board
               tt-materials.sheet-width     = b-ef.gsh-wid
               tt-materials.sheet-length    = b-ef.gsh-len
               tt-materials.need-to-produce = (FI-number-of-weeks * v-weekly-usage) - v-total-inventory               
               tt-materials.no-ups          = b-eb.num-up
               tt-materials.colors          = b-eb.i-coldscr
               tt-materials.needed-sheets   = tt-materials.need-to-produce / b-eb.num-up.          
         END.
      END.
   END.
END. 

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
  DISPLAY TG-print-rq-materials FI-number-of-weeks lv-ornt lines-per-page 
          lv-font-no td-show-parm lv-font-name rd-dest tb_excel tb_runExcel 
          fi_file FI-beg-vend-code FI-beg-vend-plant-code FI-beg-fg-item-no 
          FI-beg-cust-part-no FI-end-vend-code FI-end-vend-plant-code 
          FI-end-fg-item-no FI-end-cust-part-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE TG-print-rq-materials FI-number-of-weeks lv-ornt lines-per-page 
         lv-font-no td-show-parm btn-ok btn-cancel rd-dest tb_excel tb_runExcel 
         fi_file FI-beg-vend-code FI-beg-vend-plant-code FI-beg-fg-item-no 
         FI-beg-cust-part-no FI-end-vend-code FI-end-vend-plant-code 
         FI-end-fg-item-no FI-end-cust-part-no RECT-6 RECT-7 RECT-19 RECT-20 
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

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/
/*
  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */                                  
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*==== Report main body procedure ================================
==================================================================*/
   DEF VAR v-head      AS CHAR FORMAT "x(280)" EXTENT 4 NO-UNDO.
   DEF VAR v-excelheader      AS CHAR NO-UNDO.

   {sys/inc/print1.i}

   {sys/inc/outprint.i VALUE(lines-per-page)}

   IF td-show-parm THEN RUN show-param.
      SESSION:SET-WAIT-STATE("general").

   {sys/form/r-top3w.f}

   FORMAT HEADER
      v-head[1] SKIP
      v-head[2] SKIP
      v-head[3] SKIP
      v-head[4]
   WITH FRAME f-top WIDTH 285.

   v-head[1] = "".

   ASSIGN                                                                                                                              
      v-head[2] = "                                  EST ANNUAL    ONHAND      PLANT      PRODUCTION            NEEDED         BOARD"
      v-head[3] = "FG ITEM NO      BOARD       CAL     USAGE      QUANTITY   ONHAND QTY    QUANTITY    #UPS     SHEETS          COST"
      v-head[4] = FILL("-",113).

   DISPLAY "" WITH FRAME r-top.
   DISPLAY "" WITH FRAME f-top.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).
      ASSIGN v-excelheader = "FG ITEM NO,BOARD,CALIPER,EST ANNUAL USAGE,ONHAND QTY,PLANT ONHAND QTY,PRODUCTION QTY,#UPS,NEEDED SHEETS,BOARD COST".
      PUT STREAM excel UNFORMATTED v-excelheader SKIP.
   END.  

   FOR EACH tt-vend-whse-item:
      DISPLAY
         tt-vend-whse-item.fg-item-no        FORMAT "X(15)"
         tt-vend-whse-item.board             FORMAT "X(10)"
         tt-vend-whse-item.caliper           FORMAT "X(4)"
         SPACE(3)
         tt-vend-whse-item.est-annual-usage  FORMAT "->,>>>,>>9"
         SPACE(1) 
         tt-vend-whse-item.q-onh             FORMAT "->,>>>,>>9"
         SPACE(3)
         tt-vend-whse-item.plant-tot-oh-qty  FORMAT "->,>>>,>>9"
         SPACE(3)
         tt-vend-whse-item.need-to-produce   FORMAT "->,>>>,>>9"
         SPACE(4)
         tt-vend-whse-item.no-of-ups         FORMAT ">>9"
         tt-vend-whse-item.needed-sheets     FORMAT "->,>>>,>>9"
         tt-vend-whse-item.board-cost        FORMAT "->,>>>,>>9.99"
         WITH FRAME a NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            '"' REPLACE(tt-vend-whse-item.fg-item-no,'"', "")  '",'
            '"' tt-vend-whse-item.board                        '",'
            '"' tt-vend-whse-item.caliper                      '",'
            '"' tt-vend-whse-item.est-annual-usage             '",'
            '"' tt-vend-whse-item.q-onh                        '",'
            '"' tt-vend-whse-item.plant-tot-oh-qty             '",'
            '"' tt-vend-whse-item.need-to-produce              '",'
            '"' tt-vend-whse-item.no-of-ups                    '",'
            '"' tt-vend-whse-item.needed-sheets                '",'
            '"' tt-vend-whse-item.board-cost                   '",'
            SKIP.
   END.

   IF TG-print-rq-materials = YES THEN DO:
      HIDE FRAME f-top.
      HIDE FRAME a.

      ASSIGN                                                                                                                              
         v-head[2] = "                              SHEET      SHEET  PRODUCTION               NEEDED     COLOR"
         v-head[3] = "FG ITEM NO      BOARD         WIDTH     LENGTH    QUANTITY    #UPS       SHEETS     DESCRIPTION"
         v-head[4] = FILL("-",113).

      PAGE.

      FORMAT HEADER
         v-head[1] SKIP
         v-head[2] SKIP
         v-head[3] SKIP
         v-head[4]
      WITH FRAME f-top2 WIDTH 285.

      DISPLAY "" WITH FRAME f-top2.

      IF tb_excel THEN DO:
         ASSIGN v-excelheader = "FG ITEM NO,BOARD,SHEET WIDTH,SHEET LENGTH,PRODUCTION QTY,#UPS,NEEDED SHEETS,COLOR DESCRIPTION".
         PUT STREAM excel UNFORMATTED v-excelheader SKIP.
      END.

      FOR EACH tt-materials:
         DISPLAY
            tt-materials.fg-item-no       FORMAT "X(15)"
            tt-materials.board            FORMAT "X(10)"
            tt-materials.sheet-width
            SPACE(3)
            tt-materials.sheet-length
            SPACE(2)
            tt-materials.need-to-produce  FORMAT "->,>>>,>>9"
            SPACE(5)
            tt-materials.no-ups           FORMAT ">>9"
            SPACE(3)
            tt-materials.needed-sheets    FORMAT "->,>>>,>>9"
            SPACE(4)
            tt-materials.colors
            WITH FRAME b NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            '"' REPLACE(tt-materials.fg-item-no,'"', "")       '",'
            '"' tt-materials.board                             '",'
            '"' tt-materials.sheet-width                       '",'
            '"' tt-materials.sheet-length                      '",'
            '"' tt-materials.need-to-produce                   '",'
            '"' tt-materials.no-ups                            '",'
            '"' tt-materials.needed-sheets                     '",'
            '"' tt-materials.colors                            '",'
            SKIP.

      END.
   END.

   OUTPUT CLOSE.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
   END.  

   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

