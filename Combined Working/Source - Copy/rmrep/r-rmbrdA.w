&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep/r-rmbrd.w

  Description: Job Board/Printer Report

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
DEF VAR llWasFound AS LOG NO-UNDO.
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

DEF STREAM excel.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR v-mat-list AS CHAR NO-UNDO.

{sys/form/r-topw.f}

DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-po NO-UNDO
   FIELD i-no AS CHAR
   FIELD po-line AS CHAR
   FIELD COUNT AS INT
   FIELD po-no AS INT
   INDEX i-no i-no po-no.

DEFINE TEMP-TABLE tt-job NO-UNDO
   FIELD i-no AS CHAR
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD seq AS INT
   FIELD resource AS CHAR
   FIELD print-date AS DATE FORMAT "99/99/9999"
   FIELD start-date-su AS DATE
   FIELD start-time-su AS INT
   FIELD alloc-qty AS DEC FORMAT ">>>,>>>,>>9.99"
   FIELD m-code AS CHAR
   INDEX i-no i-no print-date seq m-code
   INDEX m-code m-code.

DEFINE TEMP-TABLE tt-mach NO-UNDO
    FIELD m-code AS CHAR
    INDEX m-code m-code.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date begin_date end_date ~
begin_rm-no end_rm-no begin_procat end_procat begin_mach end_mach rd_qty ~
rd_item select-mat tb_zero-bal tb_qty-ord tb_neg tb_allocated rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date begin_date end_date begin_rm-no ~
end_rm-no begin_procat end_procat begin_mach end_mach lbl_grnd-tot rd_qty ~
lbl_itm-code rd_item select-mat tb_zero-bal tb_qty-ord tb_neg tb_allocated ~
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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "Inventory as of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "Beginning Run Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning  Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/25 
     LABEL "Ending Run Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-rmbrd.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_grnd-tot AS CHARACTER FORMAT "X(256)":U INITIAL "Calculate Value Using On Hand or Available?" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_itm-code AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "8" 
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

DEFINE VARIABLE rd_item AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimated", "Estimated",
"Real", "Real",
"Both", "Both"
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Available" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Available", "Available",
"On Hand", "Oh Hand"
     SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 13.57.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 5 NO-UNDO.

DEFINE VARIABLE tb_allocated AS LOGICAL INITIAL no 
     LABEL "Allocated Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_neg AS LOGICAL INITIAL no 
     LABEL "Negative Balances Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE tb_qty-ord AS LOGICAL INITIAL yes 
     LABEL "Inc Qty On Order with Qty Avail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_zero-bal AS LOGICAL INITIAL no 
     LABEL "Include Zero Balances?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     as-of-date AT ROW 1.29 COL 64.4 COLON-ALIGNED WIDGET-ID 2
     begin_date AT ROW 2.33 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Run-Start-Date" WIDGET-ID 34
     end_date AT ROW 2.33 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Run-Start-Date" WIDGET-ID 36
     begin_rm-no AT ROW 3.38 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 3.38 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_procat AT ROW 4.43 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 4
     end_procat AT ROW 4.43 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Category" WIDGET-ID 6
     begin_mach AT ROW 5.48 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Machine" WIDGET-ID 30
     end_mach AT ROW 5.48 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Machine" WIDGET-ID 32
     lbl_grnd-tot AT ROW 6.95 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     rd_qty AT ROW 6.95 COL 61 NO-LABEL WIDGET-ID 16
     lbl_itm-code AT ROW 8.14 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rd_item AT ROW 8.14 COL 61 NO-LABEL WIDGET-ID 10
     select-mat AT ROW 8.86 COL 3 NO-LABEL WIDGET-ID 28
     tb_zero-bal AT ROW 10.05 COL 54 WIDGET-ID 24
     tb_qty-ord AT ROW 11 COL 54 WIDGET-ID 22
     tb_neg AT ROW 11.95 COL 54 WIDGET-ID 20
     tb_allocated AT ROW 12.95 COL 54 WIDGET-ID 38
     rd-dest AT ROW 15.95 COL 6 NO-LABEL
     lv-ornt AT ROW 16.14 COL 31 NO-LABEL
     lines-per-page AT ROW 16.14 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 18.29 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 19.24 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.29 COL 31
     tb_excel AT ROW 21.62 COL 68 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.62 COL 89 RIGHT-ALIGNED
     fi_file AT ROW 22.43 COL 46 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.05 COL 18
     btn-cancel AT ROW 24.05 COL 57
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 8.14 COL 3 WIDGET-ID 26
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.05 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 14.81 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.4 BY 24.57.


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
         TITLE              = "Job Material/Machine Report"
         HEIGHT             = 24.52
         WIDTH              = 96.2
         MAX-HEIGHT         = 53.71
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.71
         VIRTUAL-WIDTH      = 384
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_grnd-tot IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_grnd-tot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

/* SETTINGS FOR FILL-IN lbl_itm-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_itm-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_item".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_item:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_allocated:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_neg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_qty-ord:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Job Material/Machine Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Material/Machine Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* Inventory as of */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Run Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
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
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning RM Item# */
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

 SESSION:SET-WAIT-STATE("general").
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").


  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= 'Board List'
                            &begin_cust= begin_rm-no
                            &END_cust=begin_rm-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:

            {custom/asimailr.i &TYPE = "Board List"
                               &begin_cust= begin_rm-no
                               &END_cust=begin_rm-no
                               &mail-subject=c-win:title
                               &mail-body=c-win:title
                               &mail-file=list-name }

       END.
       WHEN 6 THEN RUN output-to-port.

  end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Run Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
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
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending RM Item# */
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


&Scoped-define SELF-NAME rd_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_item C-Win
ON VALUE-CHANGED OF rd_item IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_allocated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_allocated C-Win
ON VALUE-CHANGED OF tb_allocated IN FRAME FRAME-A /* Allocated Only? */
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


&Scoped-define SELF-NAME tb_neg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_neg C-Win
ON VALUE-CHANGED OF tb_neg IN FRAME FRAME-A /* Negative Balances Only? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qty-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-ord C-Win
ON VALUE-CHANGED OF tb_qty-ord IN FRAME FRAME-A /* Inc Qty On Order with Qty Avail? */
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
ON VALUE-CHANGED OF tb_zero-bal IN FRAME FRAME-A /* Include Zero Balances? */
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

  as-of-date = TODAY.

  RUN enable_UI.

  for each mat:
      v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
     substr(v-mat-list,length(trim(v-mat-list)),1) = "".

  select-mat:list-items = v-mat-list.

  {methods/nowait.i}

  find first rm-ctrl where rm-ctrl.company = cocode NO-LOCK NO-ERROR.

  v-avgcost = not avail rm-ctrl or rm-ctrl.avg-lst-cst.

  DO WITH FRAME {&FRAME-NAME}:
     {custom/usrprint.i}
     as-of-date:SCREEN-VALUE = STRING(TODAY).
     APPLY "entry" TO as-of-date.
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
  DISPLAY as-of-date begin_date end_date begin_rm-no end_rm-no begin_procat 
          end_procat begin_mach end_mach lbl_grnd-tot rd_qty lbl_itm-code 
          rd_item select-mat tb_zero-bal tb_qty-ord tb_neg tb_allocated rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 as-of-date begin_date end_date begin_rm-no end_rm-no 
         begin_procat end_procat begin_mach end_mach rd_qty rd_item select-mat 
         tb_zero-bal tb_qty-ord tb_neg tb_allocated rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
    {custom\out2file.i}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-mkbin C-Win 
PROCEDURE rm-mkbin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-dec AS DEC NO-UNDO.

DEF VAR v-r-qty AS   DEC    NO-UNDO.
DEF VAR v-i-qty AS   DEC    NO-UNDO.
DEF VAR v-t-qty AS   DEC    NO-UNDO.
DEF VAR ld-qty  AS   DEC    NO-UNDO.
DEF VAR ld-cst  AS   DEC    NO-UNDO.
DEF VAR lv-uom  AS   CHAR   NO-UNDO.


FOR EACH tt-rm-bin:
  DELETE tt-rm-bin.
END.

FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL item THEN DO:
  {rm/rmmkbin1.i as-of-date tt-}
END.

FOR EACH tt-rm-bin:
  op-dec = op-dec + tt-rm-bin.qty.
  DELETE tt-rm-bin.
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
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-po-line AS CHAR NO-UNDO.
DEF VAR v-value AS DEC DECIMALS 4 NO-UNDO.
DEF VAR rm-cst-amt AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-alloc-total AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR li-seq AS INT NO-UNDO.
DEF VAR lv-q-onh AS DEC NO-UNDO.
def var v-qty as dec format "->>>>9.99" NO-UNDO.
def var v-av  as log format "Avail/OnHand" init YES NO-UNDO.
def var v-type as char format "!" init "B".
DEF VAR v-today-365 AS DATE NO-UNDO.
DEF VAR v-mtype AS CHAR format "x(47)" NO-UNDO.
def var v-len like po-ordl.s-len no-undo.
def var v-wid like po-ordl.s-len no-undo.
def var v-dep like po-ordl.s-len no-undo. 
def var v-bwt like po-ordl.s-len no-undo.
DEF VAR v-mat-act-qty LIKE mat-act.qty NO-UNDO.
DEF VAR noDate AS LOGICAL NO-UNDO. /* rstark 08111413 */

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Whse,Item,Description,Product Category,UOM,Cost,On Hand,On Order, PO - Due Date,Quantity Available,Value".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

EMPTY TEMP-TABLE tt-po.
EMPTY TEMP-TABLE tt-job.
EMPTY TEMP-TABLE tt-mach.

form
    item.loc COLUMN-LABEL "Whse"
    item.i-no COLUMN-LABEL "Item"
    item.i-name    format "x(27)" COLUMN-LABEL "Description"
    procat.dscr FORMAT "X(15)" COLUMN-LABEL "Product Category"
    ITEM.cons-uom COLUMN-LABEL "UOM"
    rm-cst-amt FORMAT ">>>,>>9.9999" COLUMN-LABEL "Cost"
    lv-q-onh FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "On Hand"
    ITEM.q-ono FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "On Order"
    v-po-line FORMAT "X(100)" COLUMN-LABEL "PO - Due Date"
    ITEM.q-avail FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "Quantity Available"
    v-value FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Value"
    with frame itemx no-box down stream-io width 255.

SESSION:SET-WAIT-STATE ("general").

DO WITH FRAME {&FRAME-NAME}:

   do i = 1 to select-mat:num-items:
      if select-mat:is-selected(i) then
         v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
   end.

   IF LENGTH(TRIM(v-mtype)) EQ 0 THEN
   DO:
      MESSAGE "No Material Type Selected."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      LEAVE.
   END.

   if substr(v-mtype,length(trim(v-mtype)),1) eq "," then
      substr(v-mtype,length(trim(v-mtype)),1) = "".
end.

assign
 /* rstark 08111413 */
 noDate = CAN-FIND(FIRST sys-ctrl
                   WHERE sys-ctrl.company EQ cocode
                     AND sys-ctrl.name EQ 'Schedule'
                     AND sys-ctrl.log-fld EQ YES)
 str-tit2 = c-win:TITLE
 v-av     = rd_qty begins "Av"
 v-type   = substr(rd_item,1,1)
 v-today-365 = TODAY - 365
 {sys/inc/ctrtext.i str-tit2 112}.

    display "" with frame r-top.

    FOR EACH job FIELDS(job job-no job-no2 est-no) WHERE
        job.company EQ cocode AND
        job.opened EQ YES
        NO-LOCK:

        {custom/statusMsg.i "'Processing... '"} 

        FOR EACH job-mat WHERE
            job-mat.company EQ cocode AND
            job-mat.job EQ job.job AND
            job-mat.all-flg EQ YES AND
            job-mat.i-no >= begin_rm-no AND
            job-mat.i-no <= end_rm-no
            NO-LOCK,
            FIRST ITEM WHERE
                  item.company = cocode and
                  ITEM.i-no EQ job-mat.i-no AND
                  lookup(item.mat-type,v-mtype) gt 0 AND
                  ITEM.procat GE begin_procat AND
                  ITEM.procat LE end_procat
                  NO-LOCK:

            {custom/statusMsg.i "'Processing... '"} 

            IF NOT (item.i-code eq v-type or v-type eq "B") THEN NEXT.

            CREATE tt-job.
            ASSIGN tt-job.i-no   = job-mat.i-no
                   tt-job.job-no = job-mat.job-no 
                   tt-job.job-no2 = job-mat.job-no2
                   tt-job.alloc-qty = job-mat.qty-all.

            IF ITEM.cons-uom NE job-mat.qty-uom THEN
            DO:
               ASSIGN
                  v-bwt = job-mat.basis-w
                  v-len = job-mat.len
                  v-wid = job-mat.wid
                  v-dep = item.s-dep.

               if v-len eq 0 then v-len = item.s-len.
               if v-wid eq 0 then v-wid = IF item.r-wid ne 0 then item.r-wid
                                          else ITEM.s-wid.
               if v-bwt eq 0 then v-bwt = item.basis-w. 

               run custom/convquom.p (INPUT cocode,
                                      INPUT job-mat.qty-uom,
                                      INPUT ITEM.cons-uom,
                                      INPUT v-bwt,
                                      INPUT v-len,
                                      input v-wid,
                                      input v-dep,
                                      input tt-job.alloc-qty,
                                      output tt-job.alloc-qty).
            END.

            FOR EACH mat-act FIELDS(qty qty-uom) WHERE
                mat-act.company EQ job-mat.company AND
                mat-act.job     EQ job-mat.job AND
                mat-act.job-no  EQ job-mat.job-no AND
                mat-act.job-no2 EQ job-mat.job-no2 AND
                mat-act.rm-i-no EQ job-mat.rm-i-no
                NO-LOCK:

                v-mat-act-qty = mat-act.qty.

                IF ITEM.cons-uom NE job-mat.qty-uom THEN
                DO:
                   ASSIGN
                      v-bwt = job-mat.basis-w
                      v-len = job-mat.len
                      v-wid = job-mat.wid
                      v-dep = item.s-dep.

                   if v-len eq 0 then v-len = item.s-len.
                   if v-wid eq 0 then v-wid = IF item.r-wid ne 0 then item.r-wid
                                              else ITEM.s-wid.
                   if v-bwt eq 0 then v-bwt = item.basis-w. 

                   run custom/convquom.p (INPUT cocode,
                                          INPUT job-mat.qty-uom,
                                          INPUT ITEM.cons-uom,
                                          INPUT v-bwt,
                                          INPUT v-len,
                                          input v-wid,
                                          input v-dep,
                                          input mat-act.qty,
                                          output v-mat-act-qty).
                END.

                /*tt-job.alloc-qty = tt-job.alloc-qty - v-mat-act-qty.*/ /* task 120314103*/
            END.

            RELEASE job-mch.

            FOR EACH job-mch WHERE
                job-mch.company EQ cocode AND
                job-mch.job EQ job.job AND
/*                 job-mch.frm EQ job-mat.frm AND */
                job-mch.run-complete EQ NO AND 
                job-mch.m-code GE begin_mach AND
                job-mch.m-code LE end_mach AND 
              ((job-mch.start-date GE begin_date AND
                job-mch.start-date LE end_date) OR noDate)                
/*                 NO-LOCK,                            */
/*                 FIRST mach WHERE                    */
/*                       mach.company EQ cocode AND    */
/*                       mach.loc EQ locode AND        */
/*                       mach.m-code EQ job-mch.m-code */
                      NO-LOCK:

/*                 IF NOT(mach.dept[1] EQ "PR" OR      */
/*                    mach.dept[2] EQ "PR" OR          */
/*                    mach.dept[3] EQ "PR" OR          */
/*                    mach.dept[4] EQ "PR") THEN NEXT. */
                IF NOT(job-mch.blank-no EQ 0 OR job-mat.blank-no EQ 0 OR
                   job-mch.blank-no EQ job-mat.blank-no) THEN
                   NEXT.

                ASSIGN
                   tt-job.m-code   = job-mch.m-code
                   tt-job.print-date = job-mch.start-date
                   tt-job.start-date-su = job-mch.start-date-su
                   tt-job.start-time-su = job-mch.start-time-su.

                IF NOT CAN-FIND(FIRST tt-mach WHERE
                   tt-mach.m-code EQ job-mch.m-code) THEN
                DO:
                    CREATE tt-mach.
                    ASSIGN tt-mach.m-code = job-mch.m-code.
                    RELEASE tt-mach.
                END.

                LEAVE.
            END.

            RELEASE tt-job.
        END. /*each job-mat*/
    END. /*each job*/

    FOR EACH tt-mach:

        {custom/statusMsg.i "'Processing... '"} 

        li-seq = 0.

        FOR EACH job-mch WHERE
            job-mch.company EQ cocode AND
            job-mch.m-code EQ tt-mach.m-code AND
            job-mch.run-complete EQ NO AND
          ((job-mch.start-date-su NE ? AND
            job-mch.start-time-su NE ? AND
            job-mch.start-date-su GE v-today-365) OR noDate)
            NO-LOCK,
            FIRST job WHERE
                  job.company EQ cocode AND
                  job.job-no EQ job-mch.job-no AND
                  job.job-no2 EQ job-mch.job-no2 AND
                  job.opened EQ YES
                  NO-LOCK
            BREAK BY job-mch.start-date-su
                  BY job-mch.start-time-su:

            llWasFound = NO.
            FOR EACH tt-job WHERE
                 tt-job.job-no EQ job-mch.job-no AND
                 tt-job.job-no2 EQ job-mch.job-no2 AND
                 tt-job.m-code EQ job-mch.m-code AND
                 tt-job.seq EQ 0:
                 llWasFound = YES.
            END.

            IF llWasFound THEN DO:
              /* Only increment if tt-job's exists to update */
              li-seq = li-seq + 1.

              FOR EACH tt-job WHERE
                   tt-job.job-no EQ job-mch.job-no AND
                   tt-job.job-no2 EQ job-mch.job-no2 AND
                   tt-job.m-code EQ job-mch.m-code AND
                   tt-job.seq EQ 0
                   BY tt-job.start-date-su
                   BY tt-job.start-time-su:

                 ASSIGN
                    tt-job.seq = li-seq
                    tt-job.resource = "#" + STRING(tt-job.seq) + " "
                                    + tt-job.m-code.
              END.
            END.
/*             FIND FIRST tt-job WHERE                                */
/*                  tt-job.job-no EQ job-mch.job-no AND               */
/*                  tt-job.job-no2 EQ job-mch.job-no2 AND             */
/*                  tt-job.m-code EQ job-mch.m-code AND               */
/*                  tt-job.seq EQ 0                                   */
/*                  NO-ERROR.                                         */
/*                                                                    */
/*             IF AVAIL tt-job THEN                                   */
/*                ASSIGN                                              */
/*                   tt-job.seq = li-seq                              */
/*                   tt-job.resource = "#" + STRING(tt-job.seq) + " " */
/*                                   + tt-job.m-code.                 */
        END.

    END.

    for each ITEM WHERE
        item.company = cocode and
        ITEM.i-no >= begin_rm-no AND
        ITEM.i-no <= end_rm-no AND
        ITEM.procat >= begin_procat AND
        ITEM.procat <= end_procat AND
        lookup(item.mat-type,v-mtype) gt 0
        NO-LOCK:

        {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"} 

        IF NOT(item.i-code  eq v-type or v-type eq "B") THEN NEXT.

        FOR EACH po-ordl FIELDS(po-no ord-qty due-date) WHERE
            po-ordl.company EQ cocode AND
            po-ordl.i-no EQ item.i-no AND
            po-ordl.opened EQ YES AND
            po-ordl.item-type
            NO-LOCK:

            {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"} 

            FIND FIRST tt-po WHERE
                 tt-po.i-no EQ ITEM.i-no AND
                 tt-po.COUNT LT 3
                 NO-ERROR.

            IF NOT AVAIL tt-po THEN
            DO:
               CREATE tt-po.
               ASSIGN tt-po.i-no = ITEM.i-no
                      tt-po.po-no = po-ordl.po-no.
            END.

            ASSIGN
               tt-po.COUNT = tt-po.COUNT + 1
               tt-po.po-line = tt-po.po-line
                             + (IF tt-po.COUNT NE 1 THEN " " ELSE "")
                             + STRING(po-ordl.po-no)
                             + " - "
                             + STRING(po-ordl.ord-qty)
                             + " due "
                             + STRING(po-ordl.due-date,"99/99/99")
                             + ";".
            RELEASE tt-po.
        END.

        IF item.i-code EQ "E" THEN
           ASSIGN
              v-qty = 0
              rm-cst-amt = 0
              lv-q-onh = 0.
        ELSE DO:

           IF rm-ctrl.avg-lst-cst = TRUE THEN  
              rm-cst-amt = item.avg-cost.
           ELSE
              rm-cst-amt = item.last-cost.

           IF as-of-date EQ TODAY THEN
              lv-q-onh = item.q-onh.
           ELSE
              RUN rm-mkbin (ROWID(item), OUTPUT lv-q-onh).

           assign
              v-qty   = lv-q-onh + (if tb_qty-ord then item.q-ono else 0) - item.q-comm
              v-value = (if v-av then v-qty else lv-q-onh) *
                        (if v-avgcost then item.avg-cost else item.last-cost).
        END.
        FIND FIRST tt-job WHERE tt-job.i-no = ITEM.i-no
            AND tt-job.resource NE "" NO-LOCK NO-ERROR.
        if (tb_zero-bal or v-qty ne 0)     and
           (NOT tb_neg OR v-qty LT 0) AND 
           (NOT tb_allocated OR AVAIL tt-job) then do:

           FIND FIRST tt-po WHERE
                tt-po.i-no EQ ITEM.i-no
                NO-ERROR.

           FIND FIRST procat WHERE
                procat.company EQ cocode AND
                procat.procat EQ ITEM.procat
                NO-LOCK NO-ERROR.

           display
              item.loc
              item.i-no
              item.i-name
              procat.dscr WHEN AVAIL procat
              ITEM.cons-uom when item.i-code ne "E"
              rm-cst-amt when item.i-code ne "E"
              lv-q-onh when item.i-code ne "E"
              ITEM.q-ono when item.i-code ne "E"
              tt-po.po-line WHEN AVAIL tt-po @ v-po-line
              ITEM.q-avail when item.i-code ne "E"
              v-value when item.i-code ne "E"
              with frame itemx.

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  '"' item.loc '",'              
                  '"' REPLACE(item.i-no,",","") '",'  
                  '"' REPLACE(item.i-name,",","") '",'
                  '"' (IF AVAIL procat THEN procat.dscr ELSE " ") '",'
                  '"' (IF item.i-code NE "E" THEN ITEM.cons-uom ELSE "") '",'
                  '"' (IF item.i-code NE "E" THEN STRING(rm-cst-amt,">>>,>>9.9999") ELSE "") '",'    
                  '"' (IF item.i-code NE "E" THEN STRING(lv-q-onh,"->>>,>>>,>>9.99") ELSE "") '",'
                  '"' (IF item.i-code NE "E" THEN STRING(ITEM.q-ono,"->>>,>>>,>>9.99") ELSE "") '",'
                  '"' (IF AVAIL tt-po THEN tt-po.po-line ELSE "") '",'
                  '"' (IF item.i-code NE "E" THEN STRING(ITEM.q-avail,"->>>,>>>,>>9.99") ELSE "") '",'
                  '"' (IF item.i-code NE "E" THEN STRING(v-value,"->,>>>,>>9.99") ELSE "") '",'
                  SKIP.

           IF AVAIL tt-po THEN
              DELETE tt-po.

           FOR EACH tt-po WHERE
               tt-po.i-no EQ ITEM.i-no:

               DISPLAY tt-po.po-line @ v-po-line WITH FRAME itemx.
               DOWN WITH FRAME itemx.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' "" '",'              
                      '"' "" '",'  
                      '"' "" '",'
                      '"' "" '",'
                      '"' "" '",'
                      '"' "" '",'    
                      '"' "" '",'
                      '"' "" '",'
                      '"' tt-po.po-line '",'
                      SKIP.

               DELETE tt-po.
           END.

           v-alloc-total = 0.
.
           FOR EACH tt-job WHERE
               tt-job.i-no EQ ITEM.i-no AND
               tt-job.resource NE ""
               BREAK BY tt-job.i-no
                     BY tt-job.print-date
                     BY tt-job.seq
                     BY tt-job.m-code:

               IF FIRST(tt-job.i-no) THEN PUT SKIP(1).

               v-alloc-total = v-alloc-total + tt-job.alloc-qty.

               IF FIRST(tt-job.i-no) THEN
               DO:
                  PUT space(45) "Jobs            Resource   Start Date  Allocation    Alloc. Total" SKIP
                      SPACE(45) "---------       ---------- ----------  ------------- ---------------" SKIP.

                  IF tb_excel THEN
                     PUT STREAM excel UNFORMATTED
                         '"' "" '",'
                         '"' "" '",'
                         '"' "" '",'
                         '"' "Jobs" '",'              
                         '"' "Resource" '",'  
                         '"' "Start Date" '",'
                         '"' "Allocation" '",'
                         '"' "Alloc. Total" '",'
                         SKIP.
               END.

               PUT space(45) tt-job.job-no FORMAT "X(6)"
                   "-"
                   tt-job.job-no2 FORMAT "99"
                   space(7) tt-job.resource FORMAT "X(10)" SPACE(1)
                   tt-job.print-date SPACE(1) tt-job.alloc-qty FORMAT "->>,>>>,>>9.99" space(2)
                   v-alloc-total FORMAT "->>,>>>,>>9.99" SKIP.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' "" '",'
                      '"' "" '",'
                      '"' "" '",'
                      '"' STRING(tt-job.job-no,"X(9)") '",'              
                      '"' STRING(tt-job.resource,"X(8)") '",'  
                      '"' (IF tt-job.print-date NE ? THEN
                             STRING(tt-job.print-date,"99/99/9999") ELSE "") '",'
                      '"' STRING(tt-job.alloc-qty,"->>>,>>>,>>9.99") '",'
                      '"' STRING(v-alloc-total,"->>>,>>>,>>9.99") '",'
                      SKIP.

               IF LAST(tt-job.i-no) THEN
               DO:
                  PUT SPACE(103) v-alloc-total SKIP(1).

                  IF tb_excel THEN
                     PUT STREAM excel UNFORMATTED
                         '"' "" '",'
                         '"' "" '",'
                         '"' "" '",'
                         '"' "" '",'              
                         '"' "" '",'  
                         '"' "" '",'
                         '"' "" '",'
                         '"' STRING(v-alloc-total,"->>>,>>>,>>9") '",'
                         SKIP(1).
               END.
           END.

           DOWN with frame itemx.
        END. /*display item*/

    end. /*each item*/

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 1992  advanced software, inc. */


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

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

run run-board.

OUTPUT CLOSE.
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
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

