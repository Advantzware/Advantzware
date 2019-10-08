&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: porep\r-purvar.w
  Description: PO Purchased Variance
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

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHAR.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20.

DEF TEMP-TABLE temp-adder NO-UNDO
    FIELD adder LIKE ITEM.i-no
    FIELD adder-index AS INT
    INDEX temp-adder-index adder-index ASC.

DEFINE BUFFER xjob-mat FOR job-mat.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEFINE STREAM st-excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_po-no end_po-no ~
begin_po-date end_po-date begin_vend-no end_vend-no begin_po-i-no ~
end_po-i-no begin_job-no end_job-no tb_receipt begin_rec-date end_rec-date ~
rd_vend-cost tb_mpv tb_repeat tb_overs tb_adder rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_po-no end_po-no begin_po-date ~
end_po-date begin_vend-no end_vend-no begin_po-i-no end_po-i-no ~
begin_job-no end_job-no tb_receipt begin_rec-date end_rec-date ~
lbl_vend-cost rd_vend-cost tb_mpv tb_repeat tb_overs tb_adder rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_rec-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Rec Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 999999 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rec-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Rec Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-purvar.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_vend-cost AS CHARACTER FORMAT "X(256)":U INITIAL "Show Vendor $ from..." 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_vend-cost AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor Matrix", "Vendor Matrix",
"Invoiced Amt", "Invoiced Amt"
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 11.38.

DEFINE VARIABLE tb_adder AS LOGICAL INITIAL no 
     LABEL "Adder Codes" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_mpv AS LOGICAL INITIAL no 
     LABEL "MPV %" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE tb_overs AS LOGICAL INITIAL no 
     LABEL "Overs %" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE tb_receipt AS LOGICAL INITIAL no 
     LABEL "Only Purchase Orders With Receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE tb_repeat AS LOGICAL INITIAL no 
     LABEL "Repeat PO#/Vendor#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

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
     begin_po-no AT ROW 2.19 COL 26 COLON-ALIGNED HELP
          "Enter Beginning PO Number"
     end_po-no AT ROW 2.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending PO Number"
     begin_po-date AT ROW 3.14 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Due Date"
     end_po-date AT ROW 3.14 COL 69 COLON-ALIGNED HELP
          "Enter ending Due Date"
     begin_vend-no AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     begin_po-i-no AT ROW 5.05 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_po-i-no AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_job-no AT ROW 6 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6 COL 69 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     tb_receipt AT ROW 7 COL 28 WIDGET-ID 6
     begin_rec-date AT ROW 7.91 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Rec Date" WIDGET-ID 2
     end_rec-date AT ROW 7.95 COL 69 COLON-ALIGNED HELP
          "Enter ending Rec Date" WIDGET-ID 4
     lbl_vend-cost AT ROW 9.1 COL 26 COLON-ALIGNED NO-LABEL
     rd_vend-cost AT ROW 9.1 COL 51 NO-LABEL
     tb_mpv AT ROW 10.48 COL 56
     tb_repeat AT ROW 10.52 COL 28
     tb_overs AT ROW 11.48 COL 28
     tb_adder AT ROW 11.48 COL 56
     rd-dest AT ROW 13.86 COL 6 NO-LABEL
     lv-ornt AT ROW 14.33 COL 31 NO-LABEL
     lines-per-page AT ROW 14.33 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 16 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.95 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.05 COL 31
     tb_excel AT ROW 19.57 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.57 COL 72.4 RIGHT-ALIGNED
     fi_file AT ROW 20.48 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.71 COL 19
     btn-cancel AT ROW 22.71 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.91 COL 4
     RECT-6 AT ROW 12.62 COL 2
     RECT-7 AT ROW 1.33 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 23.71.


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
         TITLE              = "PO Purchased Variance"
         HEIGHT             = 23.81
         WIDTH              = 95.6
         MAX-HEIGHT         = 23.81
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 23.81
         VIRTUAL-WIDTH      = 95.8
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
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rec-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rec-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_vend-cost IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_vend-cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_vend-cost".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_vend-cost:PRIVATE-DATA IN FRAME FRAME-A     = 
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PO Purchased Variance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PO Purchased Variance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date C-Win
ON LEAVE OF begin_po-date IN FRAME FRAME-A /* Beginning PO Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rec-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rec-date C-Win
ON LEAVE OF begin_rec-date IN FRAME FRAME-A /* Beginning Rec Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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
  STATUS DEFAULT "Processing Complete".
  IF tb_excel AND tb_runExcel THEN
  OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_po-no
                            &END_cust=END_po-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-date C-Win
ON LEAVE OF end_po-date IN FRAME FRAME-A /* Ending PO Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-i-no C-Win
ON LEAVE OF end_po-i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rec-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rec-date C-Win
ON LEAVE OF end_rec-date IN FRAME FRAME-A /* Ending Rec Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME tb_adder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adder C-Win
ON VALUE-CHANGED OF tb_adder IN FRAME FRAME-A /* Adder Codes */
DO:
    assign {&self-name}
           tb_mpv
           tb_overs.

    IF tb_adder THEN
       ASSIGN lv-ornt:SCREEN-VALUE = "L"
              lv-ornt:SENSITIVE = NO.
    ELSE
       IF NOT tb_overs AND
          NOT tb_mpv THEN
          ASSIGN
             lv-ornt:SENSITIVE = YES
             lv-ornt:SCREEN-VALUE = "P".
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


&Scoped-define SELF-NAME tb_mpv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mpv C-Win
ON VALUE-CHANGED OF tb_mpv IN FRAME FRAME-A /* MPV % */
DO:
    assign {&self-name}
           tb_overs
           tb_adder.

    IF tb_mpv THEN
       ASSIGN lv-ornt:SCREEN-VALUE = "L"
              lv-ornt:SENSITIVE = NO.
    ELSE
       IF NOT tb_overs AND
          NOT tb_adder THEN
          ASSIGN
             lv-ornt:SENSITIVE = YES
             lv-ornt:SCREEN-VALUE = "P".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_overs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_overs C-Win
ON VALUE-CHANGED OF tb_overs IN FRAME FRAME-A /* Overs % */
DO:
    assign {&self-name}
           tb_mpv
           tb_adder.

    IF tb_overs THEN
       ASSIGN lv-ornt:SCREEN-VALUE = "L"
              lv-ornt:SENSITIVE = NO.
    ELSE
       IF NOT tb_mpv AND
          NOT tb_adder THEN
          ASSIGN
             lv-ornt:SENSITIVE = YES
             lv-ornt:SCREEN-VALUE = "P".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_receipt C-Win
ON VALUE-CHANGED OF tb_receipt IN FRAME FRAME-A /* Only Purchase Orders With Receipts? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_repeat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_repeat C-Win
ON VALUE-CHANGED OF tb_repeat IN FRAME FRAME-A /* Repeat PO#/Vendor#? */
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

  assign
   begin_po-date = date(1,1,year(today))
   end_po-date   = today.

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_po-no.

    IF tb_mpv:SCREEN-VALUE = "YES" OR
       tb_overs:SCREEN-VALUE = "YES" OR
       tb_adder:SCREEN-VALUE = "YES" THEN
       ASSIGN
         lv-ornt:SENSITIVE = NO
         lv-ornt:SCREEN-VALUE = "L".
  END.

  {methods/nowait.i}

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adder-proc C-Win 
PROCEDURE adder-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR viIndex AS INT NO-UNDO.

   EMPTY TEMP-TABLE temp-adder.

   FIND FIRST job WHERE
        job.company EQ po-ordl.company AND
        job.job-no  EQ po-ordl.job-no AND
        job.job-no2 EQ po-ordl.job-no2
        NO-LOCK NO-ERROR.

   IF AVAIL job THEN
      FIND FIRST xjob-mat WHERE
           xjob-mat.company  EQ job.company AND
           xjob-mat.job      EQ job.job AND
           xjob-mat.job-no   EQ job.job-no AND
           xjob-mat.job-no2  EQ job.job-no2 AND
           xjob-mat.frm      EQ po-ordl.s-num AND
           xjob-mat.blank-no EQ po-ordl.b-num
           USE-INDEX seq-idx
           NO-LOCK NO-ERROR.

   IF AVAIL xjob-mat THEN 
      for each job-mat WHERE
          job-mat.company  eq xjob-mat.company AND
          job-mat.job      eq xjob-mat.job AND
          job-mat.frm      eq xjob-mat.frm AND
          job-mat.job-no   eq xjob-mat.job-no AND
          job-mat.job-no2  eq xjob-mat.job-no2
          use-index seq-idx
          NO-LOCK,
          first ITEM WHERE
                item.company  eq job-mat.company AND
                item.i-no     eq job-mat.i-no AND
                item.mat-type eq "A"
                NO-LOCK:

          CREATE temp-adder.
          ASSIGN temp-adder.adder = ITEM.i-no
                 viIndex = viIndex + 1
                 temp-adder.adder-index = viIndex.
          RELEASE temp-adder.
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
  DISPLAY begin_po-no end_po-no begin_po-date end_po-date begin_vend-no 
          end_vend-no begin_po-i-no end_po-i-no begin_job-no end_job-no 
          tb_receipt begin_rec-date end_rec-date lbl_vend-cost rd_vend-cost 
          tb_mpv tb_repeat tb_overs tb_adder rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_po-no end_po-no begin_po-date end_po-date 
         begin_vend-no end_vend-no begin_po-i-no end_po-i-no begin_job-no 
         end_job-no tb_receipt begin_rec-date end_rec-date rd_vend-cost tb_mpv 
         tb_repeat tb_overs tb_adder rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

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
PROCEDURE run-report :
/* ----------------------------------------------- po/rep/po-pvar.p 11/98 FWK */
/* PO Purchased Variance                                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-foot-rem like ap-invl.amt-msf no-undo.
def var v-msf-cal as log no-undo.

def var v-s-pono like po-ord.po-no format ">>>>>>".
def var v-e-pono like v-s-pono init 999999.
def var v-s-date like po-ord.po-date format "99/99/9999" init "01/01/0001".
def var v-e-date like v-s-date init today.
def var v-s-vend like po-ord.vend-no.
def var v-e-vend like v-s-vend init "zzzzzzzz".
def var v-s-item like po-ordl.i-no.
def var v-tt-ei like v-s-item init "zzzzzzzzzzzzzzz".
def var v-s-job like po-ordl.job-no.
def var v-e-job like v-s-job init "zzzzzz".
def var v-stat   as   char format "!" init "A".
def var v-type   as   char format "!" init "B".
def var v-sort   as   char format "!" init "V".
def var vb-rec-date like po-ord.po-date format "99/99/9999" init "01/01/0001".
def var ve-rec-date like v-s-date init today.

def var v-mattype-list          as   char format "x(36)".
def var v-mat-dscr              as   char format "x(20)" extent 21.

def var v-bal                   like rm-rdtlh.qty.

def var v-first                 like report.key-02 extent 4.
def var v-ord                   like rm-rdtlh.qty extent 4.
def var v-qty                   like rm-rdtlh.qty extent 4.

def var str-tit4                like str-tit3.

def var v-tot-msf as dec format ">>,>>9.999" init 0 no-undo.
def var v-tot-vend as dec format ">>,>>9.999" init 0 no-undo.
def var v-uom-vend AS CHAR no-undo. 
def var diff-price as dec format "->>>,>>9.99" no-undo.
def var v-vend-cost as dec format "->>>,>>9.99" no-undo.
def var v-cost as dec format "->>>,>>9.99" no-undo.
def var v-sub-msf as dec format "->>,>>>,>>9.999" no-undo. 
def var v-sub-diff as dec format "->>>,>>>,>>9.99" no-undo.
def var v-sub-vend as dec format "->>>,>>>,>>9.99" no-undo.
def var v-sub-bght as dec format "->>>>>>,>>9.99" no-undo.
def var v-grand-msf as dec format "->>>,>>>,>>9.99" no-undo.
def var v-grand-diff as dec format "->>>>>>,>>9.99" no-undo.
def var v-grand-vend as dec format "->>>,>>>,>>9.99" no-undo.
def var v-grand-bght as dec format "->>>>>>,>>9.99" no-undo.

def var ii as int.
def var ld as DEC NO-UNDO.
def var v-bld-job as char format "x(9)".
DEF VAR v-inv-cost AS DEC NO-UNDO.
DEF VAR v-po-cost  AS DEC NO-UNDO.
DEF VAR v-mpv      AS DEC NO-UNDO.
DEF VAR v-line-num AS INT NO-UNDO.
DEF VAR v-overs    AS DEC NO-UNDO.
DEF VAR v-ord-qty  AS DEC NO-UNDO.
DEF VAR v-inv-qty  AS DEC NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR viIndex AS INT NO-UNDO.
DEFINE VARIABLE receiptDate AS DATE NO-UNDO.
DEFINE VARIABLE DueDate AS DATE NO-UNDO.
DEF VAR v-moa-cols AS LOG NO-UNDO.
DEF VAR ld-dim-charge AS DEC NO-UNDO.

form po-ord.po-no          column-label "PO #" space(2)
     po-ord.vend-no        column-label "Vendor #" space(2)
     v-bld-job             column-label "Job #" space(2)
     po-ordl.i-no          column-label "Item #" space(2)
     DueDate               column-label "Due Date" space(2)
     receiptDate           column-label "Received" space(2)
     v-tot-msf             column-label "MSF" space(3)
     v-vend-cost           column-label "Vendor $" space(3)
     v-cost                column-label "Bought $" space(3)
     diff-price            column-label "Diff $"
    with frame main no-box no-attr-space down STREAM-IO width 180.

form po-ord.po-no          column-label "PO #" space(2)
     po-ord.vend-no        column-label "Vendor #!/Adders" space(2)
     v-bld-job             column-label "Job #" space(2)
     po-ordl.i-no          column-label "Item #" space(2)
     DueDate               column-label "Due Date" space(2)
     receiptDate           column-label "Received" space(2)
     v-tot-msf             column-label "MSF" space(3)
     v-vend-cost           column-label "Vendor $" space(3)
     v-cost                column-label "Bought $" space(3)
     diff-price            column-label "Diff $"
     v-mpv                 COLUMN-LABEL "MPV %"  
     v-overs               COLUMN-LABEL "Overs %" 
    with frame main-b no-box no-attr-space down STREAM-IO width 180.

   {ce/msfcalc.i}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-pono   = begin_po-no
 v-e-pono   = end_po-no
 v-s-date   = begin_po-date
 v-e-date   = end_po-date
 v-s-vend   = begin_vend-no
 v-e-vend   = end_vend-no
 v-s-item   = begin_po-i-no
 v-tt-ei   = end_po-i-no
 v-s-job    = begin_job-no
 v-e-job    = END_job-no
 vb-rec-date = begin_rec-date
 ve-rec-date = end_rec-date .

IF tb_mpv OR tb_overs OR tb_adder THEN
   v-moa-cols = YES.

{sa/sa-sls01.i}

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

IF tb_excel AND fi_file NE '' THEN DO:
  OUTPUT STREAM st-excel TO VALUE(fi_file).
  excelheader = "PO #,Vendor #".
  IF tb_adder THEN
     excelheader = excelheader + "/Adders".
  excelheader = excelheader
              + ",Job #,Item #,Due Date,Received,MSF,Vendor $,Bought $,Diff $".
  IF tb_mpv THEN
     excelheader = excelheader + ",MPV %".
  IF tb_overs THEN
     excelheader = excelheader + ",Overs %".
  PUT STREAM st-excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
for each po-ord 
        where po-ord.company eq cocode
          and po-ord.po-no   ge v-s-pono
          and po-ord.po-no   le v-e-pono
          and po-ord.po-date ge v-s-date
          and po-ord.po-date le v-e-date
          and po-ord.vend-no ge v-s-vend
          and po-ord.vend-no le v-e-vend
        no-lock,
        each po-ordl
        where po-ordl.company EQ po-ord.company
          AND po-ordl.po-no EQ po-ord.po-no
          AND po-ordl.i-no      ge v-s-item
          and po-ordl.i-no      le v-tt-ei
          and po-ordl.job-no ge v-s-job
          and po-ordl.job-no le v-e-job
        no-lock break by po-ord.po-no
                by po-ord.po-date
                by po-ordl.po-no 
                by po-ord.vend-no 
                by po-ordl.job-no 
                by po-ordl.job-no2 
                by po-ordl.i-no:
    {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}
   find first item
       where item.company eq cocode
         and item.i-no    eq po-ordl.i-no
         and po-ordl.item-type
       no-lock no-error.
   assign v-bld-job = "".
   do ii = 1 to 6: 
      if substring(po-ordl.job-no,ii,1) ne " " then
               assign v-bld-job  = trim(v-bld-job +
           substring(po-ordl.job-no,ii,1)).
   end. 
   if v-bld-job ne "      " then
   assign v-bld-job =
            string(fill(" ",6 - length(v-bld-job))) +
            (trim(v-bld-job)) +  "-" + string(po-ordl.job-no2,"99").
     if po-ordl.pr-qty-uom eq "MSF" then
     v-tot-msf = po-ordl.ord-qty.
   else
     run sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                            (if avail item then item.basis-w else 0),
                            po-ordl.s-len, po-ordl.s-wid,
                            (if avail item then item.s-dep else 0),
                            po-ordl.ord-qty, output v-tot-msf).

def var pr-ct as int no-undo.
  ASSIGN
    v-vend-cost = 0
    receiptDate = ?   .

  /*IF tb_receipt THEN DO:*/
    IF po-ordl.item-type THEN DO:
      FIND FIRST rm-rcpth NO-LOCK
           WHERE rm-rcpth.company EQ cocode
             AND rm-rcpth.i-no EQ po-ordl.i-no
             and (((rm-rcpth.rita-code eq "R" or rm-rcpth.rita-code eq "A")))
             and rm-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9")) 
             AND  rm-rcpth.trans-date GE vb-rec-date  
             AND rm-rcpth.trans-date  LE ve-rec-date     NO-ERROR.
      IF AVAILABLE rm-rcpth THEN receiptDate = rm-rcpth.trans-date.
       IF tb_receipt THEN
           IF NOT AVAIL rm-rcpth THEN NEXT .
   END.
   ELSE DO:
      FIND FIRST fg-rcpth NO-LOCK
           WHERE fg-rcpth.company EQ cocode
             AND fg-rcpth.i-no EQ po-ordl.i-no
             and fg-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9"))
             and (fg-rcpth.rita-code eq "R" or
                 fg-rcpth.rita-code eq "A") 
              AND fg-rcpth.trans-date GE vb-rec-date  
             AND fg-rcpth.trans-date LE ve-rec-date  NO-ERROR.
      IF AVAILABLE fg-rcpth THEN receiptDate = fg-rcpth.trans-date .
      IF tb_receipt THEN
          IF NOT AVAIL fg-rcpth THEN NEXT .
  END.
 /*END.*/

IF rd_vend-cost BEGINS "Vend" THEN DO:
  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.
  IF po-ordl.item-type THEN DO:
    FIND FIRST e-item
        WHERE e-item.company EQ cocode
          AND e-item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL e-item THEN DO:
      CREATE tt-ei.
      ASSIGN tt-ei.std-uom = e-item.std-uom.
      FIND FIRST e-item-vend OF e-item
          WHERE e-item-vend.vend-no EQ po-ord.vend-no
          NO-LOCK NO-ERROR.
      IF AVAIL e-item-vend THEN DO:
         CREATE tt-eiv.
         DO pr-ct = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[pr-ct] = e-item-vend.run-qty[pr-ct]
               tt-eiv.run-cost[pr-ct] = e-item-vend.run-cost[pr-ct].
         END.
         FIND FIRST b-qty WHERE
              b-qty.reftable = "vend-qty" AND
              b-qty.company = e-item-vend.company AND
                  b-qty.CODE    = e-item-vend.i-no AND
              b-qty.code2   = e-item-vend.vend-no
              NO-LOCK NO-ERROR.

         IF AVAIL b-qty THEN
         DO:
            FIND FIRST b-cost WHERE
                 b-cost.reftable = "vend-cost" AND
                 b-cost.company = e-item-vend.company AND
                         b-cost.CODE    = e-item-vend.i-no AND
                 b-cost.code2   = e-item-vend.vend-no
                 NO-LOCK NO-ERROR.

            DO pr-ct = 1 TO 10:
               ASSIGN
                  tt-eiv.run-qty[pr-ct + 10] = b-qty.val[pr-ct]
                  tt-eiv.run-cost[pr-ct + 10] = b-cost.val[pr-ct].
            END.
         END.
      END.
    END.
  END.
  ELSE DO:
    FIND FIRST e-itemfg
        WHERE e-itemfg.company EQ cocode
          AND e-itemfg.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL e-itemfg THEN DO:
      CREATE tt-ei.
      BUFFER-COPY e-itemfg TO tt-ei.
      FIND FIRST e-itemfg-vend OF e-itemfg
          WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
          NO-LOCK NO-ERROR.
      IF AVAIL e-itemfg-vend THEN DO:
        CREATE tt-eiv.
        DO pr-ct = 1 TO 10:
           ASSIGN
              tt-eiv.run-qty[pr-ct] = e-itemfg-vend.run-qty[pr-ct]
              tt-eiv.run-cost[pr-ct] = e-itemfg-vend.run-cost[pr-ct].
        END.
      END.
    END.
  END.
  find first tt-eiv no-error.

  if avail tt-eiv then do:
    find first tt-ei no-error.

    v-uom-vend = if avail tt-ei then tt-ei.std-uom else "EA".

    if po-ordl.pr-qty-uom eq v-uom-vend then
      v-tot-vend = po-ordl.ord-qty.
    else
      run sys/ref/convquom.p(po-ordl.pr-qty-uom, v-uom-vend,
                             (if avail item then item.basis-w else 0),
                             po-ordl.s-len, po-ordl.s-wid,
                             (if avail item then item.s-dep else 0),
                             po-ordl.ord-qty, output v-tot-vend).

    IF AVAIL tt-eiv THEN DO:
      ld-dim-charge = 0.
      IF AVAIL e-item-vend  THEN
      RUN est/dim-charge.p (e-item-vend.rec_key,
                            po-ordl.s-wid,
                            po-ordl.s-len,
                            INPUT-OUTPUT ld-dim-charge).
      DO pr-ct = 1 TO 20:
        IF tt-eiv.run-qty[pr-ct] GE v-tot-vend THEN DO:
           v-vend-cost = (tt-eiv.run-cost[pr-ct] + ld-dim-charge) * v-tot-vend.
           LEAVE.
        END.
      END.
    END.
  end.
END.
ELSE RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT ld, OUTPUT v-vend-cost).
v-line-num = ((po-ordl.po-no * 1000) + po-ordl.line).
IF tb_mpv OR tb_overs THEN
DO:
   FIND FIRST ap-invl WHERE
        ap-invl.company EQ cocode AND
        ap-invl.po-no EQ po-ord.po-no AND
        ap-invl.LINE EQ v-line-num
        NO-LOCK NO-ERROR.
   IF AVAIL ap-invl THEN
   DO:
     if ap-invl.pr-qty-uom eq "MSF" then
        v-inv-cost = ap-invl.unit-pr.
      else
        run sys/ref/convcuom.p(ap-invl.pr-qty-uom, "MSF",
                               (if avail item then item.basis-w else 0),
                               po-ordl.s-len, po-ordl.s-wid,
                               (if avail item then item.s-dep else 0),
                               ap-invl.unit-pr, output v-inv-cost).

      IF po-ordl.pr-uom EQ "MSF" THEN
         v-po-cost = po-ordl.cost.
      ELSE
         run sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                               (if avail item then item.basis-w else 0),
                               po-ordl.s-len, po-ordl.s-wid,
                               (if avail item then item.s-dep else 0),
                               po-ordl.cost, output v-po-cost).

      if po-ordl.pr-qty-uom eq "MSF" then
         v-ord-qty = po-ordl.ord-qty.
      else
        run sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                               (if avail item then item.basis-w else 0),
                               po-ordl.s-len, po-ordl.s-wid,
                               (if avail item then item.s-dep else 0),
                               po-ordl.ord-qty, output v-ord-qty).

      if ap-invl.cons-uom eq "MSF" then
         v-inv-qty = ap-invl.qty. 
      else
        run sys/ref/convquom.p(ap-invl.cons-uom, "MSF",
                               (if avail item then item.basis-w else 0),
                               po-ordl.s-len, po-ordl.s-wid,
                               (if avail item then item.s-dep else 0),
                               ap-invl.qty, output v-inv-qty).

      ASSIGN
         v-mpv = IF v-po-cost NE 0 THEN (v-inv-cost / v-po-cost) * 100
                 ELSE 0
         v-overs = IF v-ord-qty NE 0 THEN (v-inv-qty / v-ord-qty) * 100
                   ELSE 0.

      RELEASE ap-invl.
   END.
   ELSE
      ASSIGN
         v-mpv = 0
         v-overs = 0.
END.
IF tb_adder THEN
   RUN adder-proc.
assign v-cost = po-ordl.t-cost
       DueDate = po-ordl.due-date
       diff-price = v-cost - v-vend-cost 
       v-sub-msf = v-sub-msf + v-tot-msf
       v-sub-diff = v-sub-diff + diff-price
       v-sub-vend = v-sub-vend + v-vend-cost
       v-sub-bght = v-sub-bght + v-cost
       v-grand-msf = v-grand-msf + v-tot-msf
       v-grand-diff = v-grand-diff + diff-price
       v-grand-vend = v-grand-vend + v-vend-cost
       v-grand-bght = v-grand-bght + v-cost.
DO WITH FRAME main:

  IF NOT v-moa-cols THEN
  DO:

    IF tb_repeat THEN DISPLAY po-ord.po-no WITH FRAME main.
    ELSE IF FIRST-OF(po-ord.po-no) THEN DISPLAY po-ord.po-no WITH FRAME main.
    IF tb_repeat THEN DISPLAY po-ord.vend-no WITH FRAME main.
    ELSE IF FIRST-OF(po-ord.vend-no) THEN DISPLAY po-ord.vend-no WITH FRAME main.
    DISPLAY
     v-bld-job
     po-ordl.i-no
     DueDate 
     receiptDate
     v-tot-msf
     v-vend-cost
     v-cost
     diff-price
     WITH FRAME main.
  END.
  ELSE
  DO:

      PUT SPACE (1).
    IF tb_repeat THEN DISPLAY po-ord.po-no WITH FRAME main-b.
    ELSE IF FIRST-OF(po-ord.po-no) THEN DISPLAY po-ord.po-no WITH FRAME main-b.
    IF tb_repeat THEN DISPLAY po-ord.vend-no WITH FRAME main-b.
    ELSE IF FIRST-OF(po-ord.vend-no) THEN DISPLAY po-ord.vend-no
                                          WITH FRAME main-b.
    DISPLAY
     v-bld-job
     po-ordl.i-no
     DueDate 
     receiptDate
     v-tot-msf
     v-vend-cost
     v-cost
     diff-price
     v-mpv WHEN tb_mpv
     v-overs WHEN tb_overs  WITH FRAME main-b.
  END.
  IF tb_adder THEN
     FOR EACH temp-adder:
         PUT SPACE(8) temp-adder.adder SKIP.
     END.
  IF tb_excel AND fi_file NE '' THEN
  DO:
     PUT STREAM st-excel UNFORMATTED
       '"' po-ord.po-no '",'
       '"' po-ord.vend-no '",'
       '"' v-bld-job '",'
       '"' REPLACE(REPLACE(po-ordl.i-no,',',' '),'"','~'~'') '",'
       '"' (IF po-ordl.due-date NE ? THEN STRING(po-ordl.due-date) ELSE '') '",'
       '"' (IF receiptDate NE ? THEN STRING(receiptDate) ELSE '') '",'
       '"' v-tot-msf '",'
       '"' v-vend-cost '",'
       '"' v-cost '",'
       '"' diff-price '",'
       '"' IF tb_mpv THEN STRING(v-mpv) ELSE "" '",'
       '"' IF tb_overs THEN STRING(v-overs) ELSE "" '",' SKIP.

      IF tb_adder THEN
         FOR EACH temp-adder:
             PUT STREAM st-excel UNFORMATTED
                 '"' ""               '",'
                 '"' temp-adder.adder '",' SKIP.
         END.
  END.
END.
down with frame main.
if last-of(po-ord.po-no) then
do:
  put     "----------" at 69
          "-----------" at 82
          "-----------" at 96
          "-----------" at 110 skip
          "Sub Totals:" to 30
          v-sub-msf  to 76
          v-sub-vend to 91
          v-sub-bght to 105
          v-sub-diff to 120 skip(1).
  IF tb_excel AND fi_file NE '' THEN
  PUT STREAM st-excel UNFORMATTED
    SKIP(1)
    '"",'
    '"",'
    '"Sub Totals:",'
    '"",'
    '"",'
    '"",'
    '"' v-sub-msf '",'
    '"' v-sub-vend '",'
    '"' v-sub-bght '",'
    '"' v-sub-diff '"' SKIP(1).

  assign v-sub-msf = 0
         v-sub-diff = 0
         v-sub-vend = 0
         v-sub-bght = 0.
end.
    end.
  put     "----------" at 69
          "-----------" at 82
          "-----------" at 96
          "-----------" at 110 skip
          "Grand Totals:" to 30
          v-grand-msf  to 76
          v-grand-vend to 91
          v-grand-bght to 105
          v-grand-diff to 120 skip(1).

  IF tb_excel AND fi_file NE '' THEN
  PUT STREAM st-excel UNFORMATTED
    SKIP(1)
    '"",'
    '"",'
    '"Grand Totals:",'
    '"",'
    '"",'
    '"",'
    '"' v-grand-msf '",'
    '"' v-grand-vend '",'
    '"' v-grand-bght '",'
    '"' v-grand-diff '"' SKIP(1).

  assign v-grand-msf = 0
         v-grand-diff = 0
         v-grand-vend = 0
         v-grand-bght = 0.

IF tb_excel AND fi_file NE '' THEN
   OUTPUT STREAM st-excel CLOSE.
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE ("").
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

