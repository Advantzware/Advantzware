&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-trans.w

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_date end_date begin_whs end_whs begin_job-no ~
begin_job-no2 end_job-no end_job-no2 select-mat tb_receipts tb_issues ~
tb_transfers tb_adjustments tb_counts tb_issue-detail rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_date end_date begin_whs end_whs begin_job-no begin_job-no2 ~
end_job-no end_job-no2 select-mat tb_receipts tb_issues tb_transfers ~
tb_adjustments tb_counts tb_issue-detail rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-trh.csv" 
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
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 13.1.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 5 NO-UNDO.

DEFINE VARIABLE tb_adjustments AS LOGICAL INITIAL yes 
     LABEL "Adjustments?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_counts AS LOGICAL INITIAL yes 
     LABEL "Cycle Counts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_issue-detail AS LOGICAL INITIAL yes 
     LABEL "Print FG Item Detail in Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE tb_issues AS LOGICAL INITIAL yes 
     LABEL "Issues?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_receipts AS LOGICAL INITIAL yes 
     LABEL "Receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_transfers AS LOGICAL INITIAL yes 
     LABEL "Transfers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 2.19 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_procat AT ROW 3.14 COL 26 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 3.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_date AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter ending Date"
     begin_whs AT ROW 5.05 COL 26 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_job-no AT ROW 6 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6 COL 41 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6 COL 69 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6 COL 84 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     mat-types AT ROW 7.67 COL 15 COLON-ALIGNED
     select-mat AT ROW 7.95 COL 19 HELP
          "Enter description of this Material Type." NO-LABEL
     tb_receipts AT ROW 8.19 COL 65
     tb_issues AT ROW 9.14 COL 65
     tb_transfers AT ROW 10.1 COL 65
     tb_adjustments AT ROW 11.05 COL 65
     tb_counts AT ROW 12 COL 65
     tb_issue-detail AT ROW 12.91 COL 19 WIDGET-ID 2
     rd-dest AT ROW 15.43 COL 4 NO-LABEL
     lv-ornt AT ROW 15.67 COL 30 NO-LABEL
     lines-per-page AT ROW 15.67 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 17.81 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.76 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.14 COL 26
     tb_excel AT ROW 21.14 COL 70 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.14 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 22.33 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.71 COL 26
     btn-cancel AT ROW 23.71 COL 59
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 7.24 COL 21
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.48 COL 2
     RECT-6 AT ROW 14 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
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
         TITLE              = "Transaction History"
         HEIGHT             = 24.33
         WIDTH              = 97.8
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_adjustments:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_counts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_issue-detail:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_issues:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_receipts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_transfers:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transaction History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaction History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
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
           {custom/asifax.i &type= ''
                            &begin_cust= "begin_procat"
                            &END_cust= "begin_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_procat"
                             &END_cust= "begin_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust="begin_procat"
                                  &END_cust="begin_procat"
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
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
  IF SELF:SCREEN-VALUE BEGINS "L" THEN 
    ASSIGN lv-font-no = "12"
           lines-per-page = 55
           lv-font-name = "Courier New SIZE=8(15CPI)".

 ELSE    ASSIGN lv-font-no = "11"
                lines-per-page = 99
                lv-font-name = "Courier New Size=7 (17 cpi for 132 CLMN REPORT)".

 DISPL lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_adjustments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adjustments C-Win
ON VALUE-CHANGED OF tb_adjustments IN FRAME FRAME-A /* Adjustments? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_counts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_counts C-Win
ON VALUE-CHANGED OF tb_counts IN FRAME FRAME-A /* Cycle Counts? */
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


&Scoped-define SELF-NAME tb_issue-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_issue-detail C-Win
ON VALUE-CHANGED OF tb_issue-detail IN FRAME FRAME-A /* Print FG Item Detail in Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_issues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_issues C-Win
ON VALUE-CHANGED OF tb_issues IN FRAME FRAME-A /* Issues? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_receipts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_receipts C-Win
ON VALUE-CHANGED OF tb_receipts IN FRAME FRAME-A /* Receipts? */
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


&Scoped-define SELF-NAME tb_transfers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_transfers C-Win
ON VALUE-CHANGED OF tb_transfers IN FRAME FRAME-A /* Transfers? */
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
def var v-mat-list as char no-undo.
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

  end_date = today.

  RUN enable_UI.

  for each mat FIELDS(mat dscr) NO-LOCK:
      v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
     substr(v-mat-list,length(trim(v-mat-list)),1) = "".

  select-mat:list-items = v-mat-list.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_rm-no.
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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_date end_date 
          begin_whs end_whs begin_job-no begin_job-no2 end_job-no end_job-no2 
          select-mat tb_receipts tb_issues tb_transfers tb_adjustments tb_counts 
          tb_issue-detail rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_date 
         end_date begin_whs end_whs begin_job-no begin_job-no2 end_job-no 
         end_job-no2 select-mat tb_receipts tb_issues tb_transfers 
         tb_adjustments tb_counts tb_issue-detail rd-dest lv-ornt 
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
{sys/form/r-topw.f}

def var v-fitem like rm-rcpth.i-no NO-UNDO.
def var v-titem like v-fitem init "zzzzzzzzzz" NO-UNDO.
def var v-fpcat like item.procat NO-UNDO.
def var v-tpcat like v-fpcat                  init "zzzzz" NO-UNDO.
def var v-fdate as   date format "99/99/9999" init 01/01/0001 NO-UNDO.
def var v-tdate like v-fdate                  init TODAY NO-UNDO.
def var v-floc  like rm-rcpth.loc NO-UNDO.
def var v-tloc  like v-floc                   initial "zzzzz" NO-UNDO.
def var v-fjob  like job.job-no NO-UNDO.
def var v-tjob  like v-fjob                   init "zzzzzzzzz" NO-UNDO.
def var v-fjob2 like job.job-no2 format "999" NO-UNDO.
def var v-tjob2 like v-fjob2                  init 999 NO-UNDO.
def var v-mtype as   char format "x(47)" NO-UNDO.
def var v-code  like rm-rcpth.rita-code NO-UNDO.

def var v-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-job-no as char format "x(13)" NO-UNDO.
def var v-qty like rm-rdtlh.qty extent 3 NO-UNDO.
def var v-val like v-value extent 3 NO-UNDO.
def var v-first as log extent 3 NO-UNDO.
DEF VAR v-fjob1 AS CHAR NO-UNDO.
DEF VAR v-tjob1 AS CHAR NO-UNDO.
def var v-type  as char format "x(5)" NO-UNDO.
DEF VAR v-job1sub AS INT NO-UNDO.

DEF VAR cCust AS CHAR NO-UNDO.
DEF VAR cINo AS CHAR NO-UNDO.
DEF VAR cIName AS CHAR NO-UNDO.

DEF BUFFER bf-itemfg FOR itemfg.
DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-cust FOR cust.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

    {custom/statusMsg.i "'Processing...'"} 

form rm-rcpth.trans-date format "99/99/99" label "DATE"
     rm-rcpth.i-no label "ITEM"
     rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
     rm-rcpth.po-no label "P.O.#"
     rm-rcpth.rita-code label "TY"
     v-job-no label "   Job #"
     rm-rdtlh.tag label "TAG#" FORMAT 'X(20)'
     rm-rdtlh.qty format "->>>>>9.99<<" label "QUANTITY"
     rm-rdtlh.loc label "WHSE"
     rm-rdtlh.loc-bin label "BIN"
     rm-rdtlh.loc2 column-label "WHSE! TO"
     rm-rdtlh.loc-bin2 column-label "BIN! TO"
     rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
     space(0)
     v-value label "VALUE"
     skip
    with frame itemx no-box down stream-io width 155.

form rm-rcpth.trans-date format "99/99/99" label "DATE"
     rm-rcpth.i-no label "ITEM"
     rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
     rm-rcpth.po-no label "P.O.#"
     rm-rcpth.rita-code label "TY"
     v-job-no label "   Job #"
     rm-rdtlh.tag label "TAG#" FORMAT 'X(20)'
     rm-rdtlh.qty format "->>>>>9.99<<" label "QUANTITY"
     rm-rdtlh.loc label "WHSE"
     rm-rdtlh.loc-bin label "BIN"
     rm-rdtlh.loc2 column-label "WHSE! TO"
     rm-rdtlh.loc-bin2 column-label "BIN! TO"
     rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
     space(0)
     v-value label "VALUE"
     skip
    with frame itemy no-box down stream-io width 155.

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 150}

 v-fitem = begin_rm-no
 v-titem = end_rm-no
 v-fpcat = begin_procat
 v-tpcat = end_procat
 v-fdate = begin_date
 v-tdate = end_date
 v-floc  = begin_whs
 v-tloc  = end_whs
 v-type  = (if tb_receipts    then "R" else "") +
           (if tb_issues      then "I" else "") +
           (if tb_transfers   then "T" else "") +
           (if tb_adjustments then "A" else "") +
           (if tb_counts      then "C" else "")
 v-fjob  = FILL(" ", iJobLen - length(trim(begin_job-no))) +
           trim(begin_job-no) + string(int(begin_job-no2),"999")
 v-tjob  = FILL(" ", iJobLen - length(trim(end_job-no)))   +
           trim(end_job-no)   + string(int(end_job-no2),"999").

do with frame {&frame-name}:          
  do i = 1 to select-mat:num-items:
     if select-mat:is-selected(i) then
        v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.

  if length(TRIM(v-mtype)) NE 0 AND
     substr(v-mtype,length(trim(v-mtype)),1) eq "," then
     substr(v-mtype,length(trim(v-mtype)),1) = "".

  mat-types = v-mtype.

  do i = 1 to length(mat-types):
    if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.

  display mat-types.

  mat-types:HIDDEN = YES.
end.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

   EMPTY TEMP-TABLE tt-report.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(cFileName).
      IF tb_issue-detail THEN 
        EXPORT STREAM excel DELIMITER "," 
             "DATE"
             "ITEM"
             "DESCRIPTION"
             "P.O.#"
             "TY"
             "JOB #"
             "CUSTOMER"
             "FG ITEM #"
             "FG ITEM DESCRIPTION"
             "TAG#"
             "QUANTITY"
             "WHSE"
             "BIN"
             "WHSE TO"
             "BIN TO"
             "COST"
             "VALUE"
             SKIP.
      ELSE
          EXPORT STREAM excel DELIMITER "," 
             "DATE"
             "ITEM"
             "DESCRIPTION"
             "P.O.#"
             "TY"
             "JOB #"
             "TAG#"
             "QUANTITY"
             "WHSE"
             "BIN"
             "WHSE TO"
             "BIN TO"
             "COST"
             "VALUE"
             SKIP.

   END.

   display "" with frame r-top.

   ASSIGN
      v-type = caps(v-type)
      v-fjob1 = substr(v-fjob,1,iJobLen)
      v-tjob1 = substr(v-tjob,1,iJobLen)
      v-job1sub = INT(begin_job-no2).

   IF begin_job-no EQ end_job-no AND
      begin_job-no2 EQ end_job-no2 THEN
      FOR each rm-rdtlh WHERE
          rm-rdtlh.company EQ cocode AND
          rm-rdtlh.job-no  EQ v-fjob1 AND 
          rm-rdtlh.job-no2 EQ v-job1sub AND
          index(v-type,rm-rdtlh.rita-code) gt 0 AND
          rm-rdtlh.loc       ge v-floc AND
          rm-rdtlh.loc       le v-tloc
          no-lock,
          EACH rm-rcpth WHERE
               rm-rcpth.r-no EQ rm-rdtlh.r-no AND
               rm-rcpth.i-no ge v-fitem AND
               rm-rcpth.i-no le v-titem AND
               rm-rcpth.trans-date ge v-fdate AND
               rm-rcpth.trans-date le v-tdate
               NO-LOCK,
          first ITEM WHERE
                item.company eq cocode AND
                item.i-no    eq rm-rcpth.i-no AND
                item.procat  ge v-fpcat AND
                item.procat  le v-tpcat AND
                index(v-mtype,item.mat-type) gt 0
                NO-LOCK

          break by rm-rcpth.trans-date
              by rm-rcpth.rita-code:

       {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

          {rmrep\r-trans.i}
   END.
   ELSE IF begin_job-no EQ end_job-no THEN
      FOR each rm-rdtlh WHERE
          rm-rdtlh.company EQ cocode AND
          rm-rdtlh.job-no  EQ v-fjob1 AND 
          FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 ge v-fjob AND
          FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 le v-tjob AND
          index(v-type,rm-rdtlh.rita-code) gt 0 AND
          rm-rdtlh.loc       ge v-floc AND
          rm-rdtlh.loc       le v-tloc
          no-lock,
          EACH rm-rcpth WHERE
               rm-rcpth.r-no EQ rm-rdtlh.r-no AND
               rm-rcpth.i-no ge v-fitem AND
               rm-rcpth.i-no le v-titem AND
               rm-rcpth.trans-date ge v-fdate AND
               rm-rcpth.trans-date le v-tdate
               NO-LOCK,
          first ITEM WHERE
                item.company eq cocode AND
                item.i-no    eq rm-rcpth.i-no AND
                item.procat  ge v-fpcat AND
                item.procat  le v-tpcat AND
                index(v-mtype,item.mat-type) gt 0
                NO-LOCK

          break by rm-rcpth.trans-date
              by rm-rcpth.rita-code:

       {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

          {rmrep\r-trans.i}
   END.
   ELSE IF begin_rm-no EQ end_rm-no THEN
   for each rm-rcpth
        where rm-rcpth.company    eq cocode
          and rm-rcpth.i-no       EQ v-fitem
          and rm-rcpth.trans-date ge v-fdate
          and rm-rcpth.trans-date le v-tdate
          and index(v-type,rm-rcpth.rita-code) gt 0
        no-lock,
        each rm-rdtlh
        where rm-rdtlh.r-no      eq rm-rcpth.r-no
          and rm-rdtlh.rita-code eq rm-rcpth.rita-code
          and rm-rdtlh.loc       ge v-floc
          and rm-rdtlh.loc       le v-tloc
          and rm-rdtlh.job-no    ge v-fjob1 
          and rm-rdtlh.job-no    le v-tjob1
          and FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 ge v-fjob
          and FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 le v-tjob
        no-lock,
        first item
        where item.company eq cocode
          and item.i-no    eq rm-rcpth.i-no
          and item.procat  ge v-fpcat
          and item.procat  le v-tpcat
          and index(v-mtype,item.mat-type) gt 0
        no-lock

        break by rm-rcpth.trans-date
              by rm-rcpth.rita-code:

       {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

        {rmrep\r-trans.i}
   END.
   ELSE
   for each rm-rcpth
        where rm-rcpth.company    eq cocode
          and rm-rcpth.i-no       ge v-fitem
          and rm-rcpth.i-no       le v-titem
          and rm-rcpth.trans-date ge v-fdate
          and rm-rcpth.trans-date le v-tdate
          and index(v-type,rm-rcpth.rita-code) gt 0
        no-lock,
        each rm-rdtlh
        where rm-rdtlh.r-no      eq rm-rcpth.r-no
          and rm-rdtlh.rita-code eq rm-rcpth.rita-code
          and rm-rdtlh.loc       ge v-floc
          and rm-rdtlh.loc       le v-tloc
          and rm-rdtlh.job-no    ge v-fjob1 
          and rm-rdtlh.job-no    le v-tjob1
          and FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 ge v-fjob
          and FILL(" ", iJobLen - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                                 le v-tjob
        no-lock,
        first item
        where item.company eq cocode
          and item.i-no    eq rm-rcpth.i-no
          and item.procat  ge v-fpcat
          and item.procat  le v-tpcat
          and index(v-mtype,item.mat-type) gt 0
        no-lock

        break by rm-rcpth.trans-date
              by rm-rcpth.rita-code:

       {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

        {rmrep\r-trans.i}
   END.

   v-value = 0.

   for each tt-report where tt-report.term-id eq "",
       first account
       where account.company eq cocode
         and account.actnum  eq tt-report.key-01
       no-lock
       break by tt-report.key-01
       transaction:

     if first(tt-report.key-01) then page.

     v-value = v-value + dec(tt-report.key-02).

     if last-of(tt-report.key-01) then do:
       display account.actnum
               account.dscr
               v-value  label "Amount" (total)      format "->>,>>>,>>9.99"
           with stream-io width 132.

       v-value = 0.
     end.

     delete tt-report.
   END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
END.


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

