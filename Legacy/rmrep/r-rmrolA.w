&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rmroll.w

  Description: Roll Stock Cost - Paper / Board List

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

def var fco as ch.
def var tco like fco.
def var floc as ch.
def var tloc like floc.
def var fcat as ch initial "000000".
def var tcat like fcat initial "ZZZZZZ".
def var doe    as logical initial true.
def var dor    as logical initial true.
def var detail as logical initial false.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-roll-multp AS DEC DECIMALS 4 NO-UNDO.
DEF STREAM excel.

{sys/form/r-topw.f}

DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_procat end_procat ~
begin_rm-no end_rm-no tb_category tb_po-cost tb_cost-msf tb_sub-category ~
tb_lf-comm tb_gt rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_procat end_procat begin_rm-no ~
end_rm-no tb_category tb_po-cost tb_cost-msf tb_sub-category tb_lf-comm ~
tb_gt rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
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

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-rmroll.csv" 
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
     SIZE 96 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 7.86.

DEFINE VARIABLE tb_category AS LOGICAL INITIAL no 
     LABEL "Print Category?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cost-msf AS LOGICAL INITIAL no 
     LABEL "Print Cost/MSF?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_gt AS LOGICAL INITIAL no 
     LABEL "Print Grand Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_lf-comm AS LOGICAL INITIAL no 
     LABEL "Print LF Committed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_po-cost AS LOGICAL INITIAL no 
     LABEL "Print Last PO Cost for Cost/MSF?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sub-category AS LOGICAL INITIAL no 
     LABEL "Print Subtotal By Category?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.6 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_procat AT ROW 3.1 COL 29 COLON-ALIGNED
     end_procat AT ROW 3.1 COL 67 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_rm-no AT ROW 4.24 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 4.24 COL 67 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     tb_category AT ROW 5.48 COL 16.8
     tb_po-cost AT ROW 5.48 COL 51.4
     tb_cost-msf AT ROW 6.43 COL 16.8
     tb_sub-category AT ROW 6.43 COL 51.4
     tb_lf-comm AT ROW 7.38 COL 16.8
     tb_gt AT ROW 7.38 COL 51.4
     rd-dest AT ROW 10.67 COL 6 NO-LABEL
     lv-ornt AT ROW 10.91 COL 31 NO-LABEL
     lines-per-page AT ROW 10.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.05 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.05 COL 31
     tb_excel AT ROW 16.38 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 16.38 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 17.19 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.91 COL 18
     btn-cancel AT ROW 18.91 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.48 COL 3
     RECT-6 AT ROW 9.1 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 24.57.


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
         TITLE              = "Roll Stock Cost - Paper/Board List"
         HEIGHT             = 19.57
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_category:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cost-msf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_gt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_lf-comm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_po-cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sub-category:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Roll Stock Cost - Paper/Board List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Roll Stock Cost - Paper/Board List */
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
           {custom/asifax.i &type= 'Roll Stock Cost - Paper/Board List'
                            &begin_cust= "begin_procat"
                            &END_cust= "end_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Roll Stock Cost - Paper/Board List"
                             &begin_cust= "begin_procat"
                             &END_cust= "end_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Roll Stock Cost - Paper/Board List"
                                  &begin_cust="end_procat"
                                  &END_cust="begin_procat"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
           END.
       END.
       WHEN 6 THEN RUN output-to-port.

  end case. 

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


&Scoped-define SELF-NAME tb_category
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_category C-Win
ON VALUE-CHANGED OF tb_category IN FRAME FRAME-A /* Print Category? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost-msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost-msf C-Win
ON VALUE-CHANGED OF tb_cost-msf IN FRAME FRAME-A /* Print Cost/MSF? */
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


&Scoped-define SELF-NAME tb_gt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_gt C-Win
ON VALUE-CHANGED OF tb_gt IN FRAME FRAME-A /* Print Grand Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_lf-comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_lf-comm C-Win
ON VALUE-CHANGED OF tb_lf-comm IN FRAME FRAME-A /* Print LF Committed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_po-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_po-cost C-Win
ON VALUE-CHANGED OF tb_po-cost IN FRAME FRAME-A /* Print Last PO Cost for Cost/MSF? */
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


&Scoped-define SELF-NAME tb_sub-category
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sub-category C-Win
ON VALUE-CHANGED OF tb_sub-category IN FRAME FRAME-A /* Print Subtotal By Category? */
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

  FIND FIRST uom WHERE
       uom.uom = "ROLL"
       NO-LOCK NO-ERROR.

  IF AVAIL uom THEN
  DO:
     v-roll-multp = uom.mult.
     RELEASE uom.
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
  DISPLAY begin_procat end_procat begin_rm-no end_rm-no tb_category tb_po-cost 
          tb_cost-msf tb_sub-category tb_lf-comm tb_gt rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_procat end_procat begin_rm-no end_rm-no 
         tb_category tb_po-cost tb_cost-msf tb_sub-category tb_lf-comm tb_gt 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-board C-Win 
PROCEDURE run-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var rm-cst-amt like item.last-cost NO-UNDO.
DEF VAR rm-cst-amt-lf LIKE ITEM.last-cost NO-UNDO.
def var v-printed as log init no no-undo.
DEF VAR v-qonh LIKE item.q-onh NO-UNDO.
DEF VAR v-qono LIKE item.q-ono NO-UNDO.
DEF VAR v-qcomm LIKE item.q-comm NO-UNDO.
DEF VAR v-value-oh AS DEC DECIMALS 2 NO-UNDO.
DEF VAR v-rolls-oh AS INT NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-wid AS DEC DECIMALS 5 NO-UNDO.
DEF VAR v-rolls-on-order AS INT NO-UNDO.
DEF VAR lv-rm-qty LIKE rm-bin.qty NO-UNDO.

DEF VAR sub-lf-oh AS DEC NO-UNDO.
DEF VAR sub-lf-ono AS DEC NO-UNDO.
DEF VAR sub-lf-comm AS DEC NO-UNDO.
DEF VAR sub-amt-oh AS DEC NO-UNDO.
DEF VAR sub-rolls-oh AS DEC NO-UNDO.
DEF VAR sub-rolls-on-order AS DEC NO-UNDO.

DEF VAR gt-lf-oh AS DEC NO-UNDO.
DEF VAR gt-lf-ono AS DEC NO-UNDO.
DEF VAR gt-lf-comm AS DEC NO-UNDO.
DEF VAR gt-amt-oh AS DEC NO-UNDO.
DEF VAR gt-rolls-oh AS DEC NO-UNDO.
DEF VAR gt-rolls-on-order AS DEC NO-UNDO.


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "CAT,ITEM,DESCRIPTION,Rolls on Hand,COST/MSF,LF On Hand,LF Committed,Rolls on Order,LF On Order,$ On Hand".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

form
    item.procat
    item.i-no
    item.i-name    format "x(27)"
    v-rolls-oh FORMAT ">>,>>>,>>9"
    rm-cst-amt FORMAT "->>>,>>9.9999"
    v-qonh     format "->>>,>>>,>>9"
    v-qcomm    format "->>>,>>>,>>9"
    SPACE(6)
    v-rolls-on-order FORMAT ">>,>>>,>>9"
    v-qono     format "->>>,>>>,>>9"
    v-value-oh FORMAT "->,>>>,>>9.99"
    skip
header
"CAT   ITEM       DESCRIPTION                   Rolls OH      COST/MSF   LF On Hand LF Committed  Rolls on Order  LF On Order     $ On Hand"
    with frame itemx no-box no-labels down stream-io width 140.

SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}.

      display "" with frame r-top.

      find first rm-ctrl where rm-ctrl.company = cocode NO-LOCK.

      for each ITEM WHERE
          item.company = cocode and
          item.loc = locode and
          ITEM.i-no >= begin_rm-no AND
          ITEM.i-no <= end_rm-no AND
          item.i-code = "R" and
          item.procat >= fcat and
          item.procat <= tcat AND
          (item.mat-type = "B" or item.mat-type = "P")
          NO-LOCK
          break by item.company by item.loc by item.i-code
          by item.mat-type by item.procat by item.i-no with frame itemx:

          {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"} 

          clear frame itemx all NO-PAUSE.

          v-printed = YES.

          IF NOT tb_po-cost THEN
          DO:
             if rm-ctrl.avg-lst-cst THEN
                rm-cst-amt = item.avg-cost.
             ELSE
                rm-cst-amt = item.last-cost.
          END.
          ELSE
          DO:
             FIND LAST rm-rcpth WHERE
                  rm-rcpth.company EQ cocode AND
                  rm-rcpth.i-no EQ ITEM.i-no AND
                  rm-rcpth.rita-code EQ "R" AND
                  rm-rcpth.po-no NE ""
                  NO-LOCK NO-ERROR.

             IF AVAIL rm-rcpth THEN
             DO:
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ cocode AND
                     po-ordl.i-no  EQ ITEM.i-no AND
                     po-ordl.po-no EQ INT(rm-rcpth.po-no)
                     NO-LOCK NO-ERROR.

                IF AVAIL po-ordl THEN
                DO:
                   rm-cst-amt = po-ordl.cost.

                   IF po-ordl.pr-uom NE item.cons-uom THEN
                      RUN sys/ref/convcuom.p(po-ordl.pr-uom, ITEM.cons-uom, item.basis-w,
                                   (if item.r-wid eq 0 THEN item.s-len
                                                       ELSE 12),
                                   (if item.r-wid eq 0 THEN item.s-wid
                                                       ELSE item.r-wid),
                                   item.s-dep,
                                   rm-cst-amt, OUTPUT rm-cst-amt).

                   RELEASE po-ordl.
                END.

                ELSE rm-cst-amt = 0.

                RELEASE rm-rcpth.
             END.
             ELSE
                rm-cst-amt = 0.
          END.

          IF ITEM.cons-uom NE "LF" THEN
          DO:
             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                 (if item.r-wid eq 0 THEN item.s-len
                                                     ELSE 12),
                                 (if item.r-wid eq 0 THEN item.s-wid
                                                     ELSE item.r-wid),
                                 item.s-dep,                    
                                 item.q-onh, OUTPUT v-qonh).

             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                 (if item.r-wid eq 0 THEN item.s-len
                                                     ELSE 12),
                                 (if item.r-wid eq 0 THEN item.s-wid
                                                     ELSE item.r-wid),
                                 item.s-dep,                    
                                 item.q-ono, OUTPUT v-qono).

             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                 (if item.r-wid eq 0 THEN item.s-len
                                                     ELSE 12),
                                 (if item.r-wid eq 0 THEN item.s-wid
                                                     ELSE item.r-wid),
                                 item.s-dep,                    
                                 item.q-comm, OUTPUT v-qcomm).

             RUN sys/ref/convcuom.p(ITEM.cons-uom, "LF", item.basis-w,
                                (if item.r-wid eq 0 THEN item.s-len
                                                    ELSE 12),
                                (if item.r-wid eq 0 THEN item.s-wid
                                                    ELSE item.r-wid),
                                item.s-dep,
                                rm-cst-amt, OUTPUT rm-cst-amt-lf).

          END.
          ELSE
             ASSIGN
                v-qonh = item.q-onh
                v-qono = item.q-ono
                v-qcomm = item.q-comm
                rm-cst-amt-lf = rm-cst-amt.

          ASSIGN
             v-value-oh = 0
             v-rolls-oh = 0
             v-rolls-on-order = 0.

          IF ITEM.cons-uom NE "MSF" THEN
             rm-cst-amt = IF v-qonh NE 0 THEN
                             (rm-cst-amt-lf * v-qonh * 1000) /
                             (((IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE
                                ITEM.s-wid) / 12.0) * v-qonh)
                          ELSE 0.


          FOR EACH rm-bin FIELDS(qty cost tag) WHERE
              rm-bin.company = item.company AND
              rm-bin.i-no = item.i-no AND
              rm-bin.i-no NE " "
              NO-LOCK:

              {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"} 

              v-value-oh = v-value-oh + (rm-bin.qty * rm-bin.cost).

              IF /*item.mat-type = "P" AND*/ ITEM.r-wid > 0 THEN
              DO:
                 IF rm-bin.tag NE "" AND rm-bin.qty <> 0 THEN
                    v-rolls-oh = v-rolls-oh + 1.
                 ELSE
                 DO:
                    IF ITEM.cons-uom NE "LF" THEN
                       RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                              12,
                                              item.r-wid,
                                              item.s-dep,                    
                                              rm-bin.qty, OUTPUT lv-rm-qty).
                    ELSE
                       lv-rm-qty = rm-bin.qty.

                    IF ITEM.s-len NE 0 THEN
                    DO:                
                       lv-rm-qty = lv-rm-qty / ITEM.s-len.
                       {sys/inc/roundup.i lv-rm-qty}
                       v-rolls-oh = v-rolls-oh + lv-rm-qty.
                    END.
                    ELSE
                    IF v-roll-multp NE 0 THEN
                    DO:
                       lv-rm-qty = lv-rm-qty / v-roll-multp.
                       {sys/inc/roundup.i lv-rm-qty}
                       v-rolls-oh = v-rolls-oh + lv-rm-qty.
                    END.
                 END.
              END.
          END.

          IF item.mat-type = "P" AND ITEM.r-wid > 0 THEN
          DO:
             IF ITEM.s-len NE 0 THEN
             DO:                             
                v-rolls-on-order = v-qono / ITEM.s-len.
                {sys/inc/roundup.i v-rolls-on-order}
             END.
             ELSE
             IF v-roll-multp NE 0 THEN
             DO:
                v-rolls-on-order = v-qono / v-roll-multp.
                {sys/inc/roundup.i v-rolls-on-order}
             END.
          END.

          IF tb_sub-category OR tb_gt THEN
             ASSIGN
               sub-lf-oh = sub-lf-oh + v-qonh
               sub-lf-ono = sub-lf-ono + v-qono
               sub-lf-comm = sub-lf-comm + v-qcomm
               sub-amt-oh = sub-amt-oh + v-value-oh
               sub-rolls-oh = sub-rolls-oh + v-rolls-oh
               sub-rolls-on-order = sub-rolls-on-order + v-rolls-on-order.

          display
             item.procat  when first-of(item.procat) AND tb_category
             item.i-no
             item.i-name
             v-rolls-oh
             rm-cst-amt WHEN tb_cost-msf
             v-qonh
             v-qcomm WHEN tb_lf-comm
             SPACE(6)
             v-rolls-on-order
             v-qono
             v-value-oh.
          down.

          IF LAST-OF(ITEM.procat) THEN
          DO:
             IF tb_sub-category AND tb_category THEN
             DO:
                PUT SKIP(1).

                display
                item.procat
                "Sub Totals:"   @ ITEM.i-name
                sub-rolls-oh  @ v-rolls-oh
                sub-lf-oh     @ v-qonh
                sub-lf-comm   WHEN tb_lf-comm @ v-qcomm
                SPACE(6)
                sub-rolls-on-order @ v-rolls-on-order
                sub-lf-ono @ v-qono
                sub-amt-oh @ v-value-oh.

                down.

                PUT SKIP(1).
             END.

             ASSIGN 
                gt-lf-oh = gt-lf-oh + sub-lf-oh
                gt-lf-ono = gt-lf-ono + sub-lf-ono
                gt-lf-comm = gt-lf-comm + sub-lf-comm
                gt-amt-oh = gt-amt-oh + sub-amt-oh
                gt-rolls-oh = gt-rolls-oh + sub-rolls-oh
                gt-rolls-on-order = gt-rolls-on-order + sub-rolls-on-order
                sub-lf-oh = 0
                sub-lf-ono = 0
                sub-lf-comm = 0
                sub-amt-oh = 0
                sub-rolls-oh = 0
                sub-rolls-on-order = 0.
          END.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' IF FIRST-OF(item.procat) AND tb_category THEN
                        item.procat ELSE ""                '",'
                 '"' REPLACE(ITEM.i-no,'"',"")             '",'
                 '"' REPLACE(ITEM.i-name,'"',"")           '",'
                 '"' STRING(v-rolls-oh,">>,>>>,>>9")     '",'
                 '"' IF tb_cost-msf THEN
                        STRING(rm-cst-amt,"->>>,>>9.9999")
                     ELSE ""                               '",'
                 '"' STRING(v-qonh,"->>>,>>>,>>9")         '",'
                 '"' IF tb_lf-comm THEN
                        STRING(v-qcomm,"->>>,>>>,>>9")
                     ELSE ""                               '",'
                 '"' STRING(v-rolls-on-order,">>,>>>,>>9") '",'
                 '"' STRING(v-qono,"->>>,>>>,>>9")         '",'
                 '"' STRING(v-value-oh,"->>>,>>>,>>9.99")  '",'
                 SKIP.
      end. /*each item*/

IF tb_gt THEN
DO:
   PUT SKIP(1).
   display
   "Grand Totals:"   @ ITEM.i-name
   gt-rolls-oh  @ v-rolls-oh
   gt-lf-oh     @ v-qonh
   gt-lf-comm   WHEN tb_lf-comm @ v-qcomm
   SPACE(6)
   gt-rolls-on-order @ v-rolls-on-order
   gt-lf-ono @ v-qono
   gt-amt-oh @ v-value-oh WITH FRAME itemx.
   down.
   PUT SKIP(1).
END.

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

assign
 fco    = cocode
 tco    = cocode
 floc   = locode
 tloc   = locode
 fcat   = begin_procat
 tcat   = end_procat.

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

