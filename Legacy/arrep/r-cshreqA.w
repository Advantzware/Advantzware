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

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
                                 FIELD due-date LIKE ar-inv.due-date.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date-1 begin_date-2 ~
begin_date-3 rd_sort rd_age tb_disc-date rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date-1 begin_date-2 begin_date-3 ~
lbl_sort rd_sort lbl_age rd_age tb_disc-date rd-dest lv-ornt lines-per-page ~
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

DEFINE VARIABLE begin_date-1 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 1" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 2" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-3 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Date 3" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cshreq.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_age AS CHARACTER FORMAT "X(256)":U INITIAL "Age By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_age AS CHARACTER INITIAL "Due Date" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Due Date", "Due Date",
"Avg Days", "Avg Days"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Name", "Customer Name",
"Due Date", "Due Date"
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.52.

DEFINE VARIABLE tb_disc-date AS LOGICAL INITIAL no 
     LABEL "Print Discount Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date-1 AT ROW 2.91 COL 39 COLON-ALIGNED HELP
          "Enter First Date"
     begin_date-2 AT ROW 4.1 COL 39 COLON-ALIGNED HELP
          "Enter the Second Date"
     begin_date-3 AT ROW 5.29 COL 39 COLON-ALIGNED HELP
          "Enter the Third Date"
     lbl_sort AT ROW 6.71 COL 25 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 6.76 COL 34 NO-LABEL
     lbl_age AT ROW 7.91 COL 22 COLON-ALIGNED NO-LABEL
     rd_age AT ROW 7.91 COL 34 NO-LABEL
     tb_disc-date AT ROW 9.1 COL 34
     rd-dest AT ROW 11.71 COL 5 NO-LABEL
     lv-ornt AT ROW 11.71 COL 31 NO-LABEL
     lines-per-page AT ROW 11.71 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 13.86 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14.81 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.95 COL 31
     tb_excel AT ROW 17.43 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.43 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 18.24 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20 COL 19
     btn-cancel AT ROW 20 COL 58
     "Cash Needed On:":U VIEW-AS TEXT
          SIZE 24 BY 1 AT ROW 1.71 COL 37
          BGCOLOR 8 FGCOLOR 9 FONT 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 2
     RECT-6 AT ROW 10.52 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1
         SIZE 94.4 BY 20.62.


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
         TITLE              = "AR Cash Requirements Report"
         HEIGHT             = 20.67
         WIDTH              = 95.6
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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_age IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_age:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_age".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_disc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* AR Cash Requirements Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AR Cash Requirements Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-1 C-Win
ON LEAVE OF begin_date-1 IN FRAME FRAME-A /* Date 1 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Date 2 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-3 C-Win
ON LEAVE OF begin_date-3 IN FRAME FRAME-A /* Date 3 */
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

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_date-1
                            &END_cust=begin_date-1
                            &fax-subject="Customer List"
                            &fax-body="Customer List"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject="Customer List"
                             &mail-body="Customer List"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= ''
                                  &END_cust=''
                                  &mail-subject="Customer List"
                                  &mail-body="Customer List"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
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


&Scoped-define SELF-NAME rd_age
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_age C-Win
ON VALUE-CHANGED OF rd_age IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_disc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disc-date C-Win
ON VALUE-CHANGED OF tb_disc-date IN FRAME FRAME-A /* Print Discount Date? */
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
   begin_date-1 = today
   begin_date-2 = begin_date-1 + 7
   begin_date-3 = begin_date-2 + 7.

  RUN enable_UI.

  {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_date-1.
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
  DISPLAY begin_date-1 begin_date-2 begin_date-3 lbl_sort rd_sort lbl_age rd_age 
          tb_disc-date rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date-1 begin_date-2 begin_date-3 rd_sort rd_age 
         tb_disc-date rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
 RUN custom/dprint.w (list-name).

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
/* --------------------------------------------------- ap/ap-flow.p 12/92 cd  */
/*                                                                            */
/* a/p - cash requirements report                                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

DEF VAR ws_gross LIKE ar-inv.net NO-UNDO.
DEF VAR d1 AS DATE EXTENT 3 FORMAT "99/99/9999" NO-UNDO.
DEF VAR ni AS INT NO-UNDO.
DEF VAR cust-t AS DEC EXTENT 4 FORMAT "->>>>>.99" NO-UNDO.
DEF VAR cust-d AS DEC EXTENT 4 FORMAT "->>>>.99" NO-UNDO.
DEF VAR inv-t AS DEC FORMAT "->>>>>>9.99" EXTENT 4 NO-UNDO.
DEF VAR inv-d LIKE cust-d NO-UNDO.
DEF VAR grand-t LIKE cust-t NO-UNDO.
DEF VAR grand-d LIKE cust-d NO-UNDO.
DEF VAR s AS INT NO-UNDO.
DEF VAR ag AS DEC NO-UNDO.
DEF VAR amt LIKE ag NO-UNDO.
DEF VAR t1 AS DEC FORMAT "$->>>,>>>.99" NO-UNDO.
DEF VAR c1 AS DEC FORMAT "$->>>,>>>.99" NO-UNDO.
DEF VAR m1 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR m2 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR m3 AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR first-time AS LOG INIT YES NO-UNDO.
DEF VAR ws_disc-avail AS DEC NO-UNDO COLUMN-LABEL "Disc"
    FORMAT '->>>>.99'.
DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-disc AS LOG INIT NO NO-UNDO.
DEF VAR v-disc-date AS DATE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

FORM HEADER
     d1[1] TO 47 d1[2] TO 69 d1[3] TO 91 "Beyond" TO 111 "Total" TO 135 SKIP
     "Invoice#  Inv Date/Due Date       Gross     Disc       Gross    "
     "Disc       Gross     Disc       Gross     Disc          Gross     Disc"
     SKIP
     FILL("_",135) FORMAT "x(135)"

    WITH PAGE-TOP FRAME f-top STREAM-IO WIDTH 135 NO-BOX.

FORM ar-inv.inv-no
     SPACE(4)
     ar-inv.inv-date        FORMAT "99/99/99"
     tt-report.due-date     FORMAT "99/99/99"
     inv-t[1]
     inv-d[1]               FORMAT "->>>>.99" 
     inv-t[2]
     inv-d[2]               FORMAT "->>>>.99" 
     inv-t[3]
     inv-d[3]               FORMAT "->>>>.99" 
     inv-t[4]
     inv-d[4]               FORMAT "->>>>.99" 
     ws_gross
     ws_disc-avail

    WITH STREAM-IO WIDTH 135 FRAME a NO-LABELS DOWN NO-BOX.


EMPTY TEMP-TABLE tt-report.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 d1[1]  = begin_date-1
 d1[2]  = begin_date-2
 d1[3]  = begin_date-3
 v-sort = SUBSTR(rd_sort,1,1)
 v-disc = tb_disc-date. 

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Cust#,Cust. Name,Invoice#,Inv Date,Due Date,"
              + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
              + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
              + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
              + "Beyond Gross,Beyond Disc,Gross,Total Disc".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH ar-inv
      WHERE ar-inv.company EQ cocode
        AND ar-inv.posted  EQ YES
        AND ar-inv.due     NE 0
      NO-LOCK,

      FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq ar-inv.cust-no
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

    CREATE tt-report.
    ASSIGN
     tt-report.due-date = IF rd_age EQ "Due Date" THEN ar-inv.due-date
                          ELSE (ar-inv.inv-date + cust.avg-pay)
     tt-report.key-01   = IF v-sort EQ "N" THEN cust.name
                          ELSE
                          IF v-sort EQ "D" THEN STRING((YEAR(tt-report.due-date) * 10000) +
                                                       (MONTH(tt-report.due-date) * 100)  +
                                                       DAY(tt-report.due-date))
                          ELSE ""
     tt-report.key-02  = cust.cust-no
     tt-report.key-03  = STRING(ar-inv.inv-no,"9999999999")
     tt-report.rec-id  = RECID(ar-inv).
  END.

  DISPLAY "" WITH FRAME r-top.
  DISPLAY "" WITH FRAME f-top.

  FOR EACH tt-report,
      FIRST ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK

      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03:

            {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-report.key-02) "}

    FIND FIRST terms WHERE terms.t-code EQ ar-inv.terms NO-LOCK NO-ERROR.

    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK NO-ERROR.

    IF FIRST-OF(tt-report.key-02) THEN DO:
      PUT ar-inv.cust-no.
      IF AVAIL cust THEN PUT cust.name.
      PUT SKIP.
    END.

    ASSIGN
     ws_gross      = ar-inv.due
     ws_disc-avail = IF ar-inv.net NE 0 THEN
                       (ar-inv.net * (ar-inv.disc-% / 100) - ar-inv.disc-taken)
                     ELSE 0.

    DO li = 1 to 4:
      ASSIGN
       inv-t[li] = 0
       inv-d[li] = 0.
    END.

    IF tt-report.due-date GT d1[3] THEN
      ASSIGN
       cust-t[4] = cust-t[4] + ws_gross
       inv-t[4]  = ws_gross.

    ELSE
    IF tt-report.due-date GT d1[2] THEN
      ASSIGN
       cust-t[3] = cust-t[3] + ws_gross
       inv-t[3]  = ws_gross.

    ELSE
    IF tt-report.due-date GT d1[1] THEN
      ASSIGN
       cust-t[2] = cust-t[2] + ws_gross
       inv-t[2]  = ws_gross.

    ELSE
      ASSIGN
       cust-t[1] = cust-t[1] + ws_gross
       inv-t[1]  = ws_gross.

    v-disc-date = IF AVAIL terms THEN (ar-inv.inv-date + terms.disc-days)
                                 ELSE tt-report.due-date.

    IF v-disc-date GT d1[3] THEN
      ASSIGN
       cust-d[4] = cust-d[4] + ws_disc-avail
       inv-d[4]  = ws_disc-avail.

    ELSE
    IF v-disc-date GT d1[2] THEN
      ASSIGN
       cust-d[3] = cust-d[3] + ws_disc-avail
       inv-d[3]  = ws_disc-avail.

    ELSE
    IF v-disc-date GT d1[1] THEN
      ASSIGN
       cust-d[2] = cust-d[2] + ws_disc-avail
       inv-d[2]  = ws_disc-avail.

    ELSE
      ASSIGN
       cust-d[1] = cust-d[1] + ws_disc-avail
       inv-d[1]  = ws_disc-avail.

    DISPLAY ar-inv.inv-no
            ar-inv.inv-date
            tt-report.due-date
              ar-inv.inv-date + ar-inv.disc-days
                WHEN ws_disc-avail NE 0 AND v-disc @ tt-report.due-date
            inv-t[1] WHEN inv-t[1] NE 0
            inv-d[1] WHEN inv-d[1] NE 0
            inv-t[2] WHEN inv-t[2] NE 0
            inv-d[2] WHEN inv-d[2] NE 0
            inv-t[3] WHEN inv-t[3] NE 0
            inv-d[3] WHEN inv-d[3] NE 0
            inv-t[4] WHEN inv-t[4] NE 0
            inv-d[4] WHEN inv-d[4] NE 0
            ws_gross
            ws_disc-avail

        WITH FRAME a.
    DOWN WITH FRAME a.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' IF FIRST-OF(tt-report.key-02) THEN ar-inv.cust-no
               ELSE ""                                             '",'
           '"' IF FIRST-OF(tt-report.key-02) AND AVAIL cust THEN
                  cust.NAME ELSE ""                                '",'
           '"' ar-inv.inv-no                                       '",'
           '"' ar-inv.inv-date                                     '",'
           '"' IF ws_disc-avail NE 0 AND v-disc THEN
                  STRING(ar-inv.inv-date + ar-inv.disc-days)
               ELSE STRING(tt-report.due-date)                     '",'
           '"' IF inv-t[1] NE 0 THEN STRING(inv-t[1],"->>>>>>9.99")
                  ELSE ""                                          '",'   
           '"' IF inv-d[1] NE 0 THEN STRING(inv-d[1],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[2] NE 0 THEN STRING(inv-t[2],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[2] NE 0 THEN STRING(inv-d[2],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[3] NE 0 THEN STRING(inv-t[3],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[3] NE 0 THEN STRING(inv-d[3],"->>>>.99")
                  ELSE ""                                          '",'
           '"' IF inv-t[4] NE 0 THEN STRING(inv-t[4],"->>>>>>9.99")
                  ELSE ""                                          '",'
           '"' IF inv-d[4] NE 0 THEN STRING(inv-d[4],"->>>>.99")
                  ELSE ""                                          '",'
           '"' STRING(ws_gross,"->>,>>>,>>9.99")                   '",'
           '"' STRING(ws_disc-avail,'->>>>.99')                    '",'
           SKIP.

    IF LAST-OF(tt-report.key-02) THEN DO:
      DISPLAY "       *" @ tt-report.due-date
              cust-t[1]  @ inv-t[1]
              cust-d[1]  @ inv-d[1]
              cust-t[2]  @ inv-t[2]
              cust-d[2]  @ inv-d[2]
              cust-t[3]  @ inv-t[3]
              cust-d[3]  @ inv-d[3]
              cust-t[4]  @ inv-t[4]
              cust-d[4]  @ inv-d[4]
              cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4] @ ws_gross
              cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4] @ ws_disc-avail

          WITH FRAME a.
      DOWN 2 WITH FRAME a.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' ""                                  '",'
             '"' "*"                                 '",'
             '"' STRING(cust-t[1],"->>>>>.99")     '",'   
             '"' STRING(cust-d[1],"->>>>.99")        '",'
             '"' STRING(cust-t[2],"->>>>>.99")     '",'
             '"' STRING(cust-d[2],"->>>>.99")        '",'
             '"' STRING(cust-t[3],"->>>>>.99")     '",'
             '"' STRING(cust-d[3],"->>>>.99")        '",'
             '"' STRING(cust-t[4],"->>>>>.99")     '",'
             '"' STRING(cust-d[4],"->>>>.99")        '",'
             '"' STRING(cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4],"->>,>>>,>>9.99")   '",'
             '"' STRING(cust-d[1] + cust-d[2] + cust-d[3] + cust-d[4],'->>>>.99')    '",'
             SKIP(1).

      DO li = 1 TO 4:
        ASSIGN
         grand-t[li] = grand-t[li] + cust-t[li]
         grand-d[li] = grand-d[li] + cust-d[li]

         cust-t[li]  = 0
         cust-d[li]  = 0.
      END.
    END.  /* last-of loop */

    IF LAST(tt-report.key-02) THEN DO:
      DOWN 1 WITH FRAME a.

      DISPLAY "      **" @ tt-report.due-date
              grand-t[1] @ inv-t[1]
              grand-d[1] @ inv-d[1]
              grand-t[2] @ inv-t[2]
              grand-d[2] @ inv-d[2]
              grand-t[3] @ inv-t[3]
              grand-d[3] @ inv-d[3]
              grand-t[4] @ inv-t[4]
              grand-d[4] @ inv-d[4]
              grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
              grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail

          WITH FRAME a.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' ""                                                  '",'
             '"' "**"                                                '",'
             '"' IF grand-t[1] NE 0 THEN STRING(grand-t[1],"->>>>>>9.99")
                    ELSE ""                                          '",'   
             '"' IF grand-d[1] NE 0 THEN STRING(grand-d[1],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[2] NE 0 THEN STRING(grand-t[2],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[2] NE 0 THEN STRING(grand-d[2],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[3] NE 0 THEN STRING(grand-t[3],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[3] NE 0 THEN STRING(grand-d[3],"->>>>.99")
                    ELSE ""                                          '",'
             '"' IF grand-t[4] NE 0 THEN STRING(grand-t[4],"->>>>>>9.99")
                    ELSE ""                                          '",'
             '"' IF grand-d[4] NE 0 THEN STRING(grand-d[4],"->>>>.99")
                    ELSE ""                                          '",'
             '"' STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->>,>>>,>>9.99")                   '",'
             '"' STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>.99')                    '",'
             SKIP.
    END.
  END. /* for EACH */

  IF tb_excel THEN DO:
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

