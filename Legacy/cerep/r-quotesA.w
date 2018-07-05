&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-quotes.w

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

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

/* gdm - 10130805 */
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust-no end_cust-no ~
begin_slsmn end_slsmn begin_date end_date rd_sort tb_cost tb_Include-Item ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_date end_date lbl-sort rd_sort tb_cost tb_Include-Item ~
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-quotes.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl-sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"SalesRep", "SalesRep"
     SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE VARIABLE tb_cost AS LOGICAL INITIAL no 
     LABEL "Break Out Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_Include-Item AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 3.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 3.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_date AT ROW 4.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Last Modify Date"
     end_date AT ROW 4.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Last Modify Date"
     lbl-sort AT ROW 6.48 COL 32 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 6.48 COL 44 NO-LABEL
     tb_cost AT ROW 7.67 COL 44
     tb_Include-Item AT ROW 8.81 COL 44 WIDGET-ID 2
     rd-dest AT ROW 11.71 COL 4 NO-LABEL
     lv-ornt AT ROW 12.19 COL 31 NO-LABEL
     lines-per-page AT ROW 12.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.62 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.14 COL 30
     tb_excel AT ROW 17.52 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.52 COL 73 RIGHT-ALIGNED
     fi_file AT ROW 18.71 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.05 COL 19
     btn-cancel AT ROW 21.05 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 3
     RECT-6 AT ROW 10.76 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Quotes List"
         HEIGHT             = 21.81
         WIDTH              = 95.8
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl-sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl-sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_Include-Item:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Quotes List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Quotes List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type=" "
                            &begin_cust="begin_cust-no"
                            &end_cust="begin_cust-no" 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust="begin_cust-no"
                             &end_cust="begin_cust-no"
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust="begin_cust-no"
                                  &end_cust="begin_cust-no"
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE. 

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


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Break Out Cost? */
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


&Scoped-define SELF-NAME tb_Include-Item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Include-Item C-Win
ON VALUE-CHANGED OF tb_Include-Item IN FRAME FRAME-A /* Include Inactive Items? */
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
    APPLY 'ENTRY' TO begin_cust-no.
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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_date end_date 
          lbl-sort rd_sort tb_cost tb_Include-Item rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn 
         begin_date end_date rd_sort tb_cost tb_Include-Item rd-dest lv-ornt 
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

     RUN custom/prntproc.p (list-name,int(lv-font-no),lv-ornt).


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
/* ------------------------------------------------- ce/rep/quote.p 07/97 JLF */
/*                                                                            */
/* QUOTE LIST PRINTOUT                                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

DEF VAR fcust like quote.cust-no.
DEF VAR tcust like fcust init "zzzzzzzz".
DEF VAR fsman like quote.sman.
DEF VAR tsman like fsman init "zzz".
DEF VAR fdate as date format "99/99/9999" init 01/01/01.
DEF VAR tdate like fdate init today.
DEF VAR v-srt as log format "Customer/SalesRep" init yes.

DEF VAR v-cst as log format "yes/no" init no.

DEF VAR v-cst-hdr as char format "x(60)".
DEF VAR v-lines as int.
DEF VAR i as int.
DEF VAR j as int.

DEF VAR v-misc  as char format "x" no-undo.
DEF VAR blk-dim as char.
DEF VAR v-cost  as dec format ">>>,>>9.99".
DEF VAR v-gp$   as dec format "->>,>>9.99".
DEF VAR v-gp%   as dec format "->>9.99".
DEF VAR v-dscr  like quoteitm.part-dscr1.
DEF VAR v-ext   as dec format ">>>,>>>,>>9.99" extent 2.
DEF VAR v-tot   as dec format ">>>,>>>,>>9.99" extent 4.
DEF VAR v-price as dec.
DEF VAR lv-q-qty AS INT EXTENT 2 NO-UNDO.

/* gdm - 10130805 */
DEF VAR v_quo-date AS CHAR         NO-UNDO.
DEF VAR v_sname    LIKE sman.sname NO-UNDO.

form space(1)
     quotehd.q-no
     quotehd.est-no             format "x(8)"
     quotehd.billto[1]          format "x(28)"
     v-dscr                     format "x(28)"
     quotehd.quo-date           format "99/99/99"
     space(1)
     sman.sname
     v-ext

header "Quote#     Est# Customer                     Part Description               Date   SalesRep                  Ext Price       Ext Cost"

   with frame quote no-labels no-attr-space stream-io width 150 down.



assign
 str-tit2 = "Quote List"
 {sys/inc/ctrtext.i str-tit2 112}

 fcust = begin_cust-no
 tcust = end_cust-no
 fsman = begin_slsmn
 tsman = end_slsmn
 fdate = begin_date
 tdate = end_date
 v-srt = rd_sort eq "Customer"
 v-cst = tb_cost.

EMPTY TEMP-TABLE tt-report.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display str-tit with frame r-top.

/* gdm - 10130805 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED
       "Quote#,Est#,Customer,Part Description,Date,SalesRep,Ext Price,Ext Cost,Qty,Price/M,Cost/M,GP$,GP%,"
        .    

    IF v-cst 
      THEN 
        PUT STREAM excel UNFORMATTED
           "Mat,DL,VO,FO,Misc,Charge,ChargeAmt" 
         SKIP.
      ELSE
        PUT STREAM excel UNFORMATTED
            "Misc,Charge,ChargeAmt"
            SKIP.

END.
SESSION:SET-WAIT-STATE ("general").



if v-cst then
  v-cst-hdr = "Costs --->        Mat           DL           VO           FO".

main:
FOR EACH quotehd
    WHERE quotehd.company  EQ cocode
      AND quotehd.loc      EQ locode
      AND quotehd.cust-no  GE fcust
      AND quotehd.cust-no  LE tcust
      AND quotehd.sman     GE fsman
      AND quotehd.sman     LE tsman
      AND quotehd.quo-date GE fdate
      AND quotehd.quo-date LE tdate
    NO-LOCK,

    FIRST quoteitm OF quotehd NO-LOCK,

    FIRST quoteqty OF quoteitm NO-LOCK:

     {custom/statusMsg.i " 'Processing Estimate#:  '  + quotehd.est-no  "}

  IF NOT tb_Include-Item THEN DO:
       FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ quoteitm.company
        AND itemfg.i-no EQ quoteitm.i-no NO-ERROR.

       IF AVAIL itemfg AND itemfg.stat EQ "I" THEN NEXT main.
  END.

  CREATE tt-report.
  ASSIGN
   tt-report.term-id = ""
   tt-report.key-01  = IF v-srt THEN quotehd.cust-no ELSE quotehd.sman
   tt-report.key-02  = STRING(quotehd.q-no,"9999999999")
   tt-report.rec-id  = RECID(quotehd).
END.

for each tt-report where tt-report.term-id eq "",
    first quotehd where recid(quotehd) eq tt-report.rec-id no-lock,
    first est
    where est.company eq quotehd.company
      and est.loc     eq quotehd.loc
      and est.est-no  eq quotehd.est-no
    no-lock

    break by tt-report.key-01
          by tt-report.key-02:

     {custom/statusMsg.i " 'Processing Estimate#:  '  + quotehd.est-no  "}

  IF FIRST-OF(tt-report.key-02) THEN lv-q-qty[1] = lv-q-qty[1] + 1.

  find first sman
      where sman.company eq quotehd.company
        and sman.sman    eq quotehd.sman
      no-lock no-error.

  /* gdm - 10130805 */
  ASSIGN v_sname = IF AVAIL sman THEN sman.sname ELSE "".

  for each quoteitm of quotehd  no-lock,

      each quoteqty of quoteitm no-lock

      break by quoteitm.part-no
            by quoteqty.qty

      with frame quote:

    IF quoteqty.uom EQ "M" THEN
      v-price = quoteqty.price.
    ELSE
      RUN sys/ref/convcuom.p (quoteqty.uom, "M", 0, 0, 0, 0,
                              quoteqty.price, OUTPUT v-price).

    v-cost = quoteqty.mat-cost + quoteqty.lab-cost +
             quoteqty.fo-cost  + quoteqty.vo-cost.

    if first-of(quoteitm.part-no) then do:
      assign
       v-ext[1] = v-price * (quoteqty.qty / 1000)
       v-ext[2] = v-cost  * (quoteqty.qty / 1000)

       v-tot[1] = v-tot[1] + v-ext[1]
       v-tot[2] = v-tot[2] + v-ext[2]

       v-dscr   = quoteitm.part-dscr1.

      if v-dscr eq "" then v-dscr = quoteitm.part-no.

      if not first(quoteitm.part-no) then put skip(1).

      display quotehd.q-no
              quotehd.est-no
              quotehd.billto[1]
              v-dscr
              quotehd.quo-date
              sman.sname when avail sman
              v-ext.

      put skip(1)
          "    Qty     Price/M      Cost/M         GP$      GP%"
          space(5)
          v-cst-hdr
          skip.

    end.

    assign
     v-gp$ = v-price - v-cost
     v-gp% = v-gp$ / v-price * 100.

    if v-gp% eq ? then v-gp% = 0.

    put quoteqty.qty
        space(2)
        v-price  format ">>>,>>9.99"
        space(2)
        v-cost
        space(2)
        v-gp$
        space(2)
        v-gp%
        space(15).

    if v-cst then
      put quoteqty.mat-cost
          space(2)
          quoteqty.lab-cost
          space(2)
          quoteqty.vo-cost
          space(2)
          quoteqty.fo-cost.

    put skip.

    /* gdm - 10130805 */
    IF tb_excel THEN DO:

        IF FIRST-OF(quoteitm.part-no) 
          THEN 
            PUT STREAM excel UNFORMATTED
              '"' quotehd.q-no      '",'
              '"' quotehd.est-no    '",'
              '"' quotehd.billto[1] '",'
              '"' v-dscr            '",'
              '"' quotehd.quo-date  '",'
              '"' v_sname           '",'
              '"' v-ext[1]          '",'
              '"' v-ext[2]          '",'
              '"' quoteqty.qty      '",'
              '"' STRING(ROUND(v-price,10),'->>,>>9.99') '",'
              '"' STRING(ROUND(v-cost,10),'->>,>>9.99')  '",'
              '"' STRING(ROUND(v-gp$,10),'->>,>>9.99')   '",'
              '"' STRING(ROUND(v-gp%,10),'->>,>>9.99')   '",'.

          ELSE
            PUT STREAM excel UNFORMATTED
              ',,,,,,,,'
              '"' quoteqty.qty      '",'
              '"' STRING(ROUND(v-price,10),'->>,>>9.99') '",'
              '"' STRING(ROUND(v-cost,10),'->>,>>9.99')  '",'
              '"' STRING(ROUND(v-gp$,10),'->>,>>9.99')   '",'
              '"' STRING(ROUND(v-gp%,10),'->>,>>9.99')   '",'.

        IF v-cst 
          THEN 
            PUT STREAM excel UNFORMATTED
              '"' quoteqty.mat-cost '",'
              '"' quoteqty.lab-cost '",'
              '"' quoteqty.vo-cost  '",'
              '"' quoteqty.fo-cost  '",' .        

    END. /* if excel */

    /* gdm - 10130805 */
    FIND FIRST quotechg NO-LOCK 
        WHERE quotechg.company EQ quoteqty.company
          AND quotechg.loc     EQ quoteqty.loc
          AND quotechg.q-no    EQ quoteqty.q-no
          AND quotechg.line    EQ quoteqty.line
          AND quotechg.qty     EQ quoteqty.qty NO-ERROR.
    IF NOT AVAIL quotechg THEN 
        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED SKIP.


    for each quotechg no-lock
        where quotechg.company eq quoteqty.company
          and quotechg.loc eq quoteqty.loc
          and quotechg.q-no eq quoteqty.q-no
          and quotechg.line eq quoteqty.line
          and quotechg.qty eq quoteqty.qty
        break by quotechg.charge:

      if first(quotechg.charge) then put skip(1).

      if (quotechg.labf ne 0 or  quotechg.labm ne 0) and
         (quotechg.matf eq 0 and quotechg.matm eq 0) then
        v-misc = "L".
      else
      if (quotechg.labf eq 0 and quotechg.labm eq 0) and
         (quotechg.matf ne 0 or  quotechg.matm ne 0) then
        v-misc = "M".
      else
      if quotechg.labf ne 0 or quotechg.labm ne 0 or
         quotechg.matf ne 0 or quotechg.matm ne 0 then
        v-misc = "T".
      else
        v-misc = "".

      put space(40)
          v-misc
          quotechg.charge
          space(43)
          quotechg.amt
          skip.

      /* gdm - 10130805 */
      IF tb_excel THEN DO:

          IF FIRST(quotechg.charge) THEN DO:
              PUT STREAM excel UNFORMATTED
                  '"' v-misc          '",'
                  '"' quotechg.charge '",'
                  '"' quotechg.amt    '"'
                  SKIP.
          END.
          ELSE DO:

              IF v-cst 
                THEN 
                  PUT STREAM excel UNFORMATTED
                    ',,,,,,,,,,,,,,,,,'                   
                    '"' v-misc          '",'
                    '"' quotechg.charge '",'
                    '"' quotechg.amt    '"'
                   SKIP.
                ELSE 
                  PUT STREAM excel UNFORMATTED
                    ',,,,,,,,,,,,,'
                    '"' v-misc          '",'
                    '"' quotechg.charge '",'
                    '"' quotechg.amt    '"'
                   SKIP.
          END.
      END.

      if last(quotechg.charge) then put skip(1). 

    end.

  end.

  put fill("-",133) format "x(133)"
      skip.

  if last-of(tt-report.key-01) then do:
    put fill("-",133) format "x(133)"
        skip.

    if v-srt then put "Customer Totals".
    else put "SalesRep Totals".

    put space(66)
        "# of Quotes:"
        SPACE(1)
        TRIM(STRING(lv-q-qty[1],">,>>>,>>9")) FORMAT "x(9)"
        SPACE(1)
        v-tot[1]
        space(1)
        v-tot[2]
        skip
        fill("-",133) format "x(133)"
        skip
        fill("-",133) format "x(133)"
        skip.

    assign
     v-tot[3]    = v-tot[3] + v-tot[1]
     v-tot[4]    = v-tot[4] + v-tot[2]
     lv-q-qty[2] = lv-q-qty[2] + lv-q-qty[1]

     v-tot[1]    = 0
     v-tot[2]    = 0
     lv-q-qty[1] = 0.
  end.

  IF LAST(tt-report.key-01) THEN
    PUT SKIP(2)
        "Grand Totals"
        SPACE(69)
        "# of Quotes:"
        SPACE(1)
        TRIM(STRING(lv-q-qty[2],">,>>>,>>9")) FORMAT "x(9)"
        SPACE(1)
        v-tot[3]
        SPACE(1)
        v-tot[4].

  delete tt-report.
end.

IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */

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

