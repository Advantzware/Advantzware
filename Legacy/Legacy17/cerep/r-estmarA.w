&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-estmar.w

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

DEF STREAM st-excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
tb_excel tb_runExcel fi_file rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
tb_excel tb_runExcel fi_file rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

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
     LABEL "Beginning Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Estimate#" 
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
     LABEL "Ending Add Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date-2 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-estmar.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1.

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
          "To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 7.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 11.43.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 3.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 4.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_est AT ROW 5.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Estimate"
     end_est AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Estimate"
     begin_date AT ROW 6 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date-2 AT ROW 6.95 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date-2 AT ROW 6.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     tb_excel AT ROW 9.1 COL 43 RIGHT-ALIGNED
     tb_runExcel AT ROW 9.1 COL 73 RIGHT-ALIGNED
     fi_file AT ROW 10.29 COL 21 COLON-ALIGNED HELP
          "Enter File Name"
     rd-dest AT ROW 13.38 COL 4 NO-LABEL
     lv-ornt AT ROW 14.33 COL 31 NO-LABEL
     lines-per-page AT ROW 14.33 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.76 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.71 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.14 COL 30
     btn-ok AT ROW 21 COL 26
     btn-cancel AT ROW 21 COL 56
     RECT-6 AT ROW 12.43 COL 1
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 22.24.


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
         TITLE              = "Estimates List w/Margins"
         HEIGHT             = 22.62
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       begin_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_date-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       end_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Estimates List w/Margins */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Estimates List w/Margins */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Add Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Beginning Mod Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     DEF VAR char-val AS cha NO-UNDO.
     DEF var lv-eb-tmpid as recid no-undo.

     run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).

     if char-val <> "" then do:                 
            FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                begin_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
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
    ASSIGN {&displayed-objects}.
  END.

  RUN run-report.
  STATUS DEFAULT "Processing Complete".

  IF tb_excel THEN DO:
    IF tb_runExcel THEN
    OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
  END. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type=" "
                            &begin_cust="begin_cust-no"
                            &end_cust="begin_cust-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust="begin_cust-no"
                             &end_cust="begin_cust-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust="begin_cust-no"
                                  &end_cust="begin_cust-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Add Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-2 C-Win
ON LEAVE OF end_date-2 IN FRAME FRAME-A /* Ending Mod Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     DEF VAR char-val AS cha NO-UNDO.
     DEF var lv-eb-tmpid as recid no-undo.

     run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).

     if char-val <> "" then do:                 
            FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                  end_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
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
ON HELP OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-file ls-filename 
                 title "Select File to Save "
                 filters "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 initial-dir "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
          begin_date end_date begin_date-2 end_date-2 tb_excel tb_runExcel 
          fi_file rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
         begin_date end_date begin_date-2 end_date-2 tb_excel tb_runExcel 
         fi_file rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
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
 /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                          /* font #*/ /* use-dialog(1) and landscape(2) */
                          */
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
{sys/form/r-topw.f}

DEF VAR fest LIKE est.est-no NO-UNDO.
DEF VAR test LIKE fest       NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-box-size LIKE quoteitm.size NO-UNDO.
DEF VAR lv-die-size LIKE quoteitm.size NO-UNDO.
DEF VAR lv-format AS CHAR NO-UNDO.
DEF VAR li-colors AS INT NO-UNDO.
DEF VAR li-qty LIKE probe.est-qty NO-UNDO.
DEF VAR ld-costm LIKE probe.full-cost NO-UNDO.
DEF VAR ld-costt AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR ld-price LIKE probe.sell-price NO-UNDO.
DEF VAR ld-mar AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 7 NO-UNDO.
DEF VAR ld-pct AS DEC NO-UNDO.
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO.




ASSIGN
 str-tit2 = TRIM(c-win:TITLE) + ""
 {sys/inc/ctrtext.i str-tit2 112}

 fest = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

{sys/inc/print1.i}

{sys/inc/outprint.i  VALUE(lines-per-page)}

IF tb_excel THEN do:
  OUTPUT STREAM st-excel TO VALUE(fi_file).
  PUT STREAM st-excel UNFORMATTED
      ",,,,Carton,Die,,,Carton,,,,,,FOB Variable Profit Margins,,,,,," +
      ",Total,Total,Total,Delivered"
      SKIP
      "Est#,Item Description,Bd,Cal,Size,Size,#Up,# of Col,Quantity,F" +
      "OB Cost/M,FOB Total Cost,15%,20%,25%,30%,35%,40%,45%,FOB Price" +
      "/M,FOB Gross Rev.,FOB Gross Mar.,Plate Cost,Die Cost,Freight C" +
      "ost,Selling Price"
      SKIP.
END.
SESSION:SET-WAIT-STATE ("general").

IF td-show-parm THEN RUN show-param.

VIEW FRAME r-top.

FOR EACH est
    WHERE est.company  EQ cocode
      AND est.est-no   GE fest
      AND est.est-no   LE test
      AND est.est-date GE begin_date
      AND est.est-date LE end_date
      AND est.mod-date GE begin_date-2
      AND est.mod-date LE end_date-2
    NO-LOCK,

    FIRST est-qty
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    NO-LOCK,

    EACH eb
    WHERE eb.company  EQ est.company
      AND eb.est-no   EQ est.est-no
      AND eb.cust-no  GE begin_cust-no
      AND eb.cust-no  LE end_cust-no
      AND eb.sman     GE begin_slsmn
      AND eb.sman     LE end_slsmn
      AND (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))
    NO-LOCK,

    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK,

    EACH probe
    WHERE probe.company   EQ est.company
      AND probe.est-no    EQ est.est-no
      AND probe.full-cost NE ?
    NO-LOCK

    BREAK BY est.est-no
          BY probe.est-qty
          BY probe.probe-date
          BY probe.probe-time:

    {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}

  IF LAST-OF(probe.est-qty) THEN DO:
    RUN sys/inc/calcsize.p (ROWID(eb), OUTPUT lv-box-size).

    li-colors = 0.

    IF est.est-type LE 4 THEN DO:
      ASSIGN
       lv-format   = ">>>>>9.9<<<<"
       lv-die-size = TRIM(STRING(ef.trim-w,lv-format)) + " x " +
                     TRIM(STRING(ef.trim-l,lv-format)).

      DO li = 1 TO EXTENT(eb.i-code2):
        IF TRIM(eb.i-code2[li]) NE "" THEN li-colors = li-colors + 1.
      END.
    END.

    ELSE DO:
      ASSIGN
       lv-format   = ">>>>9.99"
       lv-die-size = TRIM(STRING({sys/inc/k16v.i ef.trim-w},lv-format)) + " x " +
                     TRIM(STRING({sys/inc/k16v.i ef.trim-l},lv-format)).

      DO li = 1 TO EXTENT(eb.i-code):
        IF TRIM(eb.i-code[li]) NE "" THEN li-colors = li-colors + 1.
      END.
    END.

    RELEASE probeit.
    IF est.est-type EQ 3 OR est.est-type EQ 4 OR
       est.est-type EQ 7 OR est.est-type EQ 8 THEN
      FIND FIRST probeit NO-LOCK
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.line
            AND probeit.part-no EQ eb.part-no
          NO-ERROR.

    IF AVAIL probeit THEN
      ASSIGN
       li-qty   = IF probeit.yrprice THEN probeit.yld-qty ELSE probeit.bl-qty
       ld-costm = probeit.full-cost
       ld-price = probeit.sell-price.
    ELSE
      ASSIGN
       li-qty   = probe.est-qty
       ld-costm = probe.full-cost
       ld-price = probe.sell-price.

    ld-costt = li-qty / 1000 * ld-costm.

    ld-pct = .85.
    DO li = 1 TO EXTENT(ld-mar):
      ASSIGN
       ld-mar[li] = (ld-costt / ld-pct * 1.01) - ld-costt
       ld-pct     = ld-pct - .05.
    END.

    DISPLAY TRIM(eb.est-no)       FORMAT "x(8)"
                                  COLUMN-LABEL "Est#"
            eb.part-dscr1         COLUMN-LABEL "Item Description"
            ef.board              COLUMN-LABEL "Bd"
            ef.cal                COLUMN-LABEL "Cal"
            lv-box-size           COLUMN-LABEL "Carton Size"
            lv-die-size           COLUMN-LABEL "Die Size"
            eb.num-up             COLUMN-LABEL "Number Up!  On Die"
            li-colors             COLUMN-LABEL "# of Col"
            li-qty                COLUMN-LABEL " Carton!Quantity"
            ld-costm              COLUMN-LABEL "FOB Cost/M"
            ld-costt              COLUMN-LABEL "FOB Total Cost"
            ld-mar[1]             COLUMN-LABEL "              !           15%"
            ld-mar[2]             COLUMN-LABEL "              !           20%"
            ld-mar[3]             COLUMN-LABEL "           FOB!           25%"
            ld-mar[4]             COLUMN-LABEL "      Variable!           30%"
            ld-mar[5]             COLUMN-LABEL "        Profit!           35%"
            ld-mar[6]             COLUMN-LABEL "       Margins!           40%"
            ld-mar[7]             COLUMN-LABEL "              !           45%"

           WITH FRAME est DOWN NO-BOX STREAM-IO WIDTH 300.

    IF tb_excel THEN
      PUT STREAM st-excel UNFORMATTED
          '"'   TRIM(eb.est-no)     '",'
          '"'   eb.part-dscr1       '",'
          '"'   eb.procat           '",'
          '"'   ef.cal              '",' 
          '"'   lv-box-size         '",' 
          '"'   lv-die-size         '",'
          '"'   eb.num-up           '",'
          '"'   li-colors           '",' 
          '"'   li-qty              '",'    
          '"'   ld-costm            '",' 
          '"'   ld-costt            '",'  
          '"'   ld-mar[1]           '",' 
          '"'   ld-mar[2]           '",'            
          '"'   ld-mar[3]           '",'             
          '"'   ld-mar[4]           '",'        
          '"'   ld-mar[5]           '",'       
          '"'   ld-mar[6]           '",'        
          '"'   ld-mar[7]           '",'          
          '"'                       '",'           
          '"'                       '",'           
          '"'                       '",'           
          '"'                       '",'            
          '"'                       '",'            
          '"'                       '",'           
          '"'                       '",'
          SKIP.
  END.
END.

OUTPUT STREAM st-excel CLOSE.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

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

