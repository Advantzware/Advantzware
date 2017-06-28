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
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok   AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

{ce/msfcalc.i}
{ce/print4.i "new shared" "new shared"}


DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-summ AS LOG INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE fr-tot-pre AS DECIMAL.
DEFINE NEW SHARED VARIABLE gEstSummaryOnly AS LOG NO-UNDO.
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.
DEFINE BUFFER bf-probe FOR probe .
DEFINE STREAM st-excel.

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
tb_runExcel fi_file rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
tb_runExcel fi_file rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cust-marging.csv" 
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
              
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES 
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
     
     tb_runExcel AT ROW 9.1 COL 43 RIGHT-ALIGNED
     fi_file AT ROW 10.29 COL 21 COLON-ALIGNED HELP
          "Enter File Name"
     rd-dest AT ROW 13.38 COL 4 NO-LABELS
     lv-ornt AT ROW 14.33 COL 31 NO-LABELS
     lines-per-page AT ROW 14.33 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.76 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.71 COL 28 COLON-ALIGNED NO-LABELS
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
         TITLE              = "Customer Margin Analysis"
         HEIGHT             = 22.62
         WIDTH              = 95.6
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

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
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Add Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Beginning Mod Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     DEFINE VARIABLE char-val AS cha NO-UNDO.
     DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

     RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

     IF char-val <> "" THEN DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                begin_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
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

    IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).

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
                            &fax-file=fi_file }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust="begin_cust-no"
                             &end_cust="begin_cust-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=fi_file }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust="begin_cust-no"
                                  &end_cust="begin_cust-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=fi_file }

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
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Add Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-2 C-Win
ON LEAVE OF end_date-2 IN FRAME FRAME-A /* Ending Mod Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     DEFINE VARIABLE char-val AS cha NO-UNDO.
     DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

     RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

     IF char-val <> "" THEN DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                           lv-eb-tmpid = RECID(eb)    
                                  end_est:SCREEN-VALUE = eb.est-no.

            END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
   DEFINE VARIABLE ls-filename AS cha NO-UNDO.
   DEFINE VARIABLE ll-ok AS LOG NO-UNDO.

   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select File to Save "
                 FILTERS "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN SELF:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
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
    APPLY "entry" TO begin_cust-no.
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
  DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
          begin_date end_date begin_date-2 end_date-2 tb_runExcel 
          fi_file rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
         begin_date end_date begin_date-2 end_date-2 tb_runExcel 
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
RUN custom/d-print.w (fi_file).

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
 RUN custom/prntproc.p (fi_file,int(lv-font-no),lv-ornt).
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
  /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{sys/form/r-topw.f}

DEFINE VARIABLE fest LIKE est.est-no NO-UNDO.
DEFINE VARIABLE test LIKE fest       NO-UNDO.

DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-box-size LIKE quoteitm.size NO-UNDO.
DEFINE VARIABLE lv-die-size LIKE quoteitm.size NO-UNDO.
DEFINE VARIABLE lv-format AS CHARACTER NO-UNDO.
DEFINE VARIABLE li-colors AS INTEGER NO-UNDO.
DEFINE VARIABLE li-qty LIKE probe.est-qty NO-UNDO.
DEFINE VARIABLE ld-costm LIKE probe.full-cost NO-UNDO.
DEFINE VARIABLE ld-costt AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE ld-price LIKE probe.sell-price NO-UNDO.
DEFINE VARIABLE ld-mar AS DECIMAL FORMAT "->>,>>>,>>9.99" EXTENT 7 NO-UNDO.
DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
DEFINE VARIABLE k_frac AS DECIMAL INIT "6.25" NO-UNDO.
DEFINE VARIABLE v-tons AS DECIMAL NO-UNDO .
DEFINE VARIABLE derpmargin AS DECIMAL NO-UNDO .
DEFINE VARIABLE destcost AS DECIMAL NO-UNDO .
DEFINE BUFFER reftable-fm FOR reftable.


ASSIGN
 str-tit2 = TRIM(c-win:TITLE) + ""
 {sys/inc/ctrtext.i str-tit2 112}

 fest = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

{sys/inc/print1.i}

{sys/inc/outprint.i  VALUE(lines-per-page)}


  OUTPUT STREAM st-excel TO VALUE(fi_file).
  PUT STREAM st-excel UNFORMATTED
      ",,Estimate # /,,,Board Cost /,Board Cost /,,,,,,ERP Margin,ERP Margin,,Est. Margin,Est. Margin ,,,,," +
      ","
      SKIP
      "Customer,Date, Order #,Selling Price,Board Tons, Ton (ERP),Ton (Actual)," +
      "Board Pad,GSA MU B,GSA MU M,GSA Labor,ERP Cost,($),(%)," +
      "Est. Cost,($),(%)" 
      SKIP.

SESSION:SET-WAIT-STATE ("general").

IF td-show-parm THEN RUN show-param.

VIEW FRAME r-top.

FOR EACH est NO-LOCK
    WHERE est.company  EQ cocode
      AND est.est-no   GE fest
      AND est.est-no   LE test
      AND est.est-date GE begin_date
      AND est.est-date LE end_date
      AND est.mod-date GE begin_date-2
      AND est.mod-date LE end_date-2
      AND (est.est-type EQ 4 OR est.est-type EQ 8)
    ,

    FIRST est-qty NO-LOCK
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    ,

    EACH eb NO-LOCK
    WHERE eb.company  EQ est.company
      AND eb.est-no   EQ est.est-no
      AND eb.cust-no  GE begin_cust-no
      AND eb.cust-no  LE end_cust-no
      AND eb.sman     GE begin_slsmn
      AND eb.sman     LE end_slsmn
     /* AND (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))*/
    ,

    FIRST ef NO-LOCK
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    ,

    EACH probe NO-LOCK
    WHERE probe.company   EQ est.company
      AND probe.est-no    EQ est.est-no
      AND probe.full-cost NE ?

    BREAK BY est.est-no DESCENDING
          BY probe.est-qty
          BY probe.probe-date
          BY probe.probe-time:

    {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}

  IF LAST-OF(probe.est-qty) THEN DO:
    
    li-colors =  0.
    derpmargin = 0 .
    destcost   = 0.
    
    RELEASE probeit.
    IF est.est-type EQ 3 OR est.est-type EQ 4 OR
       est.est-type EQ 7 OR est.est-type EQ 8 THEN
      FIND FIRST probeit NO-LOCK
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.line
            AND probeit.part-no EQ eb.part-no
          NO-ERROR.

    IF AVAILABLE probeit THEN
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

    FIND xest NO-LOCK WHERE RECID(xest) = recid(est)  NO-ERROR.
    FIND xef  NO-LOCK WHERE RECID(xef) = recid(ef)  NO-ERROR.
    FIND xeb  NO-LOCK WHERE RECID(xeb) = recid(eb)  NO-ERROR.

    ASSIGN t-blksht = 0
           tt-blk   = 0
           t-blkqty = 0
           vbsf     = 0 .
    FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.
    ASSIGN
        ctrl[1]  = ce-ctrl.whse-mrkup / 100
        ctrl[2]  = ce-ctrl.hand-pct / 100
        ctrl[3]  = ce-ctrl.rm-rate
        ctrl[4]  = ce-ctrl.spec-%[1]
        ctrl[5]  = int(ce-ctrl.comm-add)
        ctrl[6]  = int(ce-ctrl.shp-add)
        ctrl[7]  = int(ce-ctrl.sho-labor)
        ctrl[8]  = int(ce-ctrl.trunc-99)
        ctrl[11] = ce-ctrl.spec-%[2]
        ctrl[12] = ce-ctrl.spec-%[3]
        ctrl[13] = int(ce-ctrl.spec-add[1])
        ctrl[14] = int(ce-ctrl.spec-add[2])
        ctrl[15] = int(ce-ctrl.spec-add[3])
        ctrl[16] = int(ce-ctrl.spec-add[6])
        ctrl[17] = int(ce-ctrl.spec-add[7])
        ctrl[18] = int(ce-ctrl.spec-add[8]).


    FOR EACH xeb OF xef BY xeb.blank-no:
      /* set total # of blanks on all forms */

      ASSIGN
      tt-blk = tt-blk + IF xeb.yrprice /*AND NOT ll-tandem*/ THEN xeb.yld-qty ELSE xeb.bl-qty
      /* set total # of blanks on this form */
      t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up
      /* set total qty of all blanks for this form */
      t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                              IF xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty.
      /* find sheet qty needed for this form (without spoil)*/
      IF (xeb.yld-qty / xeb.num-up) > zzz THEN
      ASSIGN zzz = (xeb.yld-qty / xeb.num-up).
      {sys/inc/roundup.i zzz}
      ASSIGN
      t-shtfrm[xeb.form-no] = zzz
      /*call_id = recid(xeb)*/
      vbsf = vbsf + IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144)
      brd-l[4]  = xeb.t-len
      brd-w[4]  = xeb.t-wid
      brd-sq[4] = xeb.t-sqin  /*brd-l[4] * brd-w[4]*/
      brd-sf[4] = IF v-corr THEN (brd-sq[4] * .007) ELSE (brd-sq[4] / 144)
      brd-wu[4] = brd-sf[4] * item.basis-w.
     
   END.
   

   qty = IF eb.yrprice /*AND NOT ll-tandem*/ THEN eb.yld-qty ELSE eb.bl-qty.
   
   {est/calcpcts.i est}
   dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.

   RUN ce/com/pr4-brd.p ("").
   /* mat */
      ctrl[9] = ce-ctrl.mat-pct[1] .
   /* lab */
      ctrl[10] = ce-ctrl.lab-pct[1] .
  
  ASSIGN
       gsa-mat = ctrl[9]  * 100
       gsa-lab = ctrl[10] * 100
       gsa-com = ce-ctrl.comm-mrkup
       gsa-war = ce-ctrl.whse-mrkup
        .

   FIND FIRST reftable-fm NO-LOCK
       WHERE reftable-fm.reftable EQ "gsa-fm"
       AND reftable-fm.company  EQ xest.company
       AND reftable-fm.loc      EQ ""
       AND reftable-fm.code     EQ xest.est-no
       NO-ERROR.

   IF AVAILABLE reftable-fm THEN
       gsa-fm = reftable-fm.val[1].
   ELSE
       gsa-fm = ctrl[19].
   
   ASSIGN
       ctrl[9]  = gsa-mat / 100
       ctrl[10] = gsa-lab / 100
       ctrl[1]  = gsa-war / 100
       ctrl[19] = gsa-fm / 100.
   
   RUN ce/com/pr4-tots.p.

   FIND FIRST item NO-LOCK
        WHERE ITEM.company EQ eb.company
          AND item.i-no = ef.board NO-ERROR.
   IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.

   IF ctrl2[9] EQ ? THEN ctrl2[9] = 0.
   IF ctrl2[10] EQ ? THEN ctrl2[10] = 0.
    v-tons = (IF v-corr THEN (ef.gsh-len * ef.gsh-wid * .007)
                          ELSE (ef.gsh-len * ef.gsh-wid / 144) ) * probe.gsh-qty  / 1000 * item.basis-w / 2000 .
   
    derpmargin = (probe.sell-price - probe.full-cost) .
    destcost   = probe.full-cost - (0 + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) .
    IF v-tons EQ ? THEN v-tons = 0 .

    DISPLAY eb.cust-no            FORMAT "x(8)" COLUMN-LABEL "Customer"
            est.est-date           COLUMN-LABEL "date"
            TRIM(eb.est-no)       FORMAT "x(8)"
                                  COLUMN-LABEL "Est#"
            probe.sell-price      COLUMN-LABEL "Selling Price"
            v-tons                COLUMN-LABEL "Board Tons"
            ""                    COLUMN-LABEL "Board Cost / Ton (ERP)"
            ""                     COLUMN-LABEL "Board Cost / Ton (Actual)"
            ""                    COLUMN-LABEL  "Board Pad"
            calcpcts.val[2]        COLUMN-LABEL "GSA MU B"
            ctrl2[9]               COLUMN-LABEL "GSA MU M"
            ctrl2[10]              COLUMN-LABEL "GSA Labor"
            probe.full-cost              COLUMN-LABEL "ERP Cost"
            derpmargin                 COLUMN-LABEL "ERP Margin ($)"
            derpmargin * 100 / probe.sell-price           COLUMN-LABEL "ERP Margin (%)"
            destcost              COLUMN-LABEL "Est. Cost"
             probe.sell-price - destcost            COLUMN-LABEL "Est. Margin ($)"
            (probe.sell-price - destcost) * 100 /  probe.sell-price            COLUMN-LABEL "Est. Margin (%)"
           

           WITH FRAME est DOWN NO-BOX STREAM-IO WIDTH 300.

    
      PUT STREAM st-excel UNFORMATTED
          '"'   eb.cust-no                '",'
          '"'   est.est-date              '",'
          '"'   TRIM(eb.est-no)           '",'
          '"'   probe.sell-price          '",' 
          '"'   v-tons                    '",' 
          '"'   probe.full-cost           '",'
          '"'   ""           '",'
          '"'   ""           '",' 
          '"'   calcpcts.val[2]                        '",'    
          '"'   ctrl2[9]                               '",' 
          '"'   ctrl2[10]                              '",'  
          '"'   probe.full-cost                        '",' 
          '"'   derpmargin                             '",'            
          '"'   derpmargin * 100 / probe.sell-price FORMAT "->>>9.99%"   '",'             
          '"'   destcost                               '",'        
          '"'    probe.sell-price - destcost           '",'       
          '"'   (probe.sell-price - destcost) * 100 /  probe.sell-price  FORMAT "->>>9.99%"          '",'        
          
          SKIP.
  END.
END.

OUTPUT STREAM st-excel CLOSE.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

