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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR save_id AS RECID.
DEF VAR time_stamp AS ch.

DEF VAR start-date AS DATE INITIAL 01/01/1901 NO-UNDO.
DEF VAR end-date AS DATE INITIAL 01/01/1901 NO-UNDO.
DEF VAR tot-all  AS DEC FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR tot-tx   LIKE tot-all NO-UNDO.
DEF VAR tot-act  LIKE tot-all NO-UNDO.
DEF VAR tot-jrnl LIKE tot-all NO-UNDO.
DEF VAR open-amt LIKE tot-all NO-UNDO.
DEF VAR net-inc  AS DEC NO-UNDO.
DEF VAR per-open AS INTE FORMAT ">9" NO-UNDO.
DEF VAR per-status LIKE period.pstat NO-UNDO.
DEF VAR fiscal-yr LIKE period.yr NO-UNDO.

DEF BUFFER b-racct FOR account.
DEF BUFFER b-cacct FOR account.
DEF VAR uperiod AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEFINE VARIABLE lMessage AS LOGICAL NO-UNDO.

ASSIGN time_stamp = STRING(TIME,"hh:mmam")
       .
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_inact tb_prior-period-data ~
tb_out-bal tb_invalid-period tb_post-out-period tb_excel tb_runExcel fi_file  ~
rd-dest lines-per-page lv-ornt lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiText tran-year tran-period tb_inact ~
tb_prior-period-data tb_out-bal tb_invalid-period tb_post-out-period ~
tb_excel tb_runExcel fi_file rd-dest lines-per-page lv-ornt lv-font-no ~
lv-font-name td-show-parm v-msg1 v-msg2 

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

DEFINE VARIABLE fiText AS CHARACTER FORMAT "X(256)":U INITIAL "This operation will perform a CLOSE on your first open period" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(250)":U INITIAL "c:~\tmp~\r-glmclo.csv" 
     LABEL "File Location" 
     VIEW-AS FILL-IN 
     SIZE 57.6 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-msg1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 90 BY 1.1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-msg2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 88 BY 1.19
     FONT 6 NO-UNDO.

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
"To File", 3
     SIZE 16 BY 5.24 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 13.57.

DEFINE VARIABLE tb_inact AS LOGICAL INITIAL NO 
     LABEL "JE's with inactive account" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_invalid-period AS LOGICAL INITIAL NO 
     LABEL "Invalid Period Entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_out-bal AS LOGICAL INITIAL NO 
     LABEL "Out of balance entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_post-out-period AS LOGICAL INITIAL NO 
     LABEL "Posting Date Outside Period" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prior-period-data AS LOGICAL INITIAL NO 
     LABEL "Prior period data" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.
     
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     FGCOLOR 8  NO-UNDO. 
     
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.     


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiText AT ROW 2.43 COL 9 COLON-ALIGNED NO-LABEL
     tran-year AT ROW 3.62 COL 20 COLON-ALIGNED WIDGET-ID 6
     tran-period AT ROW 3.62 COL 39 COLON-ALIGNED
     tb_inact AT ROW 5.29 COL 21.8 WIDGET-ID 10
     tb_prior-period-data AT ROW 5.29 COL 57 WIDGET-ID 40
     tb_out-bal AT ROW 6.43 COL 21.8 WIDGET-ID 12
     tb_invalid-period AT ROW 7.52 COL 21.8 WIDGET-ID 14
     tb_post-out-period AT ROW 8.57 COL 21.8 WIDGET-ID 16
     tb_excel AT ROW 9.81 COL 41.8 RIGHT-ALIGNED WIDGET-ID 44
     tb_runExcel AT ROW 9.81 COL 67.4 RIGHT-ALIGNED WIDGET-ID 42
     fi_file AT ROW 10.62 COL 19.4 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 18
     rd-dest AT ROW 16.1 COL 9 NO-LABEL
     lines-per-page AT ROW 16.1 COL 83 COLON-ALIGNED
     lv-ornt AT ROW 16.33 COL 31 NO-LABEL
     lv-font-no AT ROW 18.48 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 19.67 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.33 COL 10
     btn-ok AT ROW 23.05 COL 18
     btn-cancel AT ROW 23.05 COL 57
     v-msg1 AT ROW 11.71 COL 3.2 NO-LABEL WIDGET-ID 2
     v-msg2 AT ROW 13.14 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.14 COL 5
     RECT-6 AT ROW 14.67 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.33.


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
         TITLE              = "G/L Month-End Closing"
         HEIGHT             = 24.76
         WIDTH              = 95.8
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
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN fiText IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiText:READ-ONLY IN FRAME FRAME-A        = TRUE.

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
   
ASSIGN 
       tb_inact:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_invalid-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_out-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_post-out-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prior-period-data:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
                
/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-year IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN v-msg1 IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       v-msg1:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN v-msg2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       v-msg2:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* G/L Month-End Closing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* G/L Month-End Closing */
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
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  ASSIGN rd-dest
           
           tran-period
           uperiod = tran-period
           .

  RUN check-date (YES).
  IF v-invalid THEN RETURN NO-APPLY.       

  ASSIGN rd-dest
         
         tran-period
         uperiod = tran-period
         .

  RUN run-report. 
    
  DO:
     choice = NO.
     MESSAGE " Close G/L Period" uperiod VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE choice.
     IF choice THEN DO:
        RUN close-month.
        
        ASSIGN v-msg1:HIDDEN = YES
               v-msg2:HIDDEN = YES
               v-msg1 = ""
               v-msg2 = ""
               v-msg1:BGCOLOR = ?
               v-msg2:BGCOLOR = ?.
               
        APPLY "close" TO THIS-PROCEDURE.       

     END.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* File Location */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* File Location */
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
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inact C-Win
ON VALUE-CHANGED OF tb_inact IN FRAME FRAME-A /* JE's with inactive account */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_invalid-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_invalid-period C-Win
ON VALUE-CHANGED OF tb_invalid-period IN FRAME FRAME-A /* Invalid Period Entries */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_out-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_out-bal C-Win
ON VALUE-CHANGED OF tb_out-bal IN FRAME FRAME-A /* Out of balance entries */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-out-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-out-period C-Win
ON VALUE-CHANGED OF tb_post-out-period IN FRAME FRAME-A /* Posting Date Outside Period */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prior-period-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prior-period-data C-Win
ON VALUE-CHANGED OF tb_prior-period-data IN FRAME FRAME-A /* Prior period data */
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


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  ASSIGN {&self-name}.
  IF LASTKEY NE -1 THEN DO:
    RUN check-date (NO).
    IF v-invalid THEN DO:
        ASSIGN SELF:SCREEN-VALUE = "".
        APPLY 'entry' TO tran-year.
        RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-year C-Win
ON LEAVE OF tran-year IN FRAME FRAME-A /* Year */
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

    IF access-close THEN DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
  
    tran-year = YEAR(TODAY) .
    tran-period = (MONTH(TODAY))  .
  
    FIND company NO-LOCK WHERE 
        company.company EQ cocode
        NO-ERROR.
    IF NOT AVAIL company THEN DO:
        MESSAGE 
            "Company " + cocode + " does not exist in the company file."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    FIND FIRST period NO-LOCK WHERE 
        period.company EQ company.company AND
        period.pstat EQ TRUE 
        NO-ERROR.
        
    IF NOT company.yend-per THEN DO:
        MESSAGE 
            "Prior year not closed.  Must close Prior year!!!" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF AVAIL period THEN ASSIGN 
        tran-year = period.yr
        tran-period = period.pnum.
        
    IF AVAIL period AND ((period.subLedgerAP NE "C" AND company.subLedgerAP) OR 
       (period.subLedgerPO NE "C" AND company.subLedgerPO) OR 
       (period.subLedgerOP NE "C" AND company.subLedgerOP) OR 
       (period.subLedgerWIP NE "C" AND company.subLedgerWIP) OR 
       (period.subLedgerRM NE "C" AND company.subLedgerRM) OR 
       (period.subLedgerFG NE "C" AND company.subLedgerFG) OR 
       (period.subLedgerBR NE "C" AND company.subLedgerBR) OR 
       (period.subLedgerAR NE "C" AND company.subLedgerAR)) THEN
    DO:
          RUN displayMessageQuestionLOG ("60", OUTPUT lMessage).
          IF NOT lMessage THEN 
          DO:
             APPLY "close" TO THIS-PROCEDURE.
             RETURN .
          END. 
    END.       
 
  RUN enable_UI.

    {methods/nowait.i}
    DO WITH FRAME {&frame-name}:
        APPLY "entry" TO tran-year.
    END.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN 
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-oktogo AS LOG NO-UNDO.
  
    DEF BUFFER alt-period FOR period.

    DO WITH FRAME {&frame-name}:
        ASSIGN 
            v-invalid = NO.

        FIND FIRST period NO-LOCK WHERE 
            period.company EQ cocode AND 
            period.yr   EQ tran-year AND 
            period.pnum EQ tran-period
            NO-ERROR.
        IF AVAIL period THEN DO:
            IF NOT period.pstat THEN DO:
                MESSAGE 
                    "Already Closed. " 
                    VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.         
            ELSE DO:
                FIND FIRST alt-period NO-LOCK WHERE 
                    alt-period.company             EQ cocode AND 
                    alt-period.pst - period.pend   EQ 1 AND 
                    (alt-period.pnum - period.pnum EQ 1     OR
                        (alt-period.pnum              EQ 1 AND
                        period.pnum EQ company.num-per)) AND 
                        alt-period.pstat               EQ YES
                        NO-ERROR.
                IF NOT AVAIL alt-period THEN DO:
                    MESSAGE 
                        "Next Period not defined.  Must define next Period !!!"
                        VIEW-AS ALERT-BOX ERROR.
                    v-invalid = YES.
                END.
                /* CODE FOR VERIFYING CLOSE OF ALL PRIOR PERIODS */
                ELSE DO:
                    FIND FIRST alt-period NO-LOCK WHERE 
                        alt-period.company EQ cocode AND 
                        alt-period.yr   EQ tran-year AND 
                        alt-period.pnum EQ tran-period
                        NO-ERROR.
                    IF AVAIL alt-period THEN 
                        fiscal-yr = alt-period.yr.
                    FIND FIRST alt-period NO-LOCK WHERE 
                        alt-period.company EQ cocode AND 
                        (alt-period.yr     LT fiscal-yr OR
                        (alt-period.yr    EQ fiscal-yr AND
                        alt-period.pnum  LT period.pnum)) AND 
                        alt-period.pstat   EQ YES
                        NO-ERROR.
                    IF AVAIL alt-period THEN DO:
                        ASSIGN 
                            per-open   = alt-period.pnum
                            per-status = alt-period.pstat.
                        MESSAGE 
                            "Prior Month(S) not closed.  Must close all prior months!!!"
                            VIEW-AS ALERT-BOX ERROR.
                        v-invalid = YES.
                    END.
                    ELSE IF period.pnum EQ 1 AND ip-oktogo THEN DO:
                        MESSAGE 
                            "You are about to close period 1." SKIP(1)
                            "You must make sure the prior fiscal year end procedure has been run!!!"
                            SKIP(2)
                            "Do you want to continue and close the month? " VIEW-AS ALERT-BOX BUTTON YES-NO
                            UPDATE choice .
                    END.
                END.
            END.
            /*tran-period:SCREEN-VALUE = string(period.pnum).*/
        END.
        ELSE DO:
            MESSAGE 
                "No defined Period exists for" tran-period VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-month C-Win 
PROCEDURE close-month :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR li AS INT NO-UNDO.
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEFINE VARIABLE lLastPeriod AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iTransNum AS INTEGER NO-UNDO.
    DEFINE VARIABLE dNetIncome AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iClosedPeriod AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAccountTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iProgressCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dEarningsRunningTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRetEarningsRunningTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dContraRunningTotal AS DECIMAL NO-UNDO.
    DEF BUFFER b-period FOR period. 
    DEF BUFFER bEarningsAcct FOR account.
    DEF BUFFER bContraAcct FOR account.
    DEF BUFFER bRetEarningsAcct FOR account.
    DEF BUFFER bNextPeriod FOR period.
    DEF BUFFER bCompany FOR company.
    DEF BUFFER bUpdateAcct FOR account.
    
    DEF VAR cContraAcctNo AS CHAR NO-UNDO.
    DEF VAR cRetEarningsAcctNo AS CHAR NO-UNDO.
      
    SESSION:SET-WAIT-STATE ("general").

    DO TRANSACTION:
        FIND FIRST gl-ctrl NO-LOCK WHERE 
            gl-ctrl.company EQ cocode 
            NO-ERROR.
        FIND FIRST company NO-LOCK WHERE 
            company.company EQ cocode.
        FIND CURRENT period NO-LOCK NO-ERROR.
        IF period.pnum EQ company.num-per THEN ASSIGN  
            lv-rowid = ROWID(period)
            lLastPeriod = YES.
        
        /* Find the earnings account */
        FIND bEarningsAcct EXCLUSIVE WHERE 
            bEarningsAcct.company EQ cocode AND 
            bEarningsAcct.actnum  EQ gl-ctrl.ret
            NO-ERROR.
        IF NOT AVAIL bEarningsAcct THEN DO ON ENDKEY UNDO, RETURN:
            MESSAGE 
                "Unable to Find Current Year Earnings Account from G/L Control File."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        ASSIGN 
            bEarningsAcct.cyr[uperiod]  = 0.
    
        /* Find the contra account */
        FIND bContraAcct EXCLUSIVE WHERE 
            bContraAcct.company EQ cocode AND 
            bContraAcct.actnum  EQ gl-ctrl.contra
            NO-ERROR.
        IF NOT AVAIL bContraAcct THEN DO ON ENDKEY UNDO, RETURN:
            MESSAGE 
                "Unable to find Profit Contra Account from G/L Control File." 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        ASSIGN 
            cContraAcctNo = bContraAcct.actnum
            bContraAcct.cyr[uperiod]  = 0.
       
        /* Find the retained earnings account */
        FIND bRetEarningsAcct NO-LOCK WHERE 
            bRetEarningsAcct.company EQ cocode AND 
            bRetEarningsAcct.actnum  EQ gl-ctrl.retainedEarnings
            NO-ERROR.
        IF NOT AVAIL bRetEarningsAcct THEN DO ON ENDKEY UNDO, RETURN:
            MESSAGE 
                "No Retained Earnings account defined. Set up Retained Earnings account in G-F-3"
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        ASSIGN 
            cRetEarningsAcctNo = bRetEarningsAcct.actnum.

    END. /* TRANSACTION 1 */
    
    ASSIGN 
        v-msg1:HIDDEN IN FRAME {&FRAME-NAME} = NO
        v-msg2:HIDDEN = NO
        v-msg1:BGCOLOR = 4
        v-msg2:BGCOLOR = 4
        v-msg1 = "Processing... Please wait and do not cancel out of screen!". 
    DISPLAY 
        v-msg1 
        WITH FRAME {&FRAME-NAME}.
    
    /* Set basis for progress bar */
    ASSIGN 
        iCount = 0
        iProgressCount = 0.
    FOR EACH account NO-LOCK WHERE 
        account.company EQ cocode:
        iCount = iCount + 1.
    END.
    
    /* Calculate account total for month we're about to close */
    FOR EACH account NO-LOCK WHERE 
        account.company EQ cocode AND 
        account.actnum NE cContraAcctNo AND 
        account.actnum NE cRetEarningsAcctNo
        BY account.actnum:
        
        iProgressCount = iProgressCount + 1.
        RUN spProgressBar ("Close Period", iProgressCount, iCount).
    
        /* Zero out any existing account balance for the month */
        ASSIGN
            dAccountTotal  = 0. 
       
        /* Recalculate account balance based on glhist txns for the month */
        FOR EACH glhist EXCLUSIVE WHERE 
            glhist.company EQ cocode AND 
            glhist.tr-date GE period.pst AND 
            glhist.tr-date LE period.pend AND 
            glhist.actnum  EQ account.actnum          
            TRANSACTION :  
       
            v-msg2 = "Account: " + glhist.actnum + "   " + glhist.jrnl.
            DISP v-msg2 WITH FRAME {&FRAME-NAME}.          
      
            /* Ensure glhist record is marked as posted, and increment the account balance counter */
            ASSIGN
                dAccountTotal = dAccountTotal + glhist.tr-amt
                glhist.posted   = YES
                glhist.postedBy = USERID(LDBNAME(1)).           
        END. /* FOR EACH glhist - TRANSACTION 2 */ 
    
        /* Set the account balance for this period equal the sum of the GL hist records */
        DO TRANSACTION:
            FIND bUpdateAcct EXCLUSIVE WHERE 
                ROWID(bUpdateAcct) EQ ROWID(account).
            ASSIGN 
                bUpdateAcct.cyr[uperiod] = dAccountTotal.
        END /* TRANSACTION 3 */. 
        
        /* If this is a Revenue or Expense account, create an offset entry in the Earnings account */
        DO TRANSACTION:
            IF INDEX("RE",account.type) GT 0 THEN DO:
                /* Credit the earnings account */
                FIND FIRST b-racct EXCLUSIVE WHERE 
                    b-racct.company EQ cocode AND 
                    b-racct.actnum  EQ gl-ctrl.ret.
                ASSIGN 
                    b-racct.cyr[uperiod] = b-racct.cyr[uperiod] + dAccountTotal.
                FIND CURRENT b-racct NO-LOCK.
                
                /* Debit the contra account */
                FIND FIRST b-cacct EXCLUSIVE WHERE 
                    b-cacct.company EQ cocode AND 
                    b-cacct.actnum  EQ gl-ctrl.contra.
                ASSIGN 
                    b-cacct.cyr[uperiod] = b-cacct.cyr[uperiod] - dAccountTotal.
                FIND CURRENT b-cacct NO-LOCK.
            END.      
        END. /* TRANSACTION 4 */
    END.

    DO TRANSACTION :
        FIND bNextPeriod EXCLUSIVE WHERE  
            ROWID(bNextPeriod) EQ ROWID(period).
        ASSIGN 
            bNextPeriod.pstat = FALSE
            iClosedPeriod = period.pnum.
            
        IF period.pnum EQ company.num-per THEN DO:
            FIND bCompany EXCLUSIVE WHERE 
                ROWID(bCompany) EQ ROWID(company).
            ASSIGN 
                bCompany.yend-per = NO.
        END.
    END. /* TRANSACTION 5 */

    FIND NEXT period NO-LOCK WHERE 
        period.company EQ cocode AND 
        period.pstat   EQ YES.
    IF AVAIL period THEN ASSIGN 
        tran-period = period.pnum
        uperiod = period.pnum.
                               
    /* If this is last month of year, do some end of year calcs */
    IF lLastPeriod THEN DO TRANSACTION:
        FIND FIRST gl-ctrl EXCLUSIVE WHERE 
            gl-ctrl.company EQ cocode 
            NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN ASSIGN 
            iTransNum = gl-ctrl.trnum + 1
            gl-ctrl.trnum = iTransNum.
        FIND CURRENT gl-ctrl NO-LOCK 
            NO-ERROR .
                  
        /* Calculate net income for year from sum of contra account monthly totals */
        RUN pGetNetIncome (
            INPUT gl-ctrl.contra, 
            OUTPUT dNetIncome). 
        
        /* Create Retained earning entries for this year end and next year open */
        RUN GL_SpCreateGLHist (
            cocode,
            gl-ctrl.retainedEarnings,
            "AutoClose",
            STRING("Auto Posted Net Income for ") + STRING(period.yr),
            period.pst,
            - dNetIncome,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
            iTransNum,
            period.pnum,
            "A",
            DATE(TODAY),
            "",
            "").        
        RUN GL_SpCreateGLHist (
            cocode, 
            gl-ctrl.ret,
            "AutoClose",
            STRING("Auto Posted Current Years Earnings for ") + STRING(period.yr),
            period.pst,
            dNetIncome,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
            iTransNum,
            period.pnum,
            "A",
            DATE(TODAY),
            "",
            "").                      
 
    END. /* TRANSACTION 6 */
  
    SESSION:SET-WAIT-STATE ("").
    
    RUN spProgressBar ("Close Period", 1, 1).
    MESSAGE 
        "Period " STRING(iClosedPeriod) " is closed." SKIP
        "Current accounting period changed to " uperiod SKIP
        "Closing G/L Period is completed." 
        VIEW-AS ALERT-BOX INFO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-test C-Win 
PROCEDURE close-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        v-msg1:HIDDEN IN FRAME {&FRAME-NAME} = NO
        v-msg2:HIDDEN = NO
        v-msg1:BGCOLOR = 4
        v-msg1 = "Processing... Please wait and do not cancel out of screen!". 
    DISPLAY 
        v-msg1 
        WITH FRAME {&FRAME-NAME}.

    FOR EACH glhist NO-LOCK WHERE 
        glhist.company EQ cocode AND 
        glhist.tr-date GE period.pst AND 
        glhist.tr-date LE period.pend AND 
        glhist.period  EQ uperiod AND 
        glhist.posted  EQ NO:

        ASSIGN 
            v-msg2 = jrnl + "Period: "  + string(glhist.period) + "Act: " + glhist.actnum.
        DISP 
            v-msg2 
            WITH FRAME {&FRAME-NAME}.
    END.
END.

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
  DISPLAY fiText tran-year tran-period tb_inact tb_prior-period-data tb_out-bal 
          tb_invalid-period tb_post-out-period tb_excel tb_runExcel fi_file 
          rd-dest lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm 
          v-msg1 v-msg2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_inact tb_prior-period-data tb_out-bal 
         tb_invalid-period tb_post-out-period tb_excel tb_runExcel fi_file 
         rd-dest lines-per-page lv-ornt lv-font-no td-show-parm btn-ok 
         btn-cancel 
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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     IF init-dir = "" THEN init-dir = "c:\temp" .

     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
         SAVE-AS
         USE-FILENAME
         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.

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

     RUN custom/prntproc.p (
        list-name,
        INT(lv-font-no), 
        lv-ornt). 

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
    RUN scr-rpt.w (
        list-name,
        c-win:TITLE,
        INT(lv-font-no), 
        lv-ornt). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    IF tb_excel THEN DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        excelheader = "Account,Description,Reason" .
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.
             
    SESSION:SET-WAIT-STATE("general").
    ASSIGN 
        uperiod = tran-period .
        
    FIND FIRST period NO-LOCK WHERE 
        period.company EQ cocode AND 
        period.yr   EQ tran-year AND 
        period.pnum EQ tran-period
        NO-ERROR.
       
    SESSION:SET-WAIT-STATE ("general").
 
    IF tb_excel THEN DO:
        FOR EACH account WHERE account.company EQ cocode NO-LOCK :
            IF tb_inact AND account.inactive THEN DO:
                PUT STREAM excel UNFORMATTED
                    '"' account.actnum                   '",'
                    '"' account.dscr                     '",'
                    '"' "Inactive Account"               '",'
                    SKIP.
            END.
        END.
   
        PUT STREAM excel UNFORMATTED SKIP(1) .
    
        PUT STREAM excel UNFORMATTED
            '"' "Account "                 '",'
            '"' "TR No   "                 '",'
            '"' "Description"              '",'
                    '"' "Journal"                  '",'
            '"' "Date"                     '",'
            '"' "Period"                   '",'
            '"' "Amount"                   '",'
            '"' "Reason  "                 '",'
            SKIP.
    END.
 
    FOR EACH account NO-LOCK WHERE 
        account.company EQ cocode 
        :
       
        IF tb_excel THEN DO:
            IF tb_invalid-period THEN DO:
                FOR EACH glhist NO-LOCK WHERE 
                    glhist.company EQ cocode AND 
                    glhist.actnum  EQ account.actnum AND 
                    glhist.tr-date GE period.pst AND 
                    glhist.tr-date LE period.pend AND 
                    glhist.period EQ 0 AND 
                    glhist.posted EQ NO BREAK BY glhist.actnum:
              
                    PUT STREAM excel UNFORMATTED
                        '"' account.actnum                  '",'
                        '"' glhist.tr-num                   '",'
                        '"' account.dscr                    '",'
                        '"' glhist.jrnl                    '",'
                        '"' glhist.tr-date                 '",'
                        '"' glhist.period                 '",'
                        '"' glhist.tr-amt                  '",'
                        '"' "Invalid Period  "               '",'
                        SKIP.
                END.
            END.
      
            IF tb_post-out-period THEN DO:
                FOR EACH glhist NO-LOCK WHERE 
                    glhist.company EQ cocode AND 
                    glhist.actnum  EQ account.actnum AND 
                    glhist.tr-date LT period.pst AND 
                    glhist.tr-date GT period.pend AND 
                    glhist.period EQ uperiod AND 
                    glhist.posted EQ NO BREAK BY glhist.actnum:
              
                    PUT STREAM excel UNFORMATTED
                        '"' account.actnum                  '",'
                        '"' glhist.tr-num                   '",'
                        '"' account.dscr                    '",'
                        '"' glhist.jrnl                    '",'
                        '"' glhist.tr-date                 '",'
                        '"' glhist.period                 '",'
                        '"' glhist.tr-amt                  '",'
                        '"' "Data outside period  "         '",'
                        SKIP.
                END.
            END.

            IF tb_prior-period-data THEN DO:
                FOR EACH glhist NO-LOCK WHERE 
                    glhist.company EQ cocode AND 
                    glhist.actnum  EQ account.actnum AND 
                    glhist.tr-date LT period.pst AND 
                    glhist.posted EQ NO:
              
                    PUT STREAM excel UNFORMATTED
                        '"' account.actnum                  '",'
                        '"' glhist.tr-num                   '",'
                        '"' account.dscr                    '",'
                        '"' glhist.jrnl                    '",'
                        '"' glhist.tr-date                 '",'
                        '"' glhist.period                 '",'
                        '"' glhist.tr-amt                  '",'
                        '"' "Invalid Data  "  '",'
                        SKIP.
                END.
            END.
        END. /* tb_excel*/

        IF LINE-COUNTER GT PAGE-SIZE - 3 THEN PAGE.
      
        open-amt = account.cyr-open.
        DO i = 1 TO uperiod:
            open-amt = open-amt + cyr[i].
        END.
      
        FIND FIRST glhist NO-LOCK WHERE 
            glhist.company EQ cocode AND 
            glhist.actnum  EQ account.actnum AND 
            glhist.tr-date GE period.pst AND 
            glhist.tr-date LE period.pend AND 
            glhist.period  EQ uperiod AND 
            glhist.posted  EQ NO
            NO-ERROR.
      
        IF open-amt EQ 0 
        AND NOT AVAIL glhist THEN 
            NEXT.
                
        tot-all = tot-all + open-amt.

        FOR EACH glhist NO-LOCK WHERE 
            glhist.company EQ account.company AND 
            glhist.actnum  EQ account.actnum AND 
            glhist.tr-date GE period.pst AND 
            glhist.tr-date LE period.pend AND 
            glhist.period  EQ uperiod AND 
            glhist.posted  EQ NO
            BREAK BY glhist.jrnl 
            :

            IF LINE-COUNTER GT PAGE-SIZE - 2 THEN PAGE.

            ASSIGN
                tot-tx   = tot-tx   + tr-amt
                tot-all  = tot-all  + tr-amt
                tot-jrnl = tot-jrnl + tr-amt
                tot-act  = tot-act  + tr-amt.

            IF LAST-OF(glhist.jrnl) THEN DO:                  
                tot-jrnl = 0.                
            END.
        END. /* each glhist */

        
        tot-act = 0.
    END. /* each account */
    
    IF tb_excel THEN DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN 
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetNetIncome C-Win 
PROCEDURE pGetNetIncome :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdNetAmount AS DECIMAL NO-UNDO.

    DEFINE BUFFER bContra FOR account.
    
    FIND FIRST bContra NO-LOCK WHERE 
        bContra.company EQ cocode AND 
        bContra.actnum  EQ ipcAccount NO-ERROR. 
    DO i = 1 TO company.num-per:
        opdNetAmount = opdNetAmount + bContra.cyr[i].
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
    DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
    DEF VAR parm-fld-list AS cha NO-UNDO.
    DEF VAR parm-lbl-list AS cha NO-UNDO.
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lv-label AS cha.

    ASSIGN 
        lv-frame-hdl = FRAME {&frame-name}:HANDLE 
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN 
            LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0 THEN DO:      
            IF lv-field-hdl:LABEL <> ? THEN ASSIGN 
                parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                .
            ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN ASSIGN 
                parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                parm-lbl-list = parm-lbl-list + lv-field-hdl:HELP + "," 
                .
            ELSE DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN ASSIGN 
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
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
        IF ENTRY(i,parm-fld-list) NE "" 
        OR entry(i,parm-lbl-list) NE "" THEN DO:
      
            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                     trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT 
                lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-period C-Win 
PROCEDURE valid-period :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

