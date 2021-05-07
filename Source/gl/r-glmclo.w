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
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.

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

def var save_id as recid.
def var time_stamp as ch.

def var start-date as date initial 01/01/1901 NO-UNDO.
def var end-date as date initial 01/01/1901 NO-UNDO.
def var tot-all  as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tot-tx   like tot-all NO-UNDO.
def var tot-act  like tot-all NO-UNDO.
def var tot-jrnl like tot-all NO-UNDO.
def var open-amt like tot-all NO-UNDO.
def var net-inc  as dec NO-UNDO.
def var per-open as inte format ">9" NO-UNDO.
def var per-status like period.pstat NO-UNDO.
def var fiscal-yr like period.yr NO-UNDO.

def buffer b-racct for account.
def buffer b-cacct for account.
DEF VAR uperiod AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEFINE VARIABLE lMessage AS LOGICAL NO-UNDO.

ASSIGN time_stamp = string(time,"hh:mmam")
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

DEFINE VARIABLE tb_inact AS LOGICAL INITIAL no 
     LABEL "JE's with inactive account" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_invalid-period AS LOGICAL INITIAL no 
     LABEL "Invalid Period Entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_out-bal AS LOGICAL INITIAL no 
     LABEL "Out of balance entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_post-out-period AS LOGICAL INITIAL no 
     LABEL "Posting Date Outside Period" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prior-period-data AS LOGICAL INITIAL no 
     LABEL "Prior period data" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.
     
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     FGCOLOR 8  NO-UNDO. 
     
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
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
THEN C-Win:HIDDEN = no.

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
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  assign rd-dest
           
           tran-period
           uperiod = tran-period
           .

  run check-date (YES).
  if v-invalid then return no-apply.       

  assign rd-dest
         
         tran-period
         uperiod = tran-period
         .

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 
  DO:
     choice = NO.
     MESSAGE " Close G/L Period" uperiod VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE choice.
     IF choice THEN do:
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


&Scoped-define SELF-NAME tb_inact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inact C-Win
ON VALUE-CHANGED OF tb_inact IN FRAME FRAME-A /* JE's with inactive account */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_invalid-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_invalid-period C-Win
ON VALUE-CHANGED OF tb_invalid-period IN FRAME FRAME-A /* Invalid Period Entries */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_out-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_out-bal C-Win
ON VALUE-CHANGED OF tb_out-bal IN FRAME FRAME-A /* Out of balance entries */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-out-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-out-period C-Win
ON VALUE-CHANGED OF tb_post-out-period IN FRAME FRAME-A /* Posting Date Outside Period */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prior-period-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prior-period-data C-Win
ON VALUE-CHANGED OF tb_prior-period-data IN FRAME FRAME-A /* Prior period data */
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


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  assign {&self-name}.
  if lastkey ne -1 then do:
    run check-date (NO).
    if v-invalid then DO:
        ASSIGN SELF:SCREEN-VALUE = "".
        APPLY 'entry' TO tran-year.
        return no-apply.
    END.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-year C-Win
ON LEAVE OF tran-year IN FRAME FRAME-A /* Year */
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
        
    if NOT company.yend-per then do:
        MESSAGE 
            "Prior year not closed.  Must close Prior year!!!" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    end.
    
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
    DO with frame {&frame-name}:
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
  
  def buffer alt-period for period.


  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          AND period.yr   EQ tran-year
          AND period.pnum EQ tran-period
        no-lock no-error.
    if avail period THEN DO:
       IF NOT period.pstat THEN DO:
          MESSAGE "Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.         
       else do:
         find first alt-period
             where alt-period.company             eq cocode
               and alt-period.pst - period.pend   eq 1
               and (alt-period.pnum - period.pnum eq 1     or
                    (alt-period.pnum              eq 1 and
                     period.pnum eq company.num-per))
               and alt-period.pstat               eq yes
           no-lock no-error.
         if not avail alt-period then do:
           MESSAGE "Next Period not defined.  Must define next Period !!!"
               VIEW-AS ALERT-BOX ERROR.
           v-invalid = YES.
         end.
         /* CODE FOR VERIFYING CLOSE OF ALL PRIOR PERIODS */
         else do:
           find first alt-period where alt-period.company eq cocode
                                   AND alt-period.yr   EQ tran-year
                                   AND alt-period.pnum EQ tran-period
                                 no-lock no-error.
           if avail alt-period then fiscal-yr = alt-period.yr.
           find first alt-period where alt-period.company eq cocode
                    and (alt-period.yr     lt fiscal-yr or
                        (alt-period.yr    eq fiscal-yr and
                         alt-period.pnum  lt period.pnum))
                    and alt-period.pstat   eq yes
                    no-lock no-error.
           if avail alt-period then do:
             ASSIGN per-open   = alt-period.pnum
                    per-status = alt-period.pstat.
             MESSAGE "Prior Month(S) not closed.  Must close all prior months!!!"
                   VIEW-AS ALERT-BOX ERROR.
             v-invalid = YES.
           end.
           ELSE
           if period.pnum eq 1 AND ip-oktogo then do:
             MESSAGE "You are about to close period 1." skip(1)
                     "You must make sure the prior fiscal year end procedure has been run!!!"
                     skip(2)
                     "Do you want to continue and close the month? " VIEW-AS ALERT-BOX BUTTON YES-NO
                     update choice .
           end.
         end.
       END.
       /*tran-period:SCREEN-VALUE = string(period.pnum).*/
    END.

    ELSE DO:
      message "No defined Period exists for" tran-period view-as alert-box error.
      v-invalid = yes.
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
   DEF BUFFER b-period FOR period. 
      
   SESSION:SET-WAIT-STATE ("general").

   find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.
   find first company where company.company eq cocode.
   find first b-racct
       where b-racct.company eq cocode
         and b-racct.actnum  eq gl-ctrl.ret
       no-lock no-error.
   if not avail b-racct then do on endkey undo, return:
      message "Unable to Find Current Year Earnings Account from G/L Control File."
              VIEW-AS ALERT-BOX ERROR.
      return.
   end.

   find first b-cacct
       where b-cacct.company eq cocode
         and b-cacct.actnum  eq gl-ctrl.contra
       no-lock no-error.
   if not avail b-cacct then do on endkey undo, return:
      message "Unable to find Profit Contra Account from G/L Control File." VIEW-AS ALERT-BOX ERROR.
      return.
   end.
   
   find first b-racct
       where b-racct.company eq cocode
         and b-racct.actnum  eq gl-ctrl.retainedEarnings
       no-lock no-error.
   if not avail b-racct then do on endkey undo, return:
      message "No Retained Earnings account defined. Set up Retained Earnings account in G-F-3"
              VIEW-AS ALERT-BOX ERROR.
      return.
   end.
   
   

   ASSIGN v-msg1:HIDDEN IN FRAME {&FRAME-NAME} = NO
          v-msg2:HIDDEN = NO
          v-msg1:BGCOLOR = 4
          v-msg2:BGCOLOR = 4
          v-msg1 = "Processing... Please wait and do not cancel out of screen!". 
   DISPLAY v-msg1 WITH FRAME {&FRAME-NAME}.
   iCount = 0.
   FOR EACH account NO-LOCK
          where account.company eq cocode
            BREAK BY account.actnum:
            iCount = iCount + 1.
   END.
   iProgressCount = 0.
   FOR EACH account EXCLUSIVE-LOCK
          where account.company eq cocode
            BREAK BY account.actnum:
    iProgressCount = iProgressCount + 1.
    RUN spProgressBar ("Close Period", iProgressCount, iCount).
    
    IF FIRST-OF(account.actnum)  THEN
       ASSIGN
         account.cyr[uperiod]  = 0
         dAccountTotal  = 0. 
       
    FOR EACH glhist
        where glhist.company eq cocode
        and glhist.tr-date ge period.pst
        and glhist.tr-date le period.pend
        and glhist.actnum  EQ account.actnum          
       TRANSACTION :  
       
       v-msg2 = "Account: " + glhist.actnum + "   " + glhist.jrnl.
       DISP v-msg2 WITH FRAME {&FRAME-NAME}.          
      
         dAccountTotal = dAccountTotal + glhist.tr-amt.
         
      assign
       glhist.posted   = YES
       glhist.postedBy = USERID(LDBNAME(1)).           
    END. /* FOR EACH glhist*/ 
    
    account.cyr[uperiod] = dAccountTotal.

     if index("RE",account.type) gt 0 then do:
        find first b-racct
            where b-racct.company eq cocode
              and b-racct.actnum  eq gl-ctrl.ret.

        b-racct.cyr[uperiod] = b-racct.cyr[uperiod] + dAccountTotal.

        find first b-cacct
            where b-cacct.company eq cocode
              and b-cacct.actnum  eq gl-ctrl.contra.

        b-cacct.cyr[uperiod] = b-cacct.cyr[uperiod] - dAccountTotal.
     end.      
    
   end.
   RELEASE glhist.
/*   for each cust where cust.company eq cocode transaction:                 */
/*     assign                                                                */
/*       cust.cost[1] = 0                                                    */
/*       cust.comm[1] = 0.                                                   */
/*                                                                           */
/*      for each ar-ledger                                                   */
/*          where ar-ledger.company eq cocode                                */
/*            and ar-ledger.cust-no eq cust.cust-no                          */
/*            and ar-ledger.tr-date gt period.pend                           */
/*            and ar-ledger.ref-num begins "INV#"                            */
/*          no-lock,                                                         */
/*                                                                           */
/*          first ar-inv                                                     */
/*          where ar-inv.company eq cocode                                   */
/*            and ar-inv.posted  eq yes                                      */
/*            and ar-inv.cust-no eq cust.cust-no                             */
/*            and ar-inv.inv-no  eq int(substr(ar-ledger.ref-num,6,          */
/*                                                length(ar-ledger.ref-num)))*/
/*          use-index posted no-lock:                                        */
/*                                                                           */
/*         assign                                                            */
/*          cust.cost[1] = cust.cost[1] +                                    */
/*                         if ar-inv.t-cost eq ? then 0 else ar-inv.t-cost   */
/*          cust.comm[1] = cust.comm[1] +                                    */
/*                         if ar-inv.t-comm eq ? then 0 else ar-inv.t-comm.  */
/*      end.                                                                 */
/*   end.                                                                    */

   IF period.pnum EQ company.num-per THEN DO:
     lv-rowid = ROWID(period).
     lLastPeriod = YES.

/*     FIND NEXT period                                                        */
/*         WHERE period.company EQ cocode                                      */
/*           AND period.pstat   EQ YES                                         */
/*         NO-LOCK NO-ERROR.                                                   */
                                                                               
/*     IF AVAIL period THEN DO:                                                    */
/*       /* Cust Processing  */                                                    */
/*       FOR EACH cust WHERE cust.company eq cocode:                               */
/*         STATUS DEFAULT "Please Wait...Updating Customer: " + TRIM(cust.cust-no).*/
/*                                                                                 */
/*         {util/reopeny1.i 1 lyytd lyr 6}                                         */
/*                                                                                 */
/*         {util/reopeny1.i 0 ytd ytd 5}                                           */
/*       END.                                                                      */
/*                                                                                 */
/*       /* Vend Processing  */                                                    */
/*       FOR EACH vend WHERE vend.company eq cocode:                               */
/*         STATUS DEFAULT "Please Wait...Updating Vendor: " + TRIM(vend.vend-no).  */
/*                                                                                 */
/*         {util/reopeny2.i 1 lyytd last-year}                                     */
/*                                                                                 */
/*         {util/reopeny2.i 0 ytd-msf purch[13]}                                   */
/*       END. /* for each vend */                                                  */
/*     END.                                                                        */

     FIND period WHERE ROWID(period) EQ lv-rowid NO-LOCK NO-ERROR.

/*     FIND FIRST b-period                                                                             */
/*         WHERE b-period.company EQ cocode                                                            */
/*           AND b-period.yr      EQ period.yr                                                         */
/*         NO-LOCK NO-ERROR.                                                                           */
/*     start-date = IF AVAIL b-period THEN b-period.pst ELSE ?.                                        */
/*                                                                                                     */
/*     FIND LAST b-period                                                                              */
/*         WHERE b-period.company EQ cocode                                                            */
/*           AND b-period.yr      EQ period.yr                                                         */
/*         NO-LOCK NO-ERROR.                                                                           */
/*     end-date = IF AVAIL b-period THEN b-period.pend ELSE ?.                                         */
/*                                                                                                     */
/*     /* Cust Processing  */                                                                          */
/*     IF start-date NE ? AND end-date NE ? THEN                                                       */
/*     for each cust where cust.company eq cocode transaction:                                         */
/*                                                                                                     */
/*       status default "Please Wait...Updating Customer: " + trim(cust.cust-no).                      */
/*                                                                                                     */
/*       cust.lyr-sales = 0.                                                                           */
/*                                                                                                     */
/*       for each ar-ledger                                                                            */
/*           where ar-ledger.company eq cocode                                                         */
/*             and ar-ledger.tr-date ge start-date                                                     */
/*             and ar-ledger.tr-date le end-date                                                       */
/*             and ar-ledger.cust-no eq cust.cust-no                                                   */
/*             and ar-ledger.ref-num begins "INV#"                                                     */
/*           no-lock:                                                                                  */
/*                                                                                                     */
/*         find first ar-inv                                                                           */
/*             where ar-inv.company eq cocode                                                          */
/*               and ar-inv.cust-no eq cust.cust-no                                                    */
/*               and ar-inv.posted  eq yes                                                             */
/*               and ar-inv.inv-no eq int(substr(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))       */
/*             USE-INDEX posted no-lock no-error.                                                      */
/*                                                                                                     */
/*         if avail ar-inv THEN do:                                                                    */
/*           for each ar-invl where ar-invl.company eq cocode and                                      */
/*                                  ar-invl.cust-no eq ar-inv.cust-no and                              */
/*                                  ar-invl.inv-no eq ar-inv.inv-no                                    */
/*                            use-index inv-no no-lock:                                                */
/*             if ar-invl.amt-msf ne 0 then                                                            */
/*               assign cust.lyytd-msf = cust.lyytd-msf + ar-invl.amt-msf                              */
/*                      cust.ytd-msf = cust.ytd-msf - ar-invl.amt-msf.                                 */
/*             ELSE do:                                                                                */
/*               find first itemfg where itemfg.company eq cocode and                                  */
/*                                       itemfg.i-no eq ar-invl.i-no                                   */
/*                                 use-index i-no no-lock no-error.                                    */
/*               if avail itemfg then                                                                  */
/*                 assign cust.lyytd-msf = cust.lyytd-msf +                                            */
/*                                         ((ar-invl.inv-qty / 1000) * itemfg.t-sqft)                  */
/*                        cust.ytd-msf   = cust.ytd-msf -                                              */
/*                                         ((ar-invl.inv-qty / 1000) * itemfg.t-sqft).                 */
/*             end.                                                                                    */
/*           end.                                                                                      */
/*                                                                                                     */
/*/*         assign cust.sales[6] = cust.sales[6] + (ar-inv.net - ar-inv.tax-amt)                      */
/*                  cust.sales[13] = cust.sales[13] - (ar-inv.net - ar-inv.tax-amt)                    */
/**/                                                                                                   */
/*           assign cust.lyr-sales = cust.lyr-sales + (ar-inv.net - ar-inv.tax-amt)                    */
/*                  cust.ytd-sales = cust.ytd-sales - (ar-inv.net - ar-inv.tax-amt)                    */
/*                  cust.cost[6]  = cust.cost[6] + ar-inv.t-cost                                       */
/*                  cust.cost[5] = cust.cost[5] - ar-inv.t-cost                                        */
/*                  cust.comm[6]  = cust.comm[6] + ar-inv.t-comm                                       */
/*                  cust.comm[5] = cust.comm[5] - ar-inv.t-comm.                                       */
/*         end. /* if avail ar-inv */                                                                  */
/*       end. /* for each ar-ledger INV */                                                             */
/*                                                                                                     */
/*       for each ar-ledger where ar-ledger.company eq cocode and                                      */
/*                                ar-ledger.tr-date ge start-date and                                  */
/*                                ar-ledger.tr-date le end-date and                                    */
/*                                ar-ledger.cust-no eq cust.cust-no and                                */
/*                                ar-ledger.ref-num begins "Memo#" no-lock:                            */
/*                                                                                                     */
/*         find first ar-cash where ar-cash.company eq cocode and                                      */
/*                                  ar-cash.cust-no eq cust.cust-no and                                */
/*                                  ar-cash.posted and                                                 */
/*                                  ar-cash.check-no eq int(substr(ar-ledger.ref-num,6,8))             */
/*                            USE-INDEX posted no-lock no-error.                                       */
/*                                                                                                     */
/*         for each ar-cashl where ar-cashl.company eq cocode and                                      */
/*                                 ar-cashl.c-no eq ar-cash.c-no                                       */
/*                           use-index c-no no-lock:                                                   */
/*/*         assign cust.sales[6] = cust.sales[6] +                                                    */
/*                                  (ar-cashl.amt-paid - ar-cashl.amt-disc)                            */
/*                  cust.sales[5] = cust.sales[5] -                                                    */
/*                                  (ar-cashl.amt-paid - ar-cashl.amt-disc).                           */
/**/                                                                                                   */
/*           assign cust.lyr-sales = cust.lyr-sales +                                                  */
/*                                   (ar-cashl.amt-paid - ar-cashl.amt-disc)                           */
/*                  cust.ytd-sales = cust.ytd-sales -                                                  */
/*                                   (ar-cashl.amt-paid - ar-cashl.amt-disc).                          */
/*         end.                                                                                        */
/*       end. /* for each ar-ledger MEMO */                                                            */
/*     end. /* for each cust */                                                                        */
/*                                                                                                     */
/*     /* Vend Processing  */                                                                          */
/*     IF start-date NE ? AND end-date NE ? THEN                                                       */
/*     for each vend where vend.company eq cocode transaction:                                         */
/*                                                                                                     */
/*       status default "Please Wait...Updating Vendor: " + trim(vend.vend-no).                        */
/*                                                                                                     */
/*       vend.last-year = 0.                                                                           */
/*                                                                                                     */
/*       for each ap-ledger where ap-ledger.company eq cocode and                                      */
/*                                ap-ledger.tr-date ge start-date and                                  */
/*                                ap-ledger.tr-date le end-date and                                    */
/*                                ap-ledger.vend-no eq vend.vend-no and                                */
/*                                ap-ledger.refnum begins "INV#" no-lock:                              */
/*                                                                                                     */
/*         find first ap-inv where ap-inv.company eq cocode and                                        */
/*                                 ap-inv.vend-no eq vend.vend-no and                                  */
/*                                 ap-inv.posted  eq yes and                                           */
/*                                 ap-inv.inv-no eq substr(ap-ledger.refnum,6,length(ap-ledger.refnum))*/
/*                           USE-INDEX ap-inv no-lock no-error.                                        */
/*                                                                                                     */
/*         if avail ap-inv THEN do:                                                                    */
/*           FOR each ap-invl where ap-invl.company eq cocode and                                      */
/*                                  ap-invl.inv-no eq ap-inv.inv-no and                                */
/*                                  ap-invl.i-no eq ap-inv.i-no                                        */
/*                            use-index i-no no-lock:                                                  */
/*             if ap-invl.amt-msf ne 0 then                                                            */
/*               assign vend.lyytd = vend.lyytd + ap-invl.amt-msf                                      */
/*                      vend.ytd-msf = vend.ytd-msf - ap-invl.amt-msf.                                 */
/*             else do:                                                                                */
/*               find first itemfg where itemfg.company eq cocode and                                  */
/*                                       itemfg.i-no eq string(ap-invl.i-no)                           */
/*                                 use-index i-no no-lock no-error.                                    */
/*               if avail itemfg then                                                                  */
/*                 assign vend.lyytd   = vend.lyytd +                                                  */
/*                                       ((ap-invl.qty / 1000) * itemfg.t-sqft)                        */
/*                        vend.ytd-msf = vend.ytd-msf -                                                */
/*                                       ((ap-invl.qty / 1000) * itemfg.t-sqft).                       */
/*             end.                                                                                    */
/*           end.                                                                                      */
/*         end. /* if avail ap-inv */                                                                  */
/*                                                                                                     */
/*         assign vend.purch[13] = vend.purch[13] - ap-ledger.amt                                      */
/*                vend.last-year = vend.last-year + ap-ledger.amt.                                     */
/*       end. /* for each ap-ledger INV */                                                             */
/*                                                                                                     */
/*       for each ap-ledger where ap-ledger.company eq cocode and                                      */
/*                                ap-ledger.tr-date ge start-date and                                  */
/*                                ap-ledger.tr-date le end-date and                                    */
/*                                ap-ledger.vend-no eq vend.vend-no and                                */
/*                                (ap-ledger.refnum begins "Memo#" or                                  */
/*                                 ap-ledger.refnum begins "Chk#") no-lock:                            */
/*                                                                                                     */
/*         assign vend.purch[13] = vend.purch[13] - ap-ledger.amt                                      */
/*                vend.last-year = vend.last-year + ap-ledger.amt.                                     */
/*       end. /* for each ap-ledger MEMO */                                                            */
/*     end. /* for each vend */                                                                        */
/*                                                                                                     */
/*     status default "".                                                                              */
   end.

   do transaction:
      find first period
          where period.company eq cocode
            AND period.yr      EQ tran-year
            and period.pnum    eq uperiod
            and period.pstat   eq yes
          exclusive-lock.
      period.pstat = false.
      if period.pnum eq company.num-per then company.yend-per = NO.
      iClosedPeriod = period.pnum.
   end.

   find next period
       where period.company eq cocode
         and period.pstat   eq yes
       no-lock.
   if avail period then ASSIGN tran-period = period.pnum
                               uperiod = period.pnum.
                               
   IF lLastPeriod THEN
   DO:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:
          ASSIGN iTransNum = gl-ctrl.trnum + 1.
                 gl-ctrl.trnum = iTransNum.
       END.          
       FIND CURRENT gl-ctrl NO-LOCK NO-ERROR .
                  
       RUN pGetNetIncome(INPUT gl-ctrl.contra, OUTPUT dNetIncome). 
       RUN GL_SpCreateGLHist(cocode,
                             gl-ctrl.retainedEarnings,
                             "AutoClose",
                             STRING("Auto Posted Net Income for ") + STRING(period.yr),
                             period.pst,
                             dNetIncome,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
                             iTransNum,
                             period.pnum,
                             "A",
                             date(TODAY),
                             "",
                             "").        
       RUN GL_SpCreateGLHist(cocode,
                             gl-ctrl.ret,
                             "AutoClose",
                             STRING("Auto Posted Current Years Earnings for ") + STRING(period.yr),
                             period.pst,
                             - dNetIncome,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
                             iTransNum,
                             period.pnum,
                             "A",
                             date(TODAY),
                             "",
                             "").                      
   END.    
    
   SESSION:SET-WAIT-STATE ("").
   RUN spProgressBar ("Close Period", 1, 1).
   message "Period " string(iClosedPeriod) " is closed." SKIP
   "Current accounting period changed to " uperiod SKIP
   "Closing G/L Period is completed." VIEW-AS ALERT-BOX INFO.

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
ASSIGN v-msg1:HIDDEN IN FRAME {&FRAME-NAME} = NO
          v-msg2:HIDDEN = NO
       v-msg1:BGCOLOR = 4
          v-msg1 = "Processing... Please wait and do not cancel out of screen!". 
   DISPLAY v-msg1 WITH FRAME {&FRAME-NAME}.

 for each glhist
       where glhist.company eq cocode
         and glhist.tr-date ge period.pst
         and glhist.tr-date le period.pend
         and glhist.period  eq uperiod
         AND glhist.posted  EQ NO 
    :

    ASSIGN v-msg2 = jrnl + "Period: "  + 
           string(glhist.period) + "Act: " + glhist.actnum.
    DISP v-msg2 WITH FRAME {&FRAME-NAME}.
    .

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

  /* /*Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
    */
     run custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt). /* open file-name, title */ 

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
    run scr-rpt.w (list-name,c-win:title,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
 DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

 RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
 IF tb_excel THEN
 DO:
     OUTPUT STREAM excel TO VALUE(cFileName).
     excelheader = "Account,Description,Reason" .
     PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
 END.

 
form account.actnum label "Account Number"
     account.dscr   label "Account Description"
     glhist.jrnl   label " Journal "
     glhist.tr-amt format "(>>>,>>>,>>>,>>9.99)" label "Transaction"
     open-amt       label "Account Balance"
    with frame r-mclo down width 132 no-box column 10 STREAM-IO.

 {sys/form/r-topw.f}

 {sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(lines-per-page)}

 IF td-show-parm THEN RUN show-param.
 SESSION:SET-WAIT-STATE("general").
 ASSIGN uperiod = tran-period .
        
 find first period                   
        where period.company eq cocode
            AND period.yr   EQ tran-year
            AND period.pnum EQ tran-period
        no-lock no-error.


 str-tit  = coname + " - " + loname.
 str-tit2 = "MONTHLY SUMMARY & G/L CLOSING" .
 str-tit3 = "Period " + string(uperiod,"99") + " - " +
               string(period.pst) + " to " + string(period.pend).
 x = (112 - length(str-tit)) / 2.
 str-tit  = fill(" ",x) + str-tit .
 x = (114 - length(str-tit2)) / 2.
 str-tit2 = fill(" ",x) + str-tit2 .
 x = (132 - length(str-tit3)) / 2.
 str-tit3 = fill(" ",x) + str-tit3 .

   display str-tit3 format "x(130)" skip(1) with frame r-top.

   SESSION:SET-WAIT-STATE ("general").
 IF tb_excel THEN
 DO:
   for each account where account.company eq cocode NO-LOCK :
       IF tb_inact AND account.inactive THEN do:
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
 for each account where account.company eq cocode no-lock with frame r-mclo:
       
       IF tb_out-bal THEN DO:

       END.
    IF tb_excel THEN
    DO:
       IF tb_invalid-period THEN DO:
          FOR EACH glhist no-lock 
              where glhist.company eq cocode
              and glhist.actnum  eq account.actnum
              and glhist.tr-date ge period.pst
              and glhist.tr-date le period.pend
              and glhist.period EQ 0
              AND glhist.posted EQ NO BREAK BY glhist.actnum:
              
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
           FOR EACH glhist no-lock 
              where glhist.company eq cocode
              and glhist.actnum  eq account.actnum
              and glhist.tr-date LT period.pst
              and glhist.tr-date GT period.pend
              and glhist.period EQ uperiod
              AND glhist.posted EQ NO BREAK BY glhist.actnum:
              
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
           FOR EACH glhist no-lock 
              where glhist.company eq cocode
              and glhist.actnum  eq account.actnum
              and glhist.tr-date LT period.pst
              AND glhist.posted EQ NO:
              
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

      if line-counter gt page-size - 3 then page.
      open-amt = account.cyr-open.
      do i = 1 to uperiod:
         open-amt = open-amt + cyr[i].
      end.
      find first glhist
          where glhist.company eq cocode
            and glhist.actnum  eq account.actnum
            and glhist.tr-date ge period.pst
            and glhist.tr-date le period.pend
            and glhist.period  eq uperiod
            AND glhist.posted  EQ NO
          no-lock no-error.
      if open-amt eq 0 and not avail glhist then next.
      display account.actnum
              account.dscr
              open-amt.
      down.
      tot-all = tot-all + open-amt.

      for each glhist no-lock
          where glhist.company eq account.company
            and glhist.actnum  eq account.actnum
            and glhist.tr-date ge period.pst
            and glhist.tr-date le period.pend
            and glhist.period  eq uperiod
            AND glhist.posted  EQ NO
          break by glhist.jrnl with frame r-mclo:

           

         if line-counter gt page-size - 2 then page.

         assign
          tot-tx   = tot-tx   + tr-amt
          tot-all  = tot-all  + tr-amt
          tot-jrnl = tot-jrnl + tr-amt
          tot-act  = tot-act  + tr-amt.

         if last-of(glhist.jrnl) then do:
            display "" @ account.actnum
                    "" @ account.dscr
                    glhist.jrnl
                    tot-jrnl @ glhist.tr-amt
                   "" @ open-amt.
            tot-jrnl = 0.
            down.
         end.
      end. /* each glhist */

      display "" @ account.actnum
              "" @ account.dscr
              "" @ glhist.jrnl
              tot-act @ glhist.tr-amt
              (tot-act + open-amt) format "->>>,>>>,>>>,>>9.99" @ open-amt
              "*" with frame r-mclo.
      down 2.
      tot-act = 0.
   end. /* each account */

   display "" @ account.actnum
           "" @ account.dscr
           "TOTAL" @ glhist.jrnl
           tot-tx  @ glhist.tr-amt
           tot-all @ open-amt
           with frame r-mclo.

   IF tb_excel THEN
   DO:
       OUTPUT STREAM excel CLOSE.
       IF tb_runExcel THEN 
       OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).

   END.

 SESSION:SET-WAIT-STATE("").

end procedure.

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

find first b-cacct NO-LOCK
            where b-cacct.company eq cocode
            and b-cacct.actnum  eq ipcAccount NO-ERROR. 
 IF index("RE",b-cacct.type) gt 0 then do:           
     DO i = 1 TO company.num-per:
       opdNetAmount = opdNetAmount + b-cacct.cyr[i].
     END.
 END.
 
            
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
           ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
               assign parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:help + "," 
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

