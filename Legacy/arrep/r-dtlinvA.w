&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:

  Created:

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

{sys/inc/custlistform.i ""AL3"" }

{sys/ref/CustList.i NEW}

DEF TEMP-TABLE tt-report FIELD actnum LIKE account.actnum
                         FIELD cust-no LIKE cust.cust-no
                         FIELD inv-no LIKE ar-inv.inv-no
                         FIELD jrnl LIKE gltrans.jrnl
                         FIELD tr-date LIKE ar-ledger.tr-date
                         FIELD tr-num LIKE ar-ledger.tr-num
                         FIELD amt AS DEC
                         FIELD po-no LIKE ar-inv.po-no
                         FIELD lot-no LIKE ar-invl.lot-no
                         FIELD i-no LIKE ar-invl.i-no
                         FIELD i-name LIKE ar-invl.i-name
                         INDEX detail actnum cust-no inv-no jrnl.

DEF STREAM excel.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust end_cust begin_date end_date begin_acct end_acct rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_date end_date begin_acct end_acct rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-iname C-Win 
FUNCTION get-iname RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(20)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-dtlinv.csv" 
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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

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
     tb_cust-list AT ROW 2.05 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 2.05 COL 64.2 WIDGET-ID 8
     begin_cust AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 3.38 COL 63 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 4.81 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_date AT ROW 4.81 COL 63 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_acct AT ROW 6.24 COL 19 COLON-ALIGNED HELP
          "Enter Beginning GL Account Number"
     end_acct AT ROW 6.24 COL 63 COLON-ALIGNED HELP
          "Enter Ending GL Account Number"
     rd-dest AT ROW 10.76 COL 6 NO-LABEL
     lv-ornt AT ROW 11 COL 30 NO-LABEL
     lines-per-page AT ROW 11 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 12.67 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 13.62 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.57 COL 29
     tb_excel AT ROW 15.62 COL 49 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.62 COL 51
     fi_file AT ROW 16.57 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.14 COL 19
     btn-cancel AT ROW 18.14 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.05 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 9.57 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.


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
         TITLE              = "AR Detail Invoice Report"
         HEIGHT             = 19.76
         WIDTH              = 96.6
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* AR Detail Invoice Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AR Detail Invoice Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
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
   
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT END_cust).
  END.       
  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
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


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
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

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust.
  END.
  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'AL3',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

  IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'AL3',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'AL3').
    

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
  DISPLAY tb_cust-list begin_cust end_cust begin_date end_date begin_acct 
          end_acct rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust end_cust begin_date 
         end_date begin_acct end_acct rd-dest lv-ornt lines-per-page lv-font-no 
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
{sys/form/r-topw.f}   

DEF VAR lv-jrnl LIKE gltrans.jrnl NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR lv-amt LIKE tt-report.amt EXTENT 3 NO-UNDO.
DEF VAR ld-tax-rate AS DEC  EXTENT 4 NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
def var v-ttl-tax  as decimal no-undo.
def var v-ttl-rate as decimal no-undo.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEF VAR fcust AS CHAR NO-UNDO .
DEF VAR tcust AS CHAR NO-UNDO .
{custom/statusMsg.i " 'Processing...  '"}

    ASSIGN
    lSelected  = tb_cust-list
    fcust      = begin_cust
    tcust      = END_cust  .

FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
     SPACE(5)
     /* account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)" */
     tt-report.cust-no     COLUMN-LABEL "Customer"      
     /*cust.name             COLUMN-LABEL "Name"        FORMAT "x(20)" */
     tt-report.inv-no      COLUMN-LABEL "Inv#"
     tt-report.tr-date     COLUMN-LABEL "Date"
     /*tt-report.jrnl        COLUMN-LABEL "Journal" */
     tt-report.po-no        COLUMN-LABEL "PO#"
     tt-report.lot-no      COLUMN-LABEL "Customer Lot#"
     tt-report.i-no        COLUMN-LABEL "Item#"
     /*tt-report.i-name      COLUMN-LABEL "Item Description" */
     tt-report.tr-num      COLUMN-LABEL "Run#"     
     tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"
   
    WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


SESSION:SET-WAIT-STATE ("general").

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}. 

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    excelHeader = 'GL Acct#,Description,Customer,Cust Name,Inv#,PO#,Customer Lot#,Item#,Item Desc,Run#,Date,Amount'.
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
  END. /* if tb_excel */
  IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
  END.
IF td-show-parm THEN RUN show-param.

FOR EACH tt-report:
  DELETE tt-report.
END.

DISPLAY "" WITH FRAME r-top.
FOR EACH ar-ledger
    WHERE ar-ledger.company EQ cocode
      /*AND ar-ledger.cust-no GE begin_cust
      AND ar-ledger.cust-no LE end_cust*/
      AND ar-ledger.cust-no  GE fcust
      AND ar-ledger.cust-no  LE tcust
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-ledger.cust-no
      AND ttCustList.log-fld no-lock) else true)
      AND ar-ledger.cust-no NE ""
      AND ar-ledger.tr-date GE begin_date
      AND ar-ledger.tr-date LE end_date
    NO-LOCK:

    {custom/statusMsg.i " 'Processing Customer#  '  + ar-ledger.cust-no "}

  IF ar-ledger.ref-num BEGINS "INV# " THEN DO:
    FIND FIRST ar-inv
        WHERE ar-inv.company EQ ar-ledger.company
          AND ar-inv.inv-no  EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
        NO-LOCK NO-ERROR.

    IF AVAIL ar-inv THEN DO:
      lv-jrnl = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt
                THEN "ARINV" ELSE "OEINV".
      
      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ar-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ar-ctrl.receivables
       tt-report.amt    = (IF lv-jrnl EQ "ARINV"
                           THEN ar-inv.net ELSE ar-inv.gross) * -1.
          
      IF ar-inv.tax-amt NE 0 THEN DO:
        RELEASE stax.
        IF ar-inv.tax-code NE "" THEN
        FIND FIRST stax
            WHERE stax.company   EQ ar-inv.company
              AND stax.tax-group EQ ar-inv.tax-code
            NO-LOCK NO-ERROR.
             
        IF AVAIL stax THEN DO:
          assign v-ttl-tax  = 0
                 v-ttl-rate = 0.

     
          DO li = 1 TO extent(stax.tax-rate1): 
           
            if stax.tax-rate1[li] = 0 then next.
           
            ld-tax-rate[li] = stax.tax-rate1[li].
           
            IF stax.accum-tax AND li GT 1 THEN
           DO lj = 1 TO li - 1: 
            
              ld-tax-rate[li] = ld-tax-rate[li] +
                                (ld-tax-rate[li] * (stax.tax-rate[lj] / 100)).
            END.
            v-ttl-rate = v-ttl-rate + ld-tax-rate[li].
          END.
              
          DO li = 1 TO extent(stax.tax-rate1):
            if stax.tax-rate1[li] = 0 then next.
            ASSIGN ld-tax-rate[li] = ROUND(ld-tax-rate[li] / v-ttl-rate *
                                           ar-inv.tax-amt,2)
                   v-ttl-tax = v-ttl-tax + ld-tax-rate[li].
          END.
              
              
          IF ar-inv.tax-amt NE v-ttl-tax THEN
            ld-tax-rate[1] = ld-tax-rate[1] +
                             (ar-inv.tax-amt - v-ttl-tax).
             
          DO li = 1 TO extent(stax.tax-rate1):
            if stax.tax-rate1[li] = 0 then next.
            CREATE tt-report.
            ASSIGN 
             tt-report.inv-no = ar-inv.inv-no
             tt-report.jrnl   = lv-jrnl
             tt-report.actnum = stax.tax-acc1[li]
             tt-report.amt    = ld-tax-rate[li]
             tt-report.po-no  = ar-inv.po-no.
          END. /* 1 to 3 */
        END. /* avail stax */

        ELSE DO:
          CREATE tt-report.
          ASSIGN 
           tt-report.inv-no = ar-inv.inv-no
           tt-report.jrnl   = lv-jrnl
           tt-report.actnum = ar-ctrl.stax
           tt-report.amt    = ar-inv.tax-amt.
        END.
      END.

      IF ar-inv.f-bill THEN DO:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no = ar-inv.inv-no
         tt-report.jrnl   = lv-jrnl
         tt-report.actnum = ar-ctrl.freight
         tt-report.amt    = ar-inv.freight.
      END.

      FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no NO-LOCK:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ar-inv.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ar-invl.actnum
         tt-report.amt     = ar-invl.amt
         tt-report.po-no   = ar-inv.po-no
         tt-report.lot-no  = ar-invl.lot-no
         tt-report.i-no    = IF ar-invl.i-no GT "" THEN ar-invl.i-no
                             ELSE ar-invl.i-dscr.
         tt-report.i-name  = get-iname().
      END.
    END.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "Memo#" THEN DO:    
    FOR EACH ar-cash
        WHERE ar-cash.company  EQ ar-ledger.company
          AND ar-cash.posted   EQ YES
          AND ar-cash.memo     EQ YES
          AND ar-cash.cust-no  EQ ar-ledger.cust-no
          AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,8))
        NO-LOCK,

        FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK:
      /* gdm - 09240903 */
      lv-jrnl = IF ar-cashl.amt-paid - ar-cashl.amt-disc GT 0
                THEN "DBMEM" ELSE "CRMEM".
              
      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-ctrl.receivables
       tt-report.amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc) * -1
/*        tt-report.jrnl    = IF tt-report.amt < 0        */
/*                              THEN "DBMEM" ELSE "CRMEM" */
                                 .

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-cashl.actnum
       tt-report.amt     = ar-cashl.amt-paid - ar-cashl.amt-disc
/*        tt-report.jrnl    = IF tt-report.amt < 0        */
/*                              THEN "DBMEM" ELSE "CRMEM" */
                                 .
    END.
  END.

  ELSE
  IF ar-ledger.ref-num BEGINS "CHK# " THEN DO:
    lv-jrnl = "CASHR".

    FOR EACH ar-cash
        WHERE ar-cash.company  EQ ar-ledger.company
          AND ar-cash.posted   EQ YES
          AND ar-cash.memo     EQ NO
          AND ar-cash.cust-no  EQ ar-ledger.cust-no
          AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
        NO-LOCK :

      /*  FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK: */
        for each ar-cashl where ar-cashl.c-no = ar-cash.c-no NO-LOCK
         break by ar-cashl.inv-no :

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-ctrl.receivables
       tt-report.amt     = ar-cashl.amt-paid + ar-cashl.amt-disc.

      CREATE tt-report.
      BUFFER-COPY ar-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ar-cashl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ar-cashl.actnum
       tt-report.amt     = ar-cashl.amt-paid * -1.

      IF ar-cashl.amt-disc NE 0 THEN DO:
        CREATE tt-report.
        BUFFER-COPY ar-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ar-cashl.inv-no
         tt-report.jrnl    = "CRDIS"
         tt-report.actnum  = ar-ctrl.discount
         tt-report.amt     = ar-cashl.amt-disc * -1.
      END.
    END.
  END.
END.
END.

FOR EACH tt-report
    WHERE tt-report.actnum GE begin_acct
      AND tt-report.actnum LE end_acct
      AND tt-report.actnum NE ""
      AND tt-report.amt    NE 0
    USE-INDEX detail
    BREAK BY tt-report.actnum
          BY tt-report.cust-no
          BY tt-report.inv-no
          BY tt-report.jrnl:

    {custom/statusMsg.i " 'Processing Customer#  '  + tt-report.cust-no "}

  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.actnum
      NO-LOCK NO-ERROR.

  FIND FIRST cust
      WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.cust-no
      NO-LOCK NO-ERROR.

  DISPLAY tt-report.actnum WHEN FIRST-OF(tt-report.actnum)
        /*  account.dscr WHEN AVAIL account AND FIRST-OF(tt-report.actnum)
              "Not on File" WHEN NOT AVAIL account AND FIRST-OF(tt-report.actnum)
                @ account.dscr */
          tt-report.cust-no WHEN FIRST-OF(tt-report.cust-no)
          /*cust.name WHEN AVAIL cust AND FIRST-OF(tt-report.cust-no)
              "Not on File" WHEN NOT AVAIL cust AND FIRST-OF(tt-report.cust-no)
                @ cust.name*/
          tt-report.inv-no
         /* tt-report.jrnl */
          tt-report.po-no
          tt-report.lot-no
          tt-report.i-no
         /* tt-report.i-name */
          tt-report.tr-num
          tt-report.tr-date
          tt-report.amt
      WITH FRAME detail.
  DOWN WITH FRAME detail.

  IF FIRST-OF(tt-report.actnum) AND AVAIL(account) THEN
      PUT account.dscr FORMAT "x(28)".

  IF FIRST-OF(tt-report.cust-no) AND AVAIL(cust) THEN
      PUT cust.NAME AT 31.

  /*IF FIRST-OF(tt-report.actnum) OR FIRST-OF(tt-report.cust-no) THEN */

  PUT tt-report.i-name AT 90 SKIP.

  IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
       '"' (IF FIRST-OF(tt-report.actnum) THEN tt-report.actnum ELSE "") '",'
       '"' (IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN account.dscr
            ELSE IF NOT AVAIL account AND FIRST-OF(tt-report.actnum) THEN
                "Not on File"
            ELSE "")                              '",'
       '"'  (IF FIRST-OF(tt-report.cust-no) THEN tt-report.cust-no
             ELSE "")                                                   '",'
      '"'  (IF FIRST-OF(tt-report.cust-no) AND AVAIL(cust) THEN cust.NAME
             ELSE "")                                                   '",'
       '"' tt-report.inv-no                                             '",' 
       '"' tt-report.po-no                                              '",'
       '"' tt-report.lot-no                                             '",'
       '"' tt-report.i-no                                               '",'
       '"' tt-report.i-name                                             '",'
       '"' tt-report.tr-num                                             '",'
       '"' (IF tt-report.tr-date NE ? THEN
            STRING(tt-report.tr-date,"99/99/9999") ELSE "")             '",'
       '"' STRING(tt-report.amt,'->>,>>>,>>9.99')                       '",'
      SKIP.

  lv-amt[1] = lv-amt[1] + tt-report.amt.

  IF LAST-OF(tt-report.cust-no) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "Customer" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[1]  @ tt-report.amt.
    DOWN.

/*     IF tb_excel THEN                                 */
/*       PUT STREAM excel UNFORMATTED                   */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' "Customer"                         '",' */
/*          '"' "Totals"                           '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' STRING(lv-amt[1],'->>,>>>,>>9.99') '",' */
/*         SKIP.                                        */

    ASSIGN
     lv-amt[2] = lv-amt[2] + lv-amt[1]
     lv-amt[1] = 0.

    PUT SKIP(1).
  END.

  IF LAST-OF(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Acct#" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[2]  @ tt-report.amt.
    DOWN.

/*     IF tb_excel THEN                                 */
/*       PUT STREAM excel UNFORMATTED                   */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' "Acct#"                            '",' */
/*          '"' "Totals"                           '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' STRING(lv-amt[2],'->>,>>>,>>9.99') '",' */
/*         SKIP.                                        */

    ASSIGN
     lv-amt[3] = lv-amt[3] + lv-amt[2]
     lv-amt[2] = 0.

    PUT SKIP(3).
  END.

  IF LAST(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Grand" @ tt-report.cust-no
            "Totals"   @ tt-report.inv-no
            lv-amt[3]  @ tt-report.amt.
    DOWN.

/*     IF tb_excel THEN                                 */
/*       PUT STREAM excel UNFORMATTED                   */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' "Grand Totals"                     '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' ""                                 '",' */
/*          '"' STRING(lv-amt[3],'->>,>>>,>>9.99') '",' */
/*         SKIP.                                        */
  END.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
  OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-iname C-Win 
FUNCTION get-iname RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF AVAIL ar-invl THEN (IF ar-invl.i-name EQ "" THEN
           ar-invl.part-dscr1 ELSE ar-invl.i-name) ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

