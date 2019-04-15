&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\prnchk.w

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

{ap/ap-chk.i NEW }
FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
def var laser-list as char no-undo
   init "l,Laser,Raritan,Hartford,ASILaser,Laser,TriadLas,ASI2000,AllLaser,Argvlas,Action,AllLaserHP1536DN".    /* Task 09301303*/
DEFINE VARIABLE laser-list-ach AS CHARACTER NO-UNDO 
    INIT "ASI,Badger".

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.    

def var next-program as char no-undo init "ap/ap-chks.p".    /* std */
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR num-of-chks AS INT NO-UNDO.
DEF VAR sel-per-chk AS INT NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF BUFFER b-ap-chk FOR ap-chk.
DEF BUFFER b-vend FOR vend.
DEF VAR ll-is-xprint-form  AS LOG  NO-UNDO INIT FALSE.
DEFINE VARIABLE giCheckNoStd AS INTEGER NO-UNDO.
DEFINE VARIABLE giCheckNoACH AS INTEGER NO-UNDO.

DEFINE VARIABLE glRemittance AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcRemittanceDefault AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcPDFFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcDefVend AS CHARACTER   NO-UNDO.

{custom/xprint.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-9 check-date bank-code ~
start_check-no tb_ach begin_vend-no end_vend-no rd-dest td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS check-date bank-code bank-name ~
start_check-no tb_ach begin_vend-no end_vend-no rd-dest td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ACHEmailOK C-Win 
FUNCTION ACHEmailOK RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ACHRunOK C-Win 
FUNCTION ACHRunOK RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE bank-code AS CHARACTER FORMAT "X(8)" 
     LABEL "Bank Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE bank-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE check-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Check Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE start_check-no AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Starting Check#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
"To Port Directly", 4,
"To Email", 5
     SIZE 23 BY 6.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 11.43.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 3.81.

DEFINE VARIABLE tb_ach AS LOGICAL INITIAL no 
     LABEL "Electronic-only Check Run" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     check-date AT ROW 3.62 COL 28.8 COLON-ALIGNED
     bank-code AT ROW 4.57 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     bank-name AT ROW 4.57 COL 46 COLON-ALIGNED NO-LABEL
     start_check-no AT ROW 5.52 COL 29 COLON-ALIGNED HELP
          "Enter Check Number"
     tb_ach AT ROW 5.62 COL 49 WIDGET-ID 2
     begin_vend-no AT ROW 8.86 COL 44 COLON-ALIGNED HELP
          "Enter Beginning Vendor  Number"
     end_vend-no AT ROW 9.81 COL 44 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     rd-dest AT ROW 14.1 COL 5 NO-LABEL
     lv-ornt AT ROW 14.1 COL 34.6 NO-LABEL
     lines-per-page AT ROW 14.1 COL 81 COLON-ALIGNED
     lv-font-no AT ROW 15.29 COL 38 COLON-ALIGNED
     lv-font-name AT ROW 16.43 COL 32 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.1 COL 34
     btn-ok AT ROW 21 COL 19
     btn-cancel AT ROW 21 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.43 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 4
     "REPRINT OPTIONS" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 7.67 COL 35
          FGCOLOR 9 FONT 6
     RECT-6 AT ROW 12.81 COL 2
     RECT-7 AT ROW 1.24 COL 2
     RECT-9 AT ROW 7.91 COL 23
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
         TITLE              = "Print A/P Checks"
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       bank-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN bank-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       check-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       start_check-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print A/P Checks */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print A/P Checks */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank-code C-Win
ON HELP OF bank-code IN FRAME FRAME-A /* Bank Code */
DO:
  DEF VAR char-val AS cha NO-UNDO.


  RUN windows/l-bank.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT char-val).
  IF char-val <> "" THEN
    ASSIGN
     FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
     bank-name:SCREEN-VALUE = ENTRY(2,char-val)
     START_check-no:SCREEN-VALUE = STRING(INT(ENTRY(3,char-val)) + 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank-code C-Win
ON LEAVE OF bank-code IN FRAME FRAME-A /* Bank Code */
DO:
  {VALIDATE/bank.i bank-code bank-name}
  ASSIGN 
     giCheckNoStd = bank.last-chk + 1
     giCheckNoACH = bank.spare-int-1 + 1.
  IF giCheckNoACH EQ 1 THEN  giCheckNoACH = 800001.

  IF tb_ach
      THEN
        ASSIGN 
            START_check-no = giCheckNoACH
            START_check-no:SCREEN-VALUE = STRING(giCheckNoACH).

      ELSE
        ASSIGN 
            START_check-no = giCheckNoStd
            START_check-no:SCREEN-VALUE = STRING(giCheckNoStd).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
DO:
   assign {&self-name}.
   ASSIGN end_vend-no:SCREEN-VALUE = begin_vend-no:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON VALUE-CHANGED OF begin_vend-no IN FRAME FRAME-A /* Check Date */
DO:
  ASSIGN end_vend-no:SCREEN-VALUE = begin_vend-no:SCREEN-VALUE.
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
    DEFINE VARIABLE cRemitFormat AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lRemit AS LOGICAL     NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bank NO-LOCK
            WHERE bank.company   EQ cocode
              AND bank.bank-code EQ bank-code:SCREEN-VALUE
            NO-ERROR.
        IF NOT AVAIL bank THEN DO:
          MESSAGE "Invalid bank, try help..."
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO bank-code.
          RETURN NO-APPLY.
        END.

        IF INT(start_check-no:SCREEN-VALUE) EQ 0 THEN 
            IF tb_ach 
                THEN
                    start_check-no:SCREEN-VALUE = STRING(bank.spare-int-1 + 1).
                ELSE
                    start_check-no:SCREEN-VALUE = STRING(bank.last-chk + 1).

        ASSIGN {&displayed-objects}.

        IF NOT tb_ach AND rd-dest EQ 5 THEN DO:
            MESSAGE "Email output only available for Electronic-only Check Run"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO check-date.
            RETURN NO-APPLY.
          END.
  END.

  stnum = START_check-no.

  IF CAN-FIND(FIRST sys-ctrl-shipto
     WHERE sys-ctrl-shipto.company = cocode
     AND sys-ctrl-shipto.NAME = "CHKFMT") 
     AND (NOT glRemittance AND NOT tb_ach) THEN
     DO:
        IF CAN-FIND(FIRST ap-chk
          where ap-chk.company   eq cocode
            and ap-chk.vend-no   ge begin_vend-no
            and ap-chk.vend-no   le end_vend-no
            and ap-chk.man-check eq no
            and can-find(first ap-sel
                         where ap-sel.company   eq cocode
                           and ap-sel.vend-no   eq ap-chk.vend-no
                           and ap-sel.man-check eq no)) THEN
           for each b-ap-chk
              where b-ap-chk.company   eq cocode
                and b-ap-chk.vend-no   ge begin_vend-no
                and b-ap-chk.vend-no   le end_vend-no
                and b-ap-chk.man-check eq no
                and can-find(first ap-sel
                             where ap-sel.company   eq cocode
                               and ap-sel.vend-no   eq b-ap-chk.vend-no
                               and ap-sel.man-check eq no),
                first b-vend
                where b-vend.company eq cocode
                  and b-vend.vend-no eq b-ap-chk.vend-no
                break by b-ap-chk.company
                      BY b-ap-chk.vend-no:

                IF FIRST-OF(b-ap-chk.vend-no) THEN
                DO:
                    FIND FIRST sys-ctrl-shipto
                        WHERE sys-ctrl-shipto.company      = cocode
                          AND sys-ctrl-shipto.NAME         = "CHKFMT"
                          AND sys-ctrl-shipto.cust-vend    = NO
                          AND sys-ctrl-shipto.cust-vend-no = b-ap-chk.vend-no 
                          AND sys-ctrl-shipto.char-fld > '' 
                        NO-LOCK NO-ERROR.
                    IF AVAIL sys-ctrl-shipto THEN
                        RUN SetChkForm (sys-ctrl-shipto.char-fld,NO).
                    ELSE
                        RUN SetChkForm (vcDefaultForm,NO). 

                    RUN run-report(b-ap-chk.vend-no, TRUE).
                    RUN GenerateReport.
                END.
           END. /* FOR EACH */
        ELSE
           MESSAGE "No Checks Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /* if can-find sys-ctrl-shipto*/
  else /*not can-find sys-ctrl-shipto*/
  DO:

   IF glRemittance AND tb_ach THEN DO:

        RUN SetChkForm(INPUT gcRemittanceDefault,
                       INPUT YES).
   END.

     RUN run-report("", FALSE).
     RUN GenerateReport.
  END.
  RUN SetParamDefaults(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME check-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL check-date C-Win
ON LEAVE OF check-date IN FRAME FRAME-A /* Check Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL check-date C-Win
ON VALUE-CHANGED OF check-date IN FRAME FRAME-A /* Check Date */
DO:
  ll-warned = NO.
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


&Scoped-define SELF-NAME start_check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_check-no C-Win
ON LEAVE OF start_check-no IN FRAME FRAME-A /* Starting Check# */
DO:
    IF tb_ach AND INT(START_check-no:SCREEN-VALUE) LT 800000  THEN DO:
        MESSAGE "Electronic Check number must not be less than 800000"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE 
        assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ach C-Win
ON VALUE-CHANGED OF tb_ach IN FRAME FRAME-A /* Electronic-only Check Run */
DO:
    ASSIGN {&SELF-NAME}.
    IF tb_ach AND NOT ACHRunOK() THEN DO:
        MESSAGE "Check selection must include only Vendors with Electronic Payment Type."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        tb_ach:SCREEN-VALUE = "NO".
        tb_ach = NO.
    END.
    ELSE IF tb_ach THEN
        ASSIGN 
            START_check-no:SCREEN-VALUE = STRING(giCheckNoACH)
            START_check-no = giCheckNoACH.
    ELSE 
        ASSIGN 
            START_check-no:SCREEN-VALUE = STRING(giCheckNoStd)
            START_check-no = giCheckNoStd.

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
    RUN SetParamDefaults(INPUT YES).
/*   FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode NO-LOCK.             */
/*   FIND FIRST bank WHERE bank.company = cocode                            */
/*                     AND bank.actnum = ap-ctrl.cash-act NO-LOCK NO-ERROR. */
/*   IF AVAIL bank THEN DO:                                                 */
/*     ASSIGN                                                               */
/*      bank-code      = bank.bank-code                                     */
/*      giCheckNoStd = bank.last-chk + 1                                    */
/*      giCheckNoACH = bank.spare-int-1 + 1.                                */
/*      IF giCheckNoACH EQ 1 THEN  giCheckNoACH = 800001.                   */
/*      IF ACHRunOK() THEN                                                  */
/*         ASSIGN                                                           */
/*             start_check-no = giCheckNoACH                                */
/*             tb_ach = YES.                                                */
/*      ELSE                                                                */
/*          ASSIGN                                                          */
/*             start_check-no = giCheckNoSTD                                */
/*             tb_ach = NO.                                                 */
/*   END.                                                                   */
  ASSIGN
     check-date = TODAY
     vcDefaultForm = sys-ctrl.char-fld.

  RUN SetChkForm(INPUT sys-ctrl.char-fld,NO).
  RUN GetRemittanceForm(INPUT cocode,
                        INPUT '',
                        OUTPUT gcRemittanceDefault,
                        OUTPUT glRemittance).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i "AND lv-field-hdl:NAME EQ 'rd-dest'"}
    APPLY "entry" TO check-date.
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
  DISPLAY check-date bank-code bank-name start_check-no tb_ach begin_vend-no 
          end_vend-no rd-dest td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-9 check-date bank-code start_check-no tb_ach 
         begin_vend-no end_vend-no rd-dest td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then run output-to-port.
       WHEN 5 THEN RUN output-to-email.
  end case. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRemittanceForm C-Win 
PROCEDURE GetRemittanceForm :
/*------------------------------------------------------------------------------
  Purpose:    Gets the Remittance Form for ACH Runs  
  Parameters:  Input Vendor, output format, output logical
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcVend AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFormat AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplRemittanceActive AS LOGICAL NO-UNDO.

DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lVend AS LOGICAL     NO-UNDO.


RUN sys/ref/nk1look.p (INPUT ipcCompany, 
                       INPUT "ChkFmtACH", 
                       INPUT 'L', 
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "", 
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).
IF lFound THEN 
    oplRemittanceActive = cReturn EQ 'YES'.

IF oplRemittanceActive THEN DO:
    IF ipcVend NE '' THEN
        lVend = YES.
    ELSE
        lVend = NO.
    RUN sys/ref/nk1look.p (INPUT ipcCompany, 
                           INPUT "ChkFmtACH", 
                           INPUT 'C', 
                           INPUT lVend, 
                           INPUT NO, 
                           INPUT ipcVend, 
                           INPUT "", 
                           OUTPUT cReturn, 
                           OUTPUT lFound).
    IF lFound THEN
        opcFormat = cReturn.

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-email C-Win 
PROCEDURE output-to-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE vcSubject AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcMailBody AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vcErrorMsg AS CHARACTER   NO-UNDO.

IF ll-is-xprint-form THEN DO:

    RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
/*        ASSIGN                               */
/*           lv-pdf-file = list-name + '.pdf'. */
/*           list-name = lv-pdf-file.          */
/*     END.                                    */
/*     ELSE                                    */
/*        list-name = list-name + ".pdf".      */

    ASSIGN
      vcSubject = "ACH Remittance for " + STRING (TODAY, '99/99/9999') + ' ' + STRING (TIME, 'HH:MM:SS AM')
      vcMailBody  = "Please review attached ACH Remittance Notification".

    RUN custom/xpmail2.p(input 'Vendor' ,
                         INPUT 'R-prnchk.' ,
                         input gcPDFFile,
                         input gcDefVend,
                         input vcSubject,
                         input vcMailBody,
                         OUTPUT vcErrorMsg).
END.
 /*  ELSE DO:                                            */
/*      {custom/asimailR.i &TYPE="CUSTOMER"             */
/*                       &begin_cust=ip-begin-cust      */
/*                       &END_cust=ip-end-cust          */
/*                       &mail-subject="ACH Remittance" */
/*                       &mail-body="Invoice"           */
/*                       &mail-file=list-name }         */
/*  END.                                                */
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
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


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

/*  IF sys-ctrl.char-fld eq "Hughes"  THEN  OS-COPY VALUE(list-name)  VALUE("LPT1") /* \\asi2000\epsonfx-")*/ . */
/*  ELSE                                                                                                        */
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
  IF ll-is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
/*   ELSE IF sys-ctrl.char-fld eq "Hughes" THEN  OS-COPY VALUE(list-name)  VALUE("LPT1") /* \\asi2000\epsonfx-")*/ . */
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
  IF ll-is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE
      run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEFINE INPUT PARAM icVendNo AS CHAR NO-UNDO.
DEFINE INPUT PARAM ip-sys-ctrl-mode AS LOG NO-UNDO.

DEF VAR lFlag AS LOGICAL NO-UNDO INIT YES.

/* Finds/creates tmp directory */
{sys/inc/print1.i}

   IF LOOKUP(next-program,"ap/ap-ckfibl.p,ap/ap-ckuni.p,ap/ap-ckprt.p,ap/ap-ckasx.p,ap/ap-ckhug.p") EQ 0 THEN DO:    /* Task 09301303*/
        {sys/inc/outprint.i 0}  /* output to value(list-name) page-size {1} */
   END.
/*    ELSE IF LOOKUP(next-program,"ap/ap-ckhug.p") GT 0 THEN */
/*         OUTPUT TO VALUE(list-name) PAGE-SIZE 59.          */
   ELSE do: /*Fibre and Unipak Laser page statement needs PAGED*/
/*        IF next-program EQ "ap/ap-ckrfc.p" THEN     /* Task 09301303*/ */
/*         output to value(list-name) page-size 80 .  /* Task 09301303*/ */
/*        ELSE */
        OUTPUT TO VALUE(list-name) PAGED.
   END.


if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

outers:
do on error undo outers, leave outers :

  ASSIGN num-of-chks = 0
         sel-per-chk = 0
         x-bank = bank-code
         wdate = check-date.

  IF ip-sys-ctrl-mode THEN
     ASSIGN
        wvend-no = icVendNo
        evend-no = icVendNo.
  ELSE
     ASSIGN
        wvend-no = begin_vend-no
        evend-no = end_vend-no.

  find first bank
      where bank.company   eq cocode
        and bank.bank-code eq bank-code
      NO-LOCK no-error.

  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.vend-no   ge wvend-no
        and ap-chk.vend-no   le evend-no
        and ap-chk.man-check eq no
        and can-find(first ap-sel
                     where ap-sel.company   eq cocode
                       and ap-sel.vend-no   eq ap-chk.vend-no
                       and ap-sel.man-check eq no),

      first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-chk.vend-no

      break by ap-chk.vend-no:
      IF FIRST-OF(ap-chk.vend-no) THEN
          gcDefVend = ap-chk.vend-no.
    assign
     ap-chk.check-no  = ?
     ap-chk.check-act = bank.actnum
     sel-per-chk      = 0
     num-of-chks      = num-of-chks + 1.

    for each ap-sel
        where ap-sel.company   eq cocode
          and ap-sel.vend-no   eq ap-chk.vend-no
          and ap-sel.man-check eq no
        no-lock

        break by ap-sel.inv-no:

      sel-per-chk = sel-per-chk + 1.

      if sel-per-chk eq max-per-chk and not last(ap-sel.inv-no) then
        assign
         num-of-chks = num-of-chks + 1
         sel-per-chk = 0.
    end.
  end.

  /* Code Added for Validating that check numbers have not been posted  */
  /* num-of-chks is actually one less then then number of checks so     */
  /* check range is appropriate number                                  */

  if num-of-chks gt 0 then
  find first ap-pay
      where ap-pay.company   eq cocode      
        AND ap-pay.bank-code EQ bank.bank-code  /* gdm - */    
        and ap-pay.check-act eq bank.actnum
        and ap-pay.check-no  ge stnum
        and ap-pay.check-no  le stnum + num-of-chks - 1
        and ap-pay.posted    eq yes
      no-lock no-error.
  if avail ap-pay then do:
     MESSAGE
            "           At Least One of the Check Numbers Between " skip(1)
             string(stnum) " and " string(stnum + num-of-chks) skip(1)
            "                 Has Already Been Posted!" skip(1)
            " Please Rerun Checks with Different Starting Check Number."
            VIEW-AS ALERT-BOX ERROR .    
    undo outers, leave outers.
  end.

  /* ========= print check =========*/
    IF ll-is-xprint-form THEN DO:
        CASE rd-dest:
            WHEN 1 THEN PUT "<PRINTER?>" /*"</PROGRESS>"*/ .
            WHEN 2 THEN do:
               IF NOT lBussFormModle THEN
                PUT "<PREVIEW><MODAL=NO>". 
               ELSE
                PUT "<PREVIEW>".        
            END.
/*           WHEN 4 THEN do:                                                                                                    */
/*                 ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".                                                          */
/*                 PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".                                                     */
/*           END.                                                                                                               */
          WHEN 5 THEN do:
              gcPDFFile = init-dir + "\RemitRun.pdf".
              PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2.5mm><PDF-OUTPUT=" + gcPdfFile + ">" FORM "x(180)".

          END.
        END CASE.
    END.  
  v-print-mode = "PROD".  /* need it to see for test */

  run value(next-program).

  /*======== end printing =========*/

  find first bank where bank.company   eq cocode
                    and bank.bank-code eq bank-code
                  EXCLUSIVE-LOCK.
  IF AVAIL bank THEN DO:
    IF tb_ach  
          THEN
            bank.spare-int-1 = stnum - 1.
          ELSE
            bank.last-chk = stnum - 1.
      FIND CURRENT bank NO-LOCK NO-ERROR.
  END.

  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.man-check eq no
      no-lock,
      each ap-sel EXCLUSIVE-LOCK
      where ap-sel.company   eq cocode
        and ap-sel.vend-no   eq ap-chk.vend-no
        and ap-chk.man-check eq no:

    assign
     ap-sel.check-no  = ap-chk.check-no
     ap-sel.bank-code = ap-chk.bank-code
     ap-sel.actnum    = ap-chk.check-act.
  end.
END. /* outers */


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetChkForm C-Win 
PROCEDURE SetChkForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcFormName AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER iplRemit AS LOGICAL NO-UNDO.

   ASSIGN
      laser-chk = NO
      lv-prt-bypass = NO.
IF iplRemit THEN DO:

    IF LOOKUP(ipcFormName,laser-list-ach) GT 0 THEN
        ASSIGN
            laser-chk = YES
            v-print-fmt = "s".
    CASE ipcFormName:
        WHEN "ASI" THEN
            ASSIGN
                ll-is-xprint-form = YES
                max-per-chk  = 40
                next-program = "ap/AchASI.p".
        WHEN "Badger" THEN
            ASSIGN 
                ll-is-xprint-form = YES
                max-per-chk  = 40
                next-program = "ap/AchBadger.p".
    END CASE.

END.
ELSE DO:
   if lookup(ipcFormName,laser-list) gt 0 then
      assign
         laser-chk = yes
         v-print-fmt = "s".
   else
      if ipcFormName eq "s" or ipcFormName eq "ASI" then
         v-print-fmt = "s".
   ELSE
      v-print-fmt = "n".

   /*================*/
   CASE ipcFormName:
       WHEN "Brick" THEN
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckbrk.p".
       WHEN "AIHalper" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckaih.p".
       WHEN "P&P" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckpnp.p".
       WHEN "ContSrvc" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckcsc.p".
       WHEN "Pacific" THEN
          assign
             max-per-chk  = 8
             next-program = "ap/ap-ckpqp.p".
       WHEN "Triad" THEN
          assign
             max-per-chk  = 16
             next-program = "ap/ap-cktri.p".
       WHEN "Royal" THEN
          assign
             max-per-chk  = 17
             next-program = "ap/ap-ckroy.p".
       WHEN "Danbury" THEN
          assign
             max-per-chk  = 16
             next-program = "ap/ap-ckdan.p".
       WHEN "Rudd" then
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckrud.p".
       WHEN "Hartford" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckhar.p".
       WHEN "Inland" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckinl.p".
       WHEN "Fibre" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckfib.p".
       WHEN "Herman" THEN
          assign
             max-per-chk  = 11
             next-program = "ap/ap-ckher.p".
       WHEN "Chillic" then
          assign
             max-per-chk  = 7
             next-program = "ap/ap-ckchl.p".
        WHEN "CAPLasAL" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckcap.p".
       WHEN "Midwest" THEN
          assign
             max-per-chk  = 14
             next-program = "ap/ap-ckmwf.p".
       WHEN "Middlesx" THEN
          assign
             max-per-chk  = 7
             next-program = "ap/ap-ckmid.p".
       WHEN "Hughes" THEN
          ASSIGN
             laser-chk = yes
             max-per-chk  = 16
             next-program = "ap/ap-ckhug.p".
       WHEN "Unipak" then
          assign
             laser-chk = yes
             max-per-chk  = 13
             next-program = "ap/ap-ckuni.p".
       WHEN "FibreLsr" THEN
          ASSIGN
             laser-chk = yes
             max-per-chk  = 28
             next-program = "ap/ap-ckfibl.p".
       WHEN "RFC" THEN
          assign
             ll-is-xprint-form = YES   /* Task 09301303*/
             /*laser-chk = yes*/
             max-per-chk  = 12
             next-program = "ap/ap-ckrfc.p".
       WHEN "Prefered" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckprf.p".
       WHEN "ASI2000" then
          assign
             max-per-chk  = 9
             next-program = "ap/ap-ckasi.p".
       WHEN "CustCorr" then
          assign
             max-per-chk  = 9
             next-program = "ap/ap-ckccr.p".
       WHEN "TriadLas" THEN
          assign
             max-per-chk  = 9
             next-program = "ap/chktriad.p".
       WHEN "Action" THEN
          assign
             max-per-chk  = 9
             next-program = "ap/chkaction.p".

       WHEN "IPLaser" THEN /*Interpak Laser*/
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckitp.p".
       WHEN "ASILaser" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckasi.p".
       WHEN "Laser" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-cklaser.p".
       WHEN "Carded" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckcard.p".

       WHEN "Frankstn" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckfnk.p".
       WHEN "Imperial" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckimp.p".
       WHEN "Harwllas" THEN
          assign
             max-per-chk  = 12               
             next-program = "ap/ap-ckhrw.p".
       WHEN "TRILAKES" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cklak.p". /* same as ap-ckasi.p */
       WHEN "COLORLAS" THEN /* color carton laser format very close to TRILAKES*/
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckclr.p". /* same as ap-ckasi.p */
       WHEN "ADVLaser" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckadv.p". /* same as ap-ckasi.p */
       WHEN "ASSILaser" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckassi.p". /* same as ap-ckasi.p */
       WHEN "Soule" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cksoule.p". /* new format */
       WHEN "MidYork" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckmyork.p". /* new format */
       WHEN "Lovepac" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cklovep.p". /* new format */
       WHEN "STCLaser" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckstc.p". /* same as ap-ckadv.p */
       WHEN "Protagon" THEN
          assign
             ll-is-xprint-form = YES
             max-per-chk  = 15
             next-program = "ap/ap-ckprt.p". /* same as ap-ckadv.p */
       WHEN "ASIX" THEN
          assign
             ll-is-xprint-form = YES
             max-per-chk  = 15
             next-program = "ap/ap-ckasx.p". /* Task# 03071305*/
       WHEN "Lakelas" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cklks.p". /* same as ap-ckasi.p */
       WHEN "IndianaL" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckind.p". /* same as ap-ckasi.p */
       WHEN "Adaptls" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckada.p". /* same as ap-ckasi.p */
       WHEN "Vineland" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckvnl.p". /* from as ap-ckadv.p */
       WHEN "Oracle" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckora.p". /* almost same as ap-ckadb.p */
       WHEN "Dayton" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckday.p".
       WHEN "AllLaser" THEN /* all package Laser*/
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckall.p".
       WHEN "AllLaserHP1536DN" THEN /* all package Laser new printer*/
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckalpkhp.p".
       WHEN "Argvlas" THEN
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckarg.p".
       WHEN "Woodland" THEN
          assign
             laser-chk    = yes
             max-per-chk  = 12
             next-program = "ap/ap-ckwdl.p".
       WHEN "Hamilton" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckham.p". /* same as ap-ckasi.p */
       WHEN "PrePkgLS" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckpre.p".
       WHEN "APChkFmt1" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckapfmt.p".
       WHEN "ACPI" then
          assign
             laser-chk = yes
             max-per-chk  = 13
             next-program = "ap/ap-ckacp.p".
       WHEN "APChkFmt2" then
          assign
             ll-is-xprint-form = YES
             max-per-chk  = 10
             next-program = "ap/ap-ckapfmt2.p".
       OTHERWISE DO:
          assign
             max-per-chk  = if v-print-fmt eq "s" then 20 else 12
             next-program = "ap/ap-chk" + v-print-fmt + ".p".

          if search(next-program) eq ? then
             next-program = "ap/ap-chk" + v-print-fmt + ".r".
       END.

   END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetParamDefaults C-Win 
PROCEDURE SetParamDefaults :
/*------------------------------------------------------------------------------
  Purpose:   Fills the starting Bank Code, Check Date, amd check number for the parameter screen  
  Parameters:  iplMain - called from main or not
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplMain AS LOGICAL     NO-UNDO.

FIND FIRST ap-ctrl 
    WHERE ap-ctrl.company = cocode 
    NO-LOCK.
FIND FIRST bank 
    WHERE bank.company = cocode
      AND bank.actnum = ap-ctrl.cash-act 
    NO-LOCK NO-ERROR.
IF AVAIL bank THEN DO:
    ASSIGN
        bank-code      = bank.bank-code
        giCheckNoStd = bank.last-chk + 1
        giCheckNoACH = bank.spare-int-1 + 1
        .
    IF giCheckNoACH EQ 1 THEN  
        giCheckNoACH = 800001.
    IF ACHRunOK() THEN
        ASSIGN 
            start_check-no = giCheckNoACH
            tb_ach = YES.
    ELSE
        ASSIGN 
            start_check-no = giCheckNoSTD
            tb_ach = NO.
END.

IF NOT iplMain THEN
    RUN enable_ui.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST period NO-LOCK
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(check-date:SCREEN-VALUE)
          AND period.pend    GE DATE(check-date:SCREEN-VALUE)
          AND period.pstat
        NO-ERROR.
    IF NOT AVAIL period THEN DO:
      MESSAGE TRIM(check-date:LABEL) + " period closed..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO check-date.
      RETURN ERROR.
    END.

    IF NOT ll-warned THEN DO:
      ll = NO.

      FOR EACH period NO-LOCK
          WHERE period.company EQ cocode
            AND period.pst     LE TODAY
            AND period.pend    GE TODAY
          BY period.pst:

        IF period.pst  GT DATE(check-date:SCREEN-VALUE) OR
           period.pend LT DATE(check-date:SCREEN-VALUE) THEN DO:
          ll = YES.
          MESSAGE TRIM(check-date:LABEL) + " is not in current period, " +
                  "would you like to re-enter..."
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
        END.

        IF ll THEN DO:
          APPLY "entry" TO check-date.
          RETURN ERROR.
        END.

        LEAVE.
      END.

      ll-warned = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ACHEmailOK C-Win 
FUNCTION ACHEmailOK RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Tests check selection for one Vendor
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE lMultVendFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cVendTemp AS CHARACTER   NO-UNDO.

lMultVendFound = YES.
IF begin_vend-no EQ END_vend-no THEN
    lMultVendFound = NO.
ELSE DO:
    FIND FIRST ap-chk
        WHERE ap-chk.company   EQ cocode
          AND ap-chk.vend-no   GE begin_vend-no
          AND ap-chk.vend-no   LE END_vend-no
          AND ap-chk.man-check EQ NO
          AND CAN-FIND(FIRST ap-sel
                       WHERE ap-sel.company   EQ cocode
                         AND ap-sel.vend-no   EQ ap-chk.vend-no
                         AND ap-sel.man-check EQ NO)
        NO-LOCK NO-ERROR.
    IF AVAIL ap-chk THEN cVendTemp = ap-chk.vend-no.
    FOR EACH ap-chk
        WHERE ap-chk.company   EQ cocode
          AND ap-chk.vend-no   GE begin_vend-no
          AND ap-chk.vend-no   LE END_vend-no
          AND ap-chk.man-check EQ NO
          AND CAN-FIND(FIRST ap-sel
                       WHERE ap-sel.company   EQ cocode
                         AND ap-sel.vend-no   EQ ap-chk.vend-no
                         AND ap-sel.man-check EQ NO)
        NO-LOCK
        BREAK BY ap-chk.vend-no:
        IF cVendTemp NE ap-chk.vend-no THEN DO:
            lMultVendFound = YES.
            LEAVE.
        END.
    END.
END.

RETURN NOT lMultVendFound.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ACHRunOK C-Win 
FUNCTION ACHRunOK RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Tests check selection for ACH-Vendors only
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE lNonACHVendorFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCheckSelected AS LOGICAL     NO-UNDO.

FOR EACH ap-chk
    WHERE ap-chk.company   EQ cocode
      AND ap-chk.man-check EQ NO
      AND CAN-FIND(FIRST ap-sel
                   WHERE ap-sel.company   EQ cocode
                     AND ap-sel.vend-no   EQ ap-chk.vend-no
                     AND ap-sel.man-check EQ NO)
    NO-LOCK,
    FIRST vend WHERE vend.company eq cocode AND 
                   vend.vend-no eq ap-chk.vend-no
    NO-LOCK,
    FIRST payment-type no-lock
        WHERE payment-type.company EQ vend.company
        AND payment-type.type EQ vend.payment-type:
    lCheckSelected = YES.
    IF payment-type.paperCheck THEN DO: 
        lNonACHVendorFound = YES.
        LEAVE.
    END.
END.
    RETURN NOT lNonACHVendorFound AND lCheckSelected.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

