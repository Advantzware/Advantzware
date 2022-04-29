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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir   AS CHARACTER NO-UNDO.

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

{ap/ap-chk.i NEW }
FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
DEFINE VARIABLE laser-list     AS CHARACTER NO-UNDO
    INIT "l,Laser,Raritan,Hartford,ASILaser,Delta,Laser,TriadLas,ASI2000,AllLaser,Argvlas,Action,AllLaserHP1536DN".    /* Task 09301303*/
DEFINE VARIABLE laser-list-ach AS CHARACTER NO-UNDO 
    INIT "ASI,Badger".

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.    

DEFINE VARIABLE next-program  AS CHARACTER NO-UNDO INIT "ap/ap-chks.p".    /* std */
DEFINE VARIABLE lv-prt-bypass AS LOG       NO-UNDO.
DEFINE VARIABLE ll-warned     AS LOG       NO-UNDO.
DEFINE VARIABLE num-of-chks   AS INTEGER   NO-UNDO.
DEFINE VARIABLE sel-per-chk   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vcDefaultForm AS CHARACTER NO-UNDO.
DEFINE BUFFER b-ap-chk FOR ap-chk.
DEFINE BUFFER b-vend   FOR vend.
DEFINE VARIABLE ll-is-xprint-form   AS LOG       NO-UNDO INIT FALSE.
DEFINE VARIABLE giCheckNoStd        AS INTEGER   NO-UNDO.
DEFINE VARIABLE giCheckNoACH        AS INTEGER   NO-UNDO.

DEFINE VARIABLE glRemittance        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcRemittanceDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcPDFFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDefVend           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAsiUser            AS LOGICAL   NO-UNDO .

DEFINE VARIABLE hPgmSecurity        AS HANDLE    NO-UNDO.
DEFINE VARIABLE lResult             AS LOG       NO-UNDO.
RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("ap/r-prnchk.w","", OUTPUT lResult).
DELETE OBJECT hPgmSecurity.

IF lResult THEN ASSIGN lAsiUser = YES .


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
start_check-no tb_ach td-reprint-posted begin_vend-no end_vend-no ~
begin_check end_check rd-dest run_format td-show-parm tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS check-date bank-code bank-name ~
start_check-no tb_ach td-reprint-posted begin_vend-no end_vend-no ~
begin_check end_check rd-dest run_format td-show-parm tbAutoClose 

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
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE bank-code AS CHARACTER FORMAT "X(8)" 
     LABEL "Bank Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE bank-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE begin_check AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Beginning Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE check-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Check Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_check AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

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

DEFINE VARIABLE run_format AS CHARACTER FORMAT "X(30)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

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
"To Email", 5
     SIZE 18.2 BY 4.95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 5.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 9.71.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 4.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ach AS LOGICAL INITIAL no 
     LABEL "Electronic-only Check Run" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE td-reprint-posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     check-date AT ROW 2.67 COL 30.6 COLON-ALIGNED
     bank-code AT ROW 3.62 COL 30.6 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     bank-name AT ROW 3.62 COL 47.6 COLON-ALIGNED NO-LABEL
     start_check-no AT ROW 4.57 COL 30.6 COLON-ALIGNED HELP
          "Enter Check Number"
     tb_ach AT ROW 4.67 COL 50.6 WIDGET-ID 2
     td-reprint-posted AT ROW 6.81 COL 60 WIDGET-ID 70
     begin_vend-no AT ROW 7.86 COL 29.6 COLON-ALIGNED HELP
          "Enter Beginning Vendor  Number"
     end_vend-no AT ROW 7.86 COL 67.4 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_check AT ROW 9.14 COL 29.6 COLON-ALIGNED HELP
          "Enter Beginning Vendor  Number" WIDGET-ID 66
     end_check AT ROW 9.14 COL 67.4 COLON-ALIGNED HELP
          "Enter Ending Check Number" WIDGET-ID 68
     lines-per-page AT ROW 11.86 COL 86.8 COLON-ALIGNED
     rd-dest AT ROW 11.91 COL 4.8 NO-LABEL
     lv-font-no AT ROW 11.95 COL 32.6 COLON-ALIGNED
     lv-ornt AT ROW 11.95 COL 43.2 NO-LABEL
     lv-font-name AT ROW 13 COL 33.4 COLON-ALIGNED NO-LABEL
     run_format AT ROW 15.86 COL 64.2 COLON-ALIGNED WIDGET-ID 12
     td-show-parm AT ROW 15.91 COL 31
     tbAutoClose AT ROW 17.33 COL 31 WIDGET-ID 64
     btn-ok AT ROW 18.29 COL 30.8
     btn-cancel AT ROW 18.29 COL 52.2
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 4
     " Output Destination" VIEW-AS TEXT
          SIZE 19.2 BY .62 AT ROW 11.33 COL 4
     " REPRINT OPTIONS" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 6.14 COL 33.4
          FONT 6
     RECT-6 AT ROW 11.67 COL 3
     RECT-7 AT ROW 1.52 COL 3
     RECT-9 AT ROW 6.48 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.2 BY 19.43
         BGCOLOR 15 .


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
         HEIGHT             = 18.71
         WIDTH              = 95.2
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
       bank-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN bank-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_check:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       check-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_check:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       run_format:HIDDEN IN FRAME FRAME-A           = TRUE.

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
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
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
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.


        RUN windows/l-bank.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN
            ASSIGN
                FOCUS:SCREEN-VALUE          = ENTRY(1,char-val)
                bank-name:SCREEN-VALUE      = ENTRY(2,char-val)
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
                START_check-no              = giCheckNoACH
                START_check-no:SCREEN-VALUE = STRING(giCheckNoACH).

        ELSE
            ASSIGN 
                START_check-no              = giCheckNoStd
                START_check-no:SCREEN-VALUE = STRING(giCheckNoStd).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_check
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_check C-Win
ON LEAVE OF begin_check IN FRAME FRAME-A /* Beginning Check# */
DO:
        ASSIGN {&self-name}.          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
DO:
        ASSIGN {&self-name}.
        ASSIGN 
            end_vend-no:SCREEN-VALUE = begin_vend-no:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON VALUE-CHANGED OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
DO:
        ASSIGN 
            end_vend-no:SCREEN-VALUE = begin_vend-no:SCREEN-VALUE.
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
        DEFINE VARIABLE cRemitFormat AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lRemit       AS LOGICAL   NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            FIND FIRST bank NO-LOCK
                WHERE bank.company   EQ cocode
                AND bank.bank-code EQ bank-code:SCREEN-VALUE
                NO-ERROR.
            IF NOT AVAILABLE bank THEN 
            DO:
                MESSAGE "Invalid bank, try help..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO bank-code.
                RETURN NO-APPLY.
            END.
            
            IF vcDefaultForm EQ "Configurable" AND td-reprint-posted THEN
            DO:      
                RUN ap\CheckPrint.p (cocode, /*Company*/ 
                        date(check-date:SCREEN-VALUE), /*Check Date*/ 
                        bank-code:SCREEN-VALUE, /*Bank Code*/ 
                        begin_vend-no:SCREEN-VALUE, end_vend-no:SCREEN-VALUE,  /*vendor range*/
                        integer(begin_check:SCREEN-VALUE), INTEGER(end_check:SCREEN-VALUE), 
                        YES, /* Print posted check*/
                        stnum, /*starting check number*/ 
                        NO, /*Run a sample set of data*/ 
                        rd-dest NE 1,  /*Preview and don't process*/ 
                        list-name + "cfg" /*output file*/).
                
                IF tbAutoClose:CHECKED THEN 
                APPLY 'CLOSE' TO THIS-PROCEDURE.
                RETURN NO-APPLY.
            END.

            IF INT(start_check-no:SCREEN-VALUE) EQ 0 THEN 
                IF tb_ach 
                    THEN
                    start_check-no:SCREEN-VALUE = STRING(bank.spare-int-1 + 1).
                ELSE
                    start_check-no:SCREEN-VALUE = STRING(bank.last-chk + 1).

            ASSIGN {&displayed-objects}.

            IF NOT tb_ach AND rd-dest EQ 5 THEN 
            DO:
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
                WHERE ap-chk.company   EQ cocode
                AND ap-chk.vend-no   GE begin_vend-no
                AND ap-chk.vend-no   LE end_vend-no
                AND ap-chk.man-check EQ NO
                AND CAN-FIND(FIRST ap-sel
                WHERE ap-sel.company   EQ cocode
                AND ap-sel.vend-no   EQ ap-chk.vend-no
                AND ap-sel.man-check EQ NO)) THEN
                FOR EACH b-ap-chk
                    WHERE b-ap-chk.company   EQ cocode
                    AND b-ap-chk.vend-no   GE begin_vend-no
                    AND b-ap-chk.vend-no   LE end_vend-no
                    AND b-ap-chk.man-check EQ NO
                    AND CAN-FIND(FIRST ap-sel
                    WHERE ap-sel.company   EQ cocode
                    AND ap-sel.vend-no   EQ b-ap-chk.vend-no
                    AND ap-sel.man-check EQ NO),
                    FIRST b-vend
                    WHERE b-vend.company EQ cocode
                    AND b-vend.vend-no EQ b-ap-chk.vend-no
                    BREAK BY b-ap-chk.company
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
                        IF NOT lAsiUser AND AVAILABLE sys-ctrl-shipto THEN 
                        DO:
                            RUN SetChkForm (sys-ctrl-shipto.char-fld,NO).
                        END.
                        ELSE 
                        DO:
                            RUN SetChkForm (vcDefaultForm,NO). 
                        END.

                        RUN run-report(b-ap-chk.vend-no, TRUE).
                        RUN GenerateReport.
                    END.
                END. /* FOR EACH */
            ELSE
                MESSAGE "No Checks Were Printed."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END. /* if can-find sys-ctrl-shipto*/
        ELSE /*not can-find sys-ctrl-shipto*/
        DO:

            IF glRemittance AND tb_ach THEN 
            DO:

                RUN SetChkForm(INPUT gcRemittanceDefault,
                    INPUT YES).
            END.

            RUN run-report("", FALSE).
            RUN GenerateReport.
        END.
        RUN SetParamDefaults(INPUT NO).
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME check-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL check-date C-Win
ON LEAVE OF check-date IN FRAME FRAME-A /* Check Date */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
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


&Scoped-define SELF-NAME end_check
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_check C-Win
ON LEAVE OF end_check IN FRAME FRAME-A /* Ending Check# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON HELP OF run_format IN FRAME FRAME-A /* Format */
DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO .
    
        RUN windows/l-syschrL.w (gcompany,"CHKFMT",run_format:SCREEN-VALUE,OUTPUT char-val).
        IF char-val NE '' THEN
            run_format:SCREEN-VALUE = ENTRY(1,char-val).
        IF vcDefaultForm NE run_format:SCREEN-VALUE THEN 
        DO:
            ASSIGN 
                vcDefaultForm = ENTRY(1,char-val) .
            RUN SetChkForm (vcDefaultForm,NO).
            RUN SetRePostOptions(INPUT vcDefaultForm).
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON LEAVE OF run_format IN FRAME FRAME-A /* Format */
DO:
        ASSIGN run_format.

        IF vcDefaultForm NE run_format THEN 
        DO:
            ASSIGN 
                vcDefaultForm = run_format .
            RUN SetChkForm (vcDefaultForm,NO).
            RUN SetRePostOptions(vcDefaultForm).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_check-no C-Win
ON LEAVE OF start_check-no IN FRAME FRAME-A /* Starting Check# */
DO:
        IF tb_ach AND INT(START_check-no:SCREEN-VALUE) LT 800000  THEN 
        DO:
            MESSAGE "Electronic Check number must not be less than 800000"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ELSE 
            ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ach C-Win
ON VALUE-CHANGED OF tb_ach IN FRAME FRAME-A /* Electronic-only Check Run */
DO:
        ASSIGN {&SELF-NAME}.
        IF tb_ach AND NOT ACHRunOK() THEN 
        DO:
            MESSAGE "Check selection must include only Vendors with Electronic Payment Type."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            tb_ach:SCREEN-VALUE = "NO".
            tb_ach = NO.
        END.
        ELSE IF tb_ach THEN
                ASSIGN 
                    START_check-no:SCREEN-VALUE = STRING(giCheckNoACH)
                    START_check-no              = giCheckNoACH.
            ELSE 
                ASSIGN 
                    START_check-no:SCREEN-VALUE = STRING(giCheckNoStd)
                    START_check-no              = giCheckNoStd.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-reprint-posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-reprint-posted C-Win
ON VALUE-CHANGED OF td-reprint-posted IN FRAME FRAME-A /* Reprint Posted */
DO:
        ASSIGN {&self-name}.
        IF td-reprint-posted THEN
        DO:
           ASSIGN
            START_check-no:HIDDEN = YES
            tb_ach:HIDDEN = YES
            begin_check:HIDDEN = NO              
            end_check:HIDDEN = NO.            
        END.
        ELSE 
        ASSIGN
            START_check-no:HIDDEN = NO
            tb_ach:HIDDEN = NO
            begin_check:HIDDEN = YES              
            end_check:HIDDEN = YES.   
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
    IF access-close THEN 
    DO:
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
        check-date    = TODAY
        vcDefaultForm = sys-ctrl.char-fld .

    RUN SetChkForm(INPUT sys-ctrl.char-fld,NO).    
    RUN GetRemittanceForm(INPUT cocode,
        INPUT '',
        OUTPUT gcRemittanceDefault,
        OUTPUT glRemittance).

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").                      
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VC1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i "AND lv-field-hdl:NAME EQ 'rd-dest'"}         
        RUN SetRePostOptions(INPUT vcDefaultForm).
        APPLY "entry" TO check-date.

    END.

    IF NOT lAsiUser THEN
        RUN_format:HIDDEN IN FRAME FRAME-A = YES .
    ELSE 
    DO: 
        ASSIGN
            RUN_format:HIDDEN IN FRAME FRAME-A           = NO 
            RUN_format:SCREEN-VALUE IN FRAME FRAME-A     = vcDefaultForm 
            RUN_format:SENSITIVE  IN FRAME {&FRAME-NAME} = YES .     
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
  DISPLAY check-date bank-code bank-name start_check-no tb_ach td-reprint-posted 
          begin_vend-no end_vend-no begin_check end_check rd-dest run_format 
          td-show-parm tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-9 check-date bank-code start_check-no tb_ach 
         td-reprint-posted begin_vend-no end_vend-no begin_check end_check 
         rd-dest run_format td-show-parm tbAutoClose btn-ok btn-cancel 
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
    IF next-program NE "Configurable" THEN 
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN RUN output-to-port.
            WHEN 5 THEN RUN output-to-email.
        END CASE. 
        
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

    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lVend   AS LOGICAL   NO-UNDO.


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

    IF oplRemittanceActive THEN 
    DO:
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

    DEFINE VARIABLE vcSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcMailBody AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcErrorMsg AS CHARACTER NO-UNDO.

    IF ll-is-xprint-form THEN 
    DO:

        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
        /*        ASSIGN                               */
        /*           lv-pdf-file = list-name + '.pdf'. */
        /*           list-name = lv-pdf-file.          */
        /*     END.                                    */
        /*     ELSE                                    */
        /*        list-name = list-name + ".pdf".      */

        ASSIGN
            vcSubject  = "ACH Remittance for " + STRING (TODAY, '99/99/9999') + ' ' + STRING (TIME, 'HH:MM:SS AM')
            vcMailBody = "Please review attached ACH Remittance Notification".

        RUN custom/xpmail2.p(INPUT 'Vendor' ,
            INPUT 'R-prnchk.' ,
            INPUT gcPDFFile,
            INPUT gcDefVend,
            INPUT vcSubject,
            INPUT vcMailBody,
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
    IF ll-is-xprint-form THEN 
    DO:
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
    IF ll-is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE
        RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEFINE INPUT PARAMETER icVendNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-mode AS LOG NO-UNDO.

    DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO INIT YES.

    /* Finds/creates tmp directory */
    {sys/inc/print1.i}

    IF LOOKUP(next-program,"ap/ap-ckfibl.p,ap/ap-ckuni.p,ap/ap-ckprt.p,ap/ap-ckasx.p,ap/ap-ckhug.p,ap/ap-chkondu.p") EQ 0 THEN 
    DO:    /* Task 09301303*/
        {sys/inc/outprint.i 0}  /* output to value(list-name) page-size {1} */
    END.
    /*    ELSE IF LOOKUP(next-program,"ap/ap-ckhug.p") GT 0 THEN */
    /*         OUTPUT TO VALUE(list-name) PAGE-SIZE 59.          */
   
    ELSE 
    DO: /*Fibre and Unipak Laser page statement needs PAGED*/
        /*        IF next-program EQ "ap/ap-ckrfc.p" THEN     /* Task 09301303*/ */
        /*         output to value(list-name) page-size 80 .  /* Task 09301303*/ */
        /*        ELSE */ 
        IF next-program NE "Configurable" THEN OUTPUT TO VALUE(list-name) PAGED.
    END.


    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    outers:
    DO ON ERROR UNDO outers, LEAVE outers :

        ASSIGN 
            num-of-chks = 0
            sel-per-chk = 0
            x-bank      = bank-code
            wdate       = check-date.

        IF ip-sys-ctrl-mode THEN
            ASSIGN
                wvend-no = icVendNo
                evend-no = icVendNo.
        ELSE
            ASSIGN
                wvend-no = begin_vend-no
                evend-no = end_vend-no.

        FIND FIRST bank
            WHERE bank.company   EQ cocode
            AND bank.bank-code EQ bank-code
            NO-LOCK NO-ERROR.

        FOR EACH ap-chk
            WHERE ap-chk.company   EQ cocode
            AND ap-chk.vend-no   GE wvend-no
            AND ap-chk.vend-no   LE evend-no
            AND ap-chk.man-check EQ NO
            AND CAN-FIND(FIRST ap-sel
            WHERE ap-sel.company   EQ cocode
            AND ap-sel.vend-no   EQ ap-chk.vend-no
            AND ap-sel.man-check EQ NO),

            FIRST vend
            WHERE vend.company EQ cocode
            AND vend.vend-no EQ ap-chk.vend-no

            BREAK BY ap-chk.vend-no:
            IF FIRST-OF(ap-chk.vend-no) THEN
                gcDefVend = ap-chk.vend-no.
            ASSIGN
                ap-chk.check-no  = ?
                ap-chk.check-act = bank.actnum
                sel-per-chk      = 0
                num-of-chks      = num-of-chks + 1.

            FOR EACH ap-sel
                WHERE ap-sel.company   EQ cocode
                AND ap-sel.vend-no   EQ ap-chk.vend-no
                AND ap-sel.man-check EQ NO
                NO-LOCK

                BREAK BY ap-sel.inv-no:

                sel-per-chk = sel-per-chk + 1.

                IF sel-per-chk EQ max-per-chk AND NOT last(ap-sel.inv-no) THEN
                    ASSIGN
                        num-of-chks = num-of-chks + 1
                        sel-per-chk = 0.
            END.
        END.

        /* Code Added for Validating that check numbers have not been posted  */
        /* num-of-chks is actually one less then then number of checks so     */
        /* check range is appropriate number                                  */

        IF num-of-chks GT 0 THEN
            FIND FIRST ap-pay
                WHERE ap-pay.company   EQ cocode      
                AND ap-pay.bank-code EQ bank.bank-code  /* gdm - */    
                AND ap-pay.check-act EQ bank.actnum
                AND ap-pay.check-no  GE stnum
                AND ap-pay.check-no  LE stnum + num-of-chks - 1
                AND ap-pay.posted    EQ YES
                NO-LOCK NO-ERROR.
        IF AVAILABLE ap-pay THEN 
        DO:
            MESSAGE
                "           At Least One of the Check Numbers Between " SKIP(1)
                STRING(stnum) " and " STRING(stnum + num-of-chks) SKIP(1)
                "                 Has Already Been Posted!" SKIP(1)
                " Please Rerun Checks with Different Starting Check Number."
                VIEW-AS ALERT-BOX ERROR .    
            UNDO outers, LEAVE outers.
        END.

        /* ========= print check =========*/
        IF next-program EQ "Configurable" THEN
            RUN ap\CheckPrint.p (cocode, /*Company*/ 
                wdate, /*Check Date*/ 
                bank-code, /*Bank Code*/ 
                wvend-no, evend-no,  /*vendor range*/ 
                0, 0, /* check range*/ 
                NO, /* Print posted check*/
                stnum, /*starting check number*/ 
                NO, /*Run a sample set of data*/ 
                rd-dest NE 1,  /*Preview and don't process*/ 
                list-name + "cfg" /*output file*/).
        ELSE 
        DO:
            IF ll-is-xprint-form THEN 
            DO:
                CASE rd-dest:
                    WHEN 1 THEN 
                        PUT "<PRINTER?>" /*"</PROGRESS>"*/ .
                    WHEN 2 THEN 
                        DO:
                            IF NOT lBussFormModle THEN
                                PUT "<PREVIEW><MODAL=NO>". 
                            ELSE
                                PUT "<PREVIEW>".        
                        END.
                    /*           WHEN 4 THEN do:                                                                                                    */
                    /*                 ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".                                                          */
                    /*                 PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".                                                     */
                    /*           END.                                                                                                               */
                    WHEN 5 THEN 
                        DO:
                            gcPDFFile = init-dir + "\RemitRun.pdf".
                            PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2.5mm><PDF-OUTPUT=" + gcPdfFile + ">" FORM "x(180)".

                        END.
                END CASE.
            END.  
            v-print-mode = "PROD".  /* need it to see for test */

            RUN value(next-program).

            /*======== end printing =========*/

            FIND FIRST bank WHERE bank.company   EQ cocode
                AND bank.bank-code EQ bank-code
                EXCLUSIVE-LOCK.
            IF AVAILABLE bank THEN 
            DO:
                IF tb_ach  
                    THEN
                    bank.spare-int-1 = stnum - 1.
                ELSE
                    bank.last-chk = stnum - 1.
                FIND CURRENT bank NO-LOCK NO-ERROR.
            END.
            FOR EACH ap-chk
                WHERE ap-chk.company   EQ cocode
                AND ap-chk.man-check EQ NO
                NO-LOCK,
                EACH ap-sel EXCLUSIVE-LOCK
                WHERE ap-sel.company   EQ cocode
                AND ap-sel.vend-no   EQ ap-chk.vend-no
                AND ap-chk.man-check EQ NO:

                ASSIGN
                    ap-sel.check-no  = ap-chk.check-no
                    ap-sel.bank-code = ap-chk.bank-code
                    ap-sel.actnum    = ap-chk.check-act
                    .
            END.
        END. /* Not configurable*/
    
  
    END. /* outers */


    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetChkForm C-Win 
PROCEDURE SetChkForm :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplRemit AS LOGICAL NO-UNDO.

    ASSIGN
        laser-chk     = NO
        lv-prt-bypass = NO.
    IF iplRemit THEN 
    DO:

        IF LOOKUP(ipcFormName,laser-list-ach) GT 0 THEN
            ASSIGN
                laser-chk   = YES
                v-print-fmt = "s".
        CASE ipcFormName:
            WHEN "ASI" THEN
                ASSIGN
                    ll-is-xprint-form = YES
                    max-per-chk       = 40
                    next-program      = "ap/AchASI.p".
            WHEN "Badger" THEN
                ASSIGN 
                    ll-is-xprint-form = YES
                    max-per-chk       = 40
                    next-program      = "ap/AchBadger.p".
        END CASE.

    END.
    ELSE 
    DO:
        IF LOOKUP(ipcFormName,laser-list) GT 0 THEN
            ASSIGN
                laser-chk   = YES
                v-print-fmt = "s".
        ELSE
            IF ipcFormName EQ "s" OR ipcFormName EQ "ASI" THEN
                v-print-fmt = "s".
            ELSE
                v-print-fmt = "n".

        /*================*/
        CASE ipcFormName:
            WHEN "Brick" THEN
                ASSIGN
                    max-per-chk  = 20
                    next-program = "ap/ap-ckbrk.p".
            WHEN "AIHalper" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckaih.p".
            WHEN "P&P" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckpnp.p".
            WHEN "ContSrvc" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckcsc.p".
            WHEN "Pacific" THEN
                ASSIGN
                    max-per-chk  = 8
                    next-program = "ap/ap-ckpqp.p".
            WHEN "Triad" THEN
                ASSIGN
                    max-per-chk  = 16
                    next-program = "ap/ap-cktri.p".
            WHEN "Royal" THEN
                ASSIGN
                    max-per-chk  = 17
                    next-program = "ap/ap-ckroy.p".
            WHEN "Danbury" THEN
                ASSIGN
                    max-per-chk  = 16
                    next-program = "ap/ap-ckdan.p".
            WHEN "Rudd" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckrud.p".
            WHEN "Hartford" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckhar.p".
            WHEN "Inland" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckinl.p".
            WHEN "Fibre" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckfib.p".
            WHEN "Herman" THEN
                ASSIGN
                    max-per-chk  = 11
                    next-program = "ap/ap-ckher.p".
            WHEN "Chillic" THEN
                ASSIGN
                    max-per-chk  = 7
                    next-program = "ap/ap-ckchl.p".
            WHEN "CAPLasAL" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckcap.p".
            WHEN "Midwest" THEN
                ASSIGN
                    max-per-chk  = 14
                    next-program = "ap/ap-ckmwf.p".
            WHEN "Middlesx" THEN
                ASSIGN
                    max-per-chk  = 7
                    next-program = "ap/ap-ckmid.p".
            WHEN "Hughes" THEN
                ASSIGN
                    laser-chk    = YES
                    max-per-chk  = 16
                    next-program = "ap/ap-ckhug.p".
            WHEN "Unipak" THEN
                ASSIGN
                    laser-chk    = YES
                    max-per-chk  = 13
                    next-program = "ap/ap-ckuni.p".
            WHEN "FibreLsr" THEN
                ASSIGN
                    laser-chk    = YES
                    max-per-chk  = 28
                    next-program = "ap/ap-ckfibl.p".
            WHEN "RFC" THEN
                ASSIGN
                    ll-is-xprint-form = YES   /* Task 09301303*/
                    /*laser-chk = yes*/
                    max-per-chk       = 12
                    next-program      = "ap/ap-ckrfc.p".
            WHEN "Prefered" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckprf.p".
            WHEN "ASI2000" THEN
                ASSIGN
                    max-per-chk  = 9
                    next-program = "ap/ap-ckasi.p".
            WHEN "CustCorr" THEN
                ASSIGN
                    max-per-chk  = 9
                    next-program = "ap/ap-ckccr.p".
            WHEN "TriadLas" THEN
                ASSIGN
                    max-per-chk  = 9
                    next-program = "ap/chktriad.p".
            WHEN "Action" THEN
                ASSIGN
                    max-per-chk  = 9
                    next-program = "ap/chkaction.p".

            WHEN "IPLaser" THEN /*Interpak Laser*/
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckitp.p".
            WHEN "ASILaser" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckasi.p".
            WHEN "Delta" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-delta.p".
            WHEN "Laser" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-cklaser.p".
            WHEN "Carded" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckcard.p".

            WHEN "Frankstn" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckfnk.p".
            WHEN "Imperial" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckimp.p".
            WHEN "Harwllas" THEN
                ASSIGN
                    max-per-chk  = 12               
                    next-program = "ap/ap-ckhrw.p".
            WHEN "TRILAKES" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-cklak.p". /* same as ap-ckasi.p */
            WHEN "COLORLAS" THEN /* color carton laser format very close to TRILAKES*/
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckclr.p". /* same as ap-ckasi.p */
            WHEN "ADVLaser" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckadv.p". /* same as ap-ckasi.p */
            WHEN "ASSILaser" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckassi.p". /* same as ap-ckasi.p */
            WHEN "Soule" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-cksoule.p". /* new format */
            WHEN "MidYork" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckmyork.p". /* new format */
            WHEN "Lovepac" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-cklovep.p". /* new format */
            WHEN "STCLaser" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckstc.p". /* same as ap-ckadv.p */
            WHEN "Protagon" THEN
                ASSIGN
                    ll-is-xprint-form = YES
                    max-per-chk       = 15
                    next-program      = "ap/ap-ckprt.p". /* same as ap-ckadv.p */
            WHEN "ASIX" THEN
                ASSIGN
                    ll-is-xprint-form = YES
                    max-per-chk       = 15
                    next-program      = "ap/ap-ckasx.p". /* Task# 03071305*/
            WHEN "Lakelas" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-cklks.p". /* same as ap-ckasi.p */
            WHEN "IndianaL" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckind.p". /* same as ap-ckasi.p */
            WHEN "Adaptls" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckada.p". /* same as ap-ckasi.p */
            WHEN "Vineland" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckvnl.p". /* from as ap-ckadv.p */
            WHEN "Oracle" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckora.p". /* almost same as ap-ckadb.p */
            WHEN "Dayton" THEN
                ASSIGN
                    max-per-chk  = 12
                    next-program = "ap/ap-ckday.p".
            WHEN "AllLaser" THEN /* all package Laser*/
                ASSIGN
                    max-per-chk  = 20
                    next-program = "ap/ap-ckall.p".
            WHEN "AllLaserHP1536DN" THEN /* all package Laser new printer*/
                ASSIGN
                    max-per-chk  = 20
                    next-program = "ap/ap-ckalpkhp.p".
            WHEN "Argvlas" THEN
                ASSIGN
                    max-per-chk  = 20
                    next-program = "ap/ap-ckarg.p".
            WHEN "Woodland" THEN
                ASSIGN
                    laser-chk    = YES
                    max-per-chk  = 12
                    next-program = "ap/ap-ckwdl.p".
            WHEN "Hamilton" THEN
                ASSIGN
                    max-per-chk  = 13
                    next-program = "ap/ap-ckham.p". /* same as ap-ckasi.p */
            WHEN "PrePkgLS" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckpre.p".
            WHEN "Valley" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-valley.p".
            WHEN "APChkFmt1" THEN
                ASSIGN
                    max-per-chk  = 10
                    next-program = "ap/ap-ckapfmt.p".
            WHEN "ACPI" THEN
                ASSIGN
                    laser-chk    = YES
                    max-per-chk  = 13
                    next-program = "ap/ap-ckacp.p".
            WHEN "APChkFmt2" THEN
                ASSIGN
                    ll-is-xprint-form = YES
                    max-per-chk       = 10
                    next-program      = "ap/ap-ckapfmt2.p".
            WHEN "Onducorr" THEN
                ASSIGN
                    ll-is-xprint-form = YES
                    max-per-chk       = 12
                    next-program      = "ap/ap-chkondu.p".
            WHEN "Configurable" THEN 
                ASSIGN 
                    ll-is-xprint-form = YES
                    next-program      = "Configurable"
                    .
            OTHERWISE 
            DO:
                ASSIGN
                    max-per-chk  = IF v-print-fmt EQ "s" THEN 20 ELSE 12
                    next-program = "ap/ap-chk" + v-print-fmt + ".p".

                IF SEARCH(next-program) EQ ? THEN
                    next-program = "ap/ap-chk" + v-print-fmt + ".r".
          
            END.

        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRePostOptions C-Win 
PROCEDURE SetRePostOptions :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormName AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF ipcFormName EQ "Configurable" THEN
        DO:                            
               ASSIGN
                   begin_check:HIDDEN = YES              
                   end_check:HIDDEN = YES
                   td-reprint-posted:HIDDEN = NO
                   td-reprint-posted:SCREEN-VALUE = "No".                    
        END.
        ELSE
        DO:
            ASSIGN
                   begin_check:HIDDEN = YES              
                   end_check:HIDDEN = YES
                   td-reprint-posted:HIDDEN = YES
                   td-reprint-posted:SCREEN-VALUE = "No"
                   start_check-no:HIDDEN = NO
                   tb_ach:HIDDEN = NO.
        END.
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
    IF AVAILABLE bank THEN 
    DO:
        ASSIGN
            bank-code    = bank.bank-code
            giCheckNoStd = bank.last-chk + 1
            giCheckNoACH = bank.spare-int-1 + 1
            .
        IF giCheckNoACH EQ 1 THEN  
            giCheckNoACH = 800001.
        IF ACHRunOK() THEN
            ASSIGN 
                start_check-no = giCheckNoACH
                tb_ach         = YES.
        ELSE
            ASSIGN 
                start_check-no = giCheckNoSTD
                tb_ach         = NO.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll AS LOG NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST period NO-LOCK
            WHERE period.company EQ cocode
            AND period.pst     LE DATE(check-date:SCREEN-VALUE)
            AND period.pend    GE DATE(check-date:SCREEN-VALUE)
            AND period.pstat
            NO-ERROR.
        IF NOT AVAILABLE period THEN 
        DO:
            MESSAGE TRIM(check-date:LABEL) + " period closed..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO check-date.
            RETURN ERROR.
        END.

        IF NOT ll-warned THEN 
        DO:
            ll = NO.

            FOR EACH period NO-LOCK
                WHERE period.company EQ cocode
                AND period.pst     LE TODAY
                AND period.pend    GE TODAY
                BY period.pst:

                IF period.pst  GT DATE(check-date:SCREEN-VALUE) OR
                    period.pend LT DATE(check-date:SCREEN-VALUE) THEN 
                DO:
                    ll = YES.
                    MESSAGE TRIM(check-date:LABEL) + " is not in current period, " +
                        "would you like to re-enter..."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
                END.

                IF ll THEN 
                DO:
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
    DEFINE VARIABLE lMultVendFound AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cVendTemp      AS CHARACTER NO-UNDO.

    lMultVendFound = YES.
    IF begin_vend-no EQ END_vend-no THEN
        lMultVendFound = NO.
    ELSE 
    DO:
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
        IF AVAILABLE ap-chk THEN cVendTemp = ap-chk.vend-no.
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
            IF cVendTemp NE ap-chk.vend-no THEN 
            DO:
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
    DEFINE VARIABLE lNonACHVendorFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckSelected     AS LOGICAL NO-UNDO.

    FOR EACH ap-chk
        WHERE ap-chk.company   EQ cocode
        AND ap-chk.man-check EQ NO
        AND CAN-FIND(FIRST ap-sel
        WHERE ap-sel.company   EQ cocode
        AND ap-sel.vend-no   EQ ap-chk.vend-no
        AND ap-sel.man-check EQ NO)
        NO-LOCK,
        FIRST vend WHERE vend.company EQ cocode AND 
        vend.vend-no EQ ap-chk.vend-no
        NO-LOCK,
        FIRST payment-type NO-LOCK
        WHERE payment-type.company EQ vend.company
        AND payment-type.type EQ vend.payment-type:
        lCheckSelected = YES.
        IF payment-type.paperCheck THEN 
        DO: 
            lNonACHVendorFound = YES.
            LEAVE.
        END.
    END.
    RETURN NOT lNonACHVendorFound AND lCheckSelected.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

