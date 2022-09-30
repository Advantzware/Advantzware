&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : arrep\r-stmt.w

  Description       : Statements

  Input Parameters  : None

  Output Parameters : None

  Author            : Ron Stark

  Created           : 01/12/2000

  History           : dgd 06/08/2007  - Task# 05300713: Batch E-Mail

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
DEFINE VARIABLE list-name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""AR4"" }*/

{sys/ref/CustList.i NEW}

/* Temp-Tables */
{arrep/ttStmtExl.i NEW} 

DEFINE TEMP-TABLE tt-cust-check NO-UNDO
    FIELD cust-no AS CHARACTER
    FIELD contact AS CHARACTER
    FIELD log-fld AS LOGICAL
    INDEX check1 cust-no ASC.

DEFINE            VARIABLE is-xprint-form    AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-pdf-file       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-fax-image      AS CHARACTER NO-UNDO.  /* fax imge file */
DEFINE            VARIABLE vlSkipRec         AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-stmt-char       AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-print-hdr       LIKE sys-ctrl.log-fld NO-UNDO.
//DEF VAR v-use-cust AS LOG NO-UNDO.
DEFINE            VARIABLE vcDefaultForm     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-dir             AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE            VARIABLE glCustListActive  AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lAsiUser          AS LOGICAL   NO-UNDO .

DEFINE            VARIABLE retcode           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lBussFormModle    AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE d-print-fmt-dec   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lValid            AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE hPgmSecurity      AS HANDLE    NO-UNDO.
DEFINE            VARIABLE lResult           AS LOG       NO-UNDO.
RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("arrep/r-stmt.w","", OUTPUT lResult).
DELETE OBJECT hPgmSecurity.

IF lResult THEN ASSIGN lAsiUser = YES .


RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{custom/xprint.i}

/* Buffers */
DEFINE BUFFER b1-cust FOR cust.
DEFINE BUFFER b-cust  FOR cust.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 stmt-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no stmt-msg fi_contact tb_detailed ~
tb_past-due tb_curr-bal tb_HideDialog rd-dest tb_BatchMail tbAutoClose ~
run_format btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS stmt-date tb_cust-list begin_cust-no ~
end_cust-no stmt-msg fi_contact lbl_detailed tb_detailed lbl_past-due ~
tb_past-due lbl_curr-bal tb_curr-bal tb_HideDialog rd-dest tb_BatchMail ~
tbAutoClose run_format

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD formatDate C-Win 
FUNCTION formatDate RETURNS CHARACTER
    ( INPUT ip-date AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_contact     AS CHARACTER FORMAT "X(40)":U 
    LABEL "Attention" 
    VIEW-AS FILL-IN 
    SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_curr-bal   AS CHARACTER FORMAT "X(256)":U INITIAL "Include customers with 0 current balance?" 
    VIEW-AS FILL-IN 
    SIZE 44 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_detailed   AS CHARACTER FORMAT "X(256)":U INITIAL "Detailed?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_past-due   AS CHARACTER FORMAT "X(256)":U INITIAL "Print Past Due Only?" 
    VIEW-AS FILL-IN 
    SIZE 22 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE stmt-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Statement Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE stmt-msg       AS CHARACTER FORMAT "X(40)":U 
    LABEL "Statement Message" 
    VIEW-AS FILL-IN 
    SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5
    SIZE 15 BY 5.43 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 6.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 9.5.

DEFINE VARIABLE tb_BatchMail  AS LOGICAL   INITIAL NO 
    LABEL "&Batch Mail" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tbAutoClose   AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_curr-bal   AS LOGICAL   INITIAL NO 
    LABEL "Include customers with 0 current balance?" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-list  AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed   AS LOGICAL   INITIAL NO 
    LABEL "Detailed?" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_HideDialog AS LOGICAL   INITIAL NO 
    LABEL "&Hide Dialog-Box" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_past-due   AS LOGICAL   INITIAL NO 
    LABEL "Print Past Due Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE run_format    AS CHARACTER FORMAT "X(30)":U 
    LABEL "Format" 
    VIEW-AS FILL-IN /*COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST*/
    SIZE 25 BY 1 NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    stmt-date AT ROW 1.95 COL 27 COLON-ALIGNED
    tb_cust-list AT ROW 2.91 COL 29.2 WIDGET-ID 6
    btnCustList AT ROW 2.91 COL 61.6 WIDGET-ID 8
    begin_cust-no AT ROW 3.86 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    stmt-msg AT ROW 5.05 COL 27 COLON-ALIGNED
    fi_contact AT ROW 6.29 COL 27 COLON-ALIGNED WIDGET-ID 2
    lbl_detailed AT ROW 7.62 COL 37 COLON-ALIGNED NO-LABELS
    tb_detailed AT ROW 7.62 COL 51
    lbl_past-due AT ROW 8.81 COL 27 COLON-ALIGNED NO-LABELS
    tb_past-due AT ROW 8.81 COL 51
    lbl_curr-bal AT ROW 9.86 COL 7 COLON-ALIGNED NO-LABELS WIDGET-ID 10
    tb_curr-bal AT ROW 9.86 COL 51 WIDGET-ID 12
    tb_HideDialog AT ROW 12.14 COL 31
    rd-dest AT ROW 12.05 COL 6 NO-LABELS
    tb_BatchMail AT ROW 13.19 COL 31
    tbAutoClose AT ROW 17.8 COL 31 WIDGET-ID 78
    lines-per-page AT ROW 14.67 COL 84 COLON-ALIGNED
    lv-ornt AT ROW 14.71 COL 31 NO-LABELS
    lv-font-no AT ROW 16.33 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 17.29 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.05 COL 31
    run_format AT ROW 16 COL 65 COLON-ALIGNED WIDGET-ID 12
    btn-ok AT ROW 18.7 COL 31
    btn-cancel AT ROW 18.7 COL 51
    " Output Destination" VIEW-AS TEXT
    SIZE 18.9 BY .62 AT ROW 11.4 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 
    RECT-6 AT ROW 11.75 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.2 BY 19.57
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
        TITLE              = "Statements"
        HEIGHT             = 19.62
        WIDTH              = 95.3
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".


ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_contact:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_curr-bal IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_detailed IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_past-due IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.
    
ASSIGN 
    stmt-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    stmt-msg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_curr-bal:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_detailed:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_past-due:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Statements */
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
ON WINDOW-CLOSE OF C-Win /* Statements */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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
        DEFINE VARIABLE v-excel         AS LOG       NO-UNDO.
        DEFINE VARIABLE v-cust-mode     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-excel-message AS LOG       NO-UNDO.

        SESSION:SET-WAIT-STATE ("GENERAL").

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        IF (rd-dest = 4 OR rd-dest = 5) AND NOT tb_BatchMail:CHECKED 
            AND begin_cust-no <> end_cust-no THEN 
        DO:

            IF rd-dest = 4 THEN
                MESSAGE "Beginning Customer and Ending Customer must be the same for Fax."
                    VIEW-AS ALERT-BOX ERROR.

            ELSE IF rd-dest = 5 THEN
                    MESSAGE "Beginning Customer and Ending Customer must be the same for E-Mail."
                        VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO end_cust-no .
            RETURN NO-APPLY.
        END.
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT tb_cust-list OR NOT AVAILABLE ttCustList THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive,
                INPUT begin_cust-no,
                INPUT END_cust-no).
        END.
        IF tb_cust-list THEN 
        DO:
            FOR EACH tt-cust-check WHERE tt-cust-check.log-fld  NO-LOCK:
                FIND FIRST ttCustList EXCLUSIVE-LOCK
                    WHERE ttCustList.cust-no EQ tt-cust-check.cust-no NO-ERROR .
                IF AVAILABLE ttCustList THEN
                    ASSIGN
                        ttCustList.log-fld = YES.
            END.
        END.

        IF NOT lAsiUser AND CAN-FIND(FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company = cocode AND
            sys-ctrl-shipto.NAME = "STMTPRINT") THEN
        DO:
            FOR EACH b-cust FIELDS(cust-no) WHERE
                b-cust.company EQ cocode AND
                b-cust.cust-no GE begin_cust-no AND
                b-cust.cust-no LE end_cust-no
                NO-LOCK:

                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "STMTPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = b-cust.cust-no AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                IF AVAILABLE sys-ctrl-shipto THEN
                    ASSIGN
                        v-stmt-char     = sys-ctrl-shipto.char-fld
                        d-print-fmt-dec = sys-ctrl-shipto.dec-fld .
                ELSE 
                    ASSIGN
                        v-stmt-char     = sys-ctrl.char-fld
                        d-print-fmt-dec = sys-ctrl.dec-fld.
                /*ELSE
                   v-stmt-char = vcDefaultForm.*/
                FIND FIRST ttCustList NO-LOCK
                    WHERE ttCustList.cust-no EQ b-cust.cust-no
                    AND ttCustList.log-fld = YES NO-ERROR .
                IF NOT AVAILABLE ttCustList THEN NEXT .

                IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN
                DO:
                    v-excel = YES.

                    CASE rd-dest:
                        WHEN 1 THEN
                            LvOutputSelection = "Printer".
                        WHEN 2 THEN
                            LvOutputSelection = "Screen". 
                        WHEN 3 THEN
                            LvOutputSelection = "File". 
                        WHEN 4 THEN
                            LvOutputSelection = "Fax". 
                        WHEN 5 THEN
                            LvOutputSelection = "Email".
                        WHEN 6 THEN
                            LvOutputSelection = "Port".
                    END CASE.
                END.
                ELSE
                    v-excel = NO.

                IF v-excel AND tb_BatchMail:CHECKED THEN
                DO:
                    IF v-excel-message = NO THEN
                    DO:
                        v-excel-message = YES.
                        MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
                            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    END.
                    NEXT.
                END.
                    
                IF v-excel OR NOT rd-dest = 5 THEN
                    RUN run-report(b-cust.cust-no, TRUE).
                     
                IF NOT CAN-FIND (FIRST tt-inv) THEN NEXT.    

                IF NOT v-excel THEN
                    RUN GenerateReport(b-cust.cust-no,b-cust.cust-no).
                ELSE IF rd-dest EQ 5 THEN /*excel*/
                    DO:   
                        v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                        ELSE "Customer1".
                        RUN SendMail-1 (b-cust.cust-no, v-cust-mode,  v-dir + "\stmt.pdf").
                    END.
                FOR EACH ttCustList NO-LOCK
                    WHERE ttCustList.cust-no EQ b-cust.cust-no :
                    ASSIGN 
                        ttCustList.log-fld = NO  .
                END.
           
            END. /*each b-cust*/

        END. /*if sys-ctrl-shipto found*/
        ELSE IF rd-dest = 5 THEN  /*if no sys-ctrl-shipto found*/
            DO:
                FOR EACH ttCustList NO-LOCK  :
  
                    /* find first ar-inv where ar-inv.company eq cocode    and
                                      ar-inv.cust-no eq ttCustList.cust-no and
                                      ar-inv.posted                and
                                      ar-inv.due ne 0              and
                                      ar-inv.inv-date le stmt-date and
                                      ar-inv.due-date le stmt-date no-lock no-error.
          
                     if not avail ar-inv THEN next.*/

                    v-stmt-char = vcDefaultForm.
                    IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN
                    DO:
                        v-excel = YES.

                        CASE rd-dest:
                            WHEN 1 THEN
                                LvOutputSelection = "Printer".
                            WHEN 2 THEN
                                LvOutputSelection = "Screen". 
                            WHEN 3 THEN
                                LvOutputSelection = "File". 
                            WHEN 4 THEN
                                LvOutputSelection = "Fax". 
                            WHEN 5 THEN
                                LvOutputSelection = "Email".
                            WHEN 6 THEN
                                LvOutputSelection = "Port".
                        END CASE.
                    END.
                    ELSE
                        v-excel = NO.

                    IF v-excel AND tb_BatchMail:CHECKED THEN
                    DO:
                        MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
                            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                        RETURN NO-APPLY.
                    END.

                    IF v-excel OR NOT rd-dest = 5 THEN
                        RUN run-report("", FALSE).

                    IF NOT v-excel THEN
                        RUN GenerateReport( ttCustList.cust-no, ttCustList.cust-no).

                    ELSE IF rd-dest EQ 5 THEN /*excel*/
                        DO:
                            v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                            ELSE "Customer1".
                            RUN SendMail-1 (ttCustList.cust-no, v-cust-mode, v-dir + "\stmt.pdf").
                        END.
                END. /* cust */
            END. /*end sys-ctrl-shipto not found*/


        IF rd-dest NE 5 AND CAN-FIND(FIRST ttCustList WHERE
            ttCustList.cust-no NE "" AND ttCustList.log-fld = YES ) THEN
        DO: 

            IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN
            DO:
                v-excel = YES.

                CASE rd-dest:
                    WHEN 1 THEN
                        LvOutputSelection = "Printer".
                    WHEN 2 THEN
                        LvOutputSelection = "Screen". 
                    WHEN 3 THEN
                        LvOutputSelection = "File". 
                    WHEN 4 THEN
                        LvOutputSelection = "Fax". 
                    WHEN 5 THEN
                        LvOutputSelection = "Email".
                    WHEN 6 THEN
                        LvOutputSelection = "Port".
                END CASE.
            END.
            ELSE
                v-excel = NO.

            IF v-excel AND tb_BatchMail:CHECKED THEN
            DO:
                MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN NO-APPLY.
            END.
            v-stmt-char = vcDefaultForm.
            IF v-excel OR NOT rd-dest = 5 THEN
                RUN run-report("", FALSE).

            IF NOT v-excel THEN
                RUN GenerateReport(begin_cust-no,end_cust-no).

            ELSE IF rd-dest EQ 5 THEN /*excel*/
                DO:
                    v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                    ELSE "Customer1".
                    RUN SendMail-1 (begin_cust-no, v-cust-mode, v-dir + "\stmt.pdf").
                END.

        END. /*end sys-ctrl-shipto not found*/

        /*current-window:WINDOW-STATE  = WINDOW-MINIMIZE. */   
        /*current-window:WINDOW-STATE  = WINDOW-Normal.*/
        STATUS DEFAULT "Processing Complete".
        RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
            
        SESSION:SET-WAIT-STATE ("").
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
        RUN setAttentionDefault.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_contact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_contact C-Win
ON ENTRY OF fi_contact IN FRAME FRAME-A /* Attention */
    DO:
        IF begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE END_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN 
        DO:
            MESSAGE "Attention line can only be changed when statement is run for a single customer."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            fi_contact:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            fi_contact = "".
            APPLY 'entry' TO begin_cust-no.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_contact C-Win
ON LEAVE OF fi_contact IN FRAME FRAME-A /* Attention */
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

        RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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
        RUN SetEmailBoxes.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stmt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stmt-date C-Win
ON LEAVE OF stmt-date IN FRAME FRAME-A /* Statement Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stmt-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stmt-msg C-Win
ON LEAVE OF stmt-msg IN FRAME FRAME-A /* Statement Message */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_BatchMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_BatchMail C-Win
ON VALUE-CHANGED OF tb_BatchMail IN FRAME FRAME-A /* Batch Mail */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_curr-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_curr-bal C-Win
ON VALUE-CHANGED OF tb_curr-bal IN FRAME FRAME-A /* Include customers with 0 current balance? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
    DO:
        ASSIGN {&self-name}.
        EMPTY TEMP-TABLE ttCustList.
        RUN SetCustRange(INPUT tb_cust-list).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_HideDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_HideDialog C-Win
ON VALUE-CHANGED OF tb_HideDialog IN FRAME FRAME-A /* Hide Dialog-Box */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_past-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_past-due C-Win
ON VALUE-CHANGED OF tb_past-due IN FRAME FRAME-A /* Print Past Due Only? */
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

&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON LEAVE OF run_format IN FRAME FRAME-A /* Warehouse Months */
    DO:
        ASSIGN run_format.

        IF v-stmt-char NE run_format THEN 
        DO:
            ASSIGN 
                v-stmt-char   = run_format
                vcDefaultForm = v-stmt-char.
            RUN  pRunFormatValueChanged .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON HELP OF run_format IN FRAME FRAME-A /* Font */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO .
    
        RUN windows/l-syschrL.w (gcompany,"STMTPRINT",run_format:SCREEN-VALUE,OUTPUT char-val).
        IF char-val NE '' THEN
            run_format:SCREEN-VALUE = ENTRY(1,char-val).
        IF v-stmt-char NE run_format:SCREEN-VALUE THEN 
        DO:
            ASSIGN 
                v-stmt-char   = run_format:SCREEN-VALUE
                vcDefaultForm = v-stmt-char.
            RUN  pRunFormatValueChanged .
        END.

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

    stmt-date = TODAY.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AR4" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "AR4",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
ASSIGN 
    stmt-date              = TODAY
    stmt-date:SCREEN-VALUE = STRING(TODAY).

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "STMTPRINT"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "STMTPRINT"
        sys-ctrl.descrip = "Print Statement Headers on Statement Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.

ASSIGN
    v-print-hdr     = sys-ctrl.log-fld
       //v-use-cust  = sys-ctrl.char-fld EQ "CUST S"
    v-stmt-char     = sys-ctrl.char-fld
    d-print-fmt-dec = sys-ctrl.dec-fld
    vcDefaultForm   = v-stmt-char.

RUN pRunFormatValueChanged .

RUN SetEmailBoxes.

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    v-dir = users.user_program[2].
ELSE
    v-dir = "c:\tmp".

APPLY "entry" TO stmt-date.
END.
RUN sys/ref/CustList.p (INPUT cocode,
    INPUT 'AR4',
    INPUT NO,
    OUTPUT glCustListActive).
{sys/inc/chblankcust.i ""AR4""}

IF ou-log THEN 
DO:
    ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list                                     = YES 
        .
    RUN SetCustRange(INPUT tb_cust-list).
END.
ELSE
    ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
        .
IF ou-log AND ou-cust-int = 0 THEN 
DO:
    ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list                                     = NO
        .
    RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
END.


IF NOT lAsiUser THEN
    RUN_format:HIDDEN IN FRAME FRAME-A = YES .
ELSE 
    RUN_format:SCREEN-VALUE IN FRAME FRAME-A = v-stmt-char .

IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchMail C-Win 
PROCEDURE BatchMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER icBegCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icEndCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBatchMode  AS LOG  NO-UNDO.

    FOR EACH b1-cust NO-LOCK
        WHERE b1-cust.company = cocode
        AND b1-cust.cust-no GE icBegCustNo
        AND b1-cust.cust-no LE icEndCustNo:

        IF icBatchMode THEN
        DO:
            vlSkipRec = YES.

            FOR EACH phone    NO-LOCK 
                WHERE phone.table_rec_key     = b1-cust.rec_key:

                IF CAN-FIND (FIRST emaildtl NO-LOCK 
                    WHERE emaildtl.emailcod  BEGINS 'R-STMT'
                    AND emaildtl.table_rec_key  = phone.rec_key) THEN 
                DO:

                    vlSkipRec = NO.
                    LEAVE.
                END.
            END.

            IF vlSkipRec THEN NEXT.
        END.

        EMPTY TEMP-TABLE tt-inv.

        RUN run-report-mail (b1-cust.cust-no).

        IF NOT CAN-FIND (FIRST tt-inv) THEN NEXT.

        STATUS DEFAULT 'Now processing CUST: ' + b1-cust.cust-no + '....'.

        RUN GenerateMail.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.

    IF iplList THEN 
    DO:
        RUN sys/ref/CustList.p (INPUT ipcCompany,
            INPUT 'AR4',
            INPUT YES,
            OUTPUT lActive).
        FOR EACH ttCustList NO-LOCK 
            WHERE ttCustList.log-fld EQ YES :
            CREATE tt-cust-check .
            ASSIGN 
                tt-cust-check.cust-no = ttCustList.cust-no 
                tt-cust-check.log-fld = YES .
        END.
    END.
    ELSE 
    DO:
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
        INPUT 'AR4').
    FOR EACH ttCustList NO-LOCK 
        WHERE ttCustList.log-fld EQ YES : 
        CREATE tt-cust-check .
        ASSIGN 
            tt-cust-check.cust-no = ttCustList.cust-no 
            tt-cust-check.log-fld = YES .
    END.

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
    DISPLAY stmt-date tb_cust-list begin_cust-no end_cust-no stmt-msg fi_contact 
        lbl_detailed tb_detailed lbl_past-due tb_past-due lbl_curr-bal 
        tb_curr-bal tb_HideDialog rd-dest tb_BatchMail tbAutoClose 
        run_format
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 stmt-date tb_cust-list btnCustList begin_cust-no 
        end_cust-no stmt-msg fi_contact tb_detailed tb_past-due tb_curr-bal 
        tb_HideDialog rd-dest tb_BatchMail tbAutoClose run_format btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail C-Win 
PROCEDURE GenerateMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-lv-ornt AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF is-xprint-form THEN 
        DO:

            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

            IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name + '.pdf').
            ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name + '.pdf').
        END.

        ELSE 
        DO:

            /*Print PDF attachment*/
            IF (v-stmt-char EQ "" OR v-stmt-char EQ "ASI") THEN
            DO:
                OS-COMMAND SILENT VALUE("copy /y " + list-name + " " + list-name + ".txt").
                list-name = list-name + ".txt".
        
            END.

            IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name).
            ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name).
        END.
    END.

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
    DEFINE INPUT PARAMETER ip-cust-from AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-cust-to AS CHARACTER NO-UNDO.

    CASE rd-dest:
        WHEN 1 THEN RUN output-to-printer.
        WHEN 2 THEN RUN output-to-screen.
        WHEN 3 THEN RUN output-to-file.
        WHEN 4 THEN 
            DO:
                /*run output-to-fax.*/
                IF is-xprint-form THEN 
                DO:
                    RUN output-to-fax-prt. /* create tif file */              
                    {custom/asifaxm3.i &TYPE="customer"
                              &begin_cust=ip-cust-from
                              &END_cust=ip-cust-to
                              &fax-subject="Statement"
                              &fax-body="statement"
                              &fax-file=lv-fax-image
                              &end-widget=end_cust-no}      
                END.
                ELSE 
                DO:
                    {custom/asifax3.i &TYPE = "CUSTOMER"
                      &begin_cust=ip-cust-from
                      &END_cust=ip-cust-to
                      &fax-subject=c-win:title
                      &fax-body=c-win:title
                      &fax-file=list-name
                      &end-widget=end_cust-no }
                END.     
            END.
        WHEN 5 THEN
        RUN output-to-mail(INPUT ip-cust-from, INPUT ip-cust-to).

        WHEN 6 THEN RUN output-to-port.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax-prt C-Win 
PROCEDURE output-to-fax-prt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-file-name AS cha FORM "x(60)" NO-UNDO.
    DEFINE VARIABLE lv-xpr-file  AS cha FORM "x(60)" NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        lv-xpr-file = FILE-INFO:FULL-PATHNAME.

        RUN printfile (lv-xpr-file).
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cust-no-from AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-cust-no-to AS CHARACTER NO-UNDO.

    IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
        RUN BatchMail (ip-cust-no-from, ip-cust-no-from, NO).
    ELSE
        RUN BatchMail (ip-cust-no-from, ip-cust-no-to, YES).

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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-asistmt C-Win 
PROCEDURE run-asistmt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    DEFINE VARIABLE v-first   AS LOG NO-UNDO.
    DEFINE VARIABLE v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

    {sys/form/r-top.f}

    DEFINE VARIABLE v-stmt-date      AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Statement Date".
    DEFINE VARIABLE v-lo-cust        LIKE cust.cust-no LABEL "From Customer#" NO-UNDO.
    DEFINE VARIABLE v-hi-cust        LIKE cust.cust-no LABEL "Thru Customer#" NO-UNDO.
    DEFINE VARIABLE v-msg            AS CHARACTER NO-UNDO FORMAT 'x(40)' LABEL "Statement Message".
    DEFINE VARIABLE v-detail         AS LOG       FORMAT "yes/no" LABEL "Print Detail?" NO-UNDO.
    DEFINE VARIABLE v-past-due       AS LOG       NO-UNDO FORMAT "yes/no" LABEL "Print Past Due Only?".

    DEFINE VARIABLE xx               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE yy               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE zz               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-print-align    AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-align-ok       AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE save_id          AS RECID     NO-UNDO.
    DEFINE VARIABLE v-balance        AS DECIMAL   LABEL "Balance" FORMAT '->>,>>>,>>>.99CR' NO-UNDO.
    DEFINE VARIABLE v-age            AS INTEGER   NO-UNDO. /* number of days old */
    DEFINE VARIABLE v-per            AS INTEGER   NO-UNDO. /* hash of v-age into aging periods */
    DEFINE VARIABLE v-aged           AS DECIMAL   NO-UNDO EXTENT 5 FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE v-days-in-per    AS INTEGER   NO-UNDO INIT 30.
    DEFINE VARIABLE ln-total         AS INTEGER   NO-UNDO INIT 48.
    DEFINE VARIABLE adv              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ws_letterhead    AS CHARACTER FORMAT 'x(80)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE ws_addr          AS CHARACTER FORMAT 'x(35)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE code-legend      AS CHARACTER FORMAT 'X(80)' NO-UNDO.
    DEFINE VARIABLE v-asi-excel      AS LOG       NO-UNDO.

    DEFINE VARIABLE v-last-amt       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-last-ref#      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-last-paydate   AS DATE      NO-UNDO.

    DEFINE VARIABLE ld-due           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE terms_dscr       AS CHARACTER FORMAT "x(30)" NO-UNDO .

    /* 07.11.95 by CAH @ASI:
    1.  There is no ar transaction type file in system, so the following
    vars have been added to support structured definition via lookups.
    */
    DEFINE VARIABLE msgx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-type-descr AS CHARACTER FORMAT 'x(30)' NO-UNDO.
    DEFINE VARIABLE v-inv-type-list  AS CHARACTER NO-UNDO INIT "I,CR,DR,P,DA,FC,R".
    DEFINE VARIABLE v-inv-type-max   AS INTEGER   NO-UNDO.
    v-inv-type-max = NUM-ENTRIES(v-inv-type-list).
    DEFINE VARIABLE v-inv-type-array AS CHARACTER NO-UNDO EXTENT 7 INIT
        ["Invoice",
        "CR Memo",
        "DR Memo",
        "Payment",
        "Disc Allowed",
        "Finance Chg",
        "Return"].

    code-legend = "CODES: ".
    DO xx = 1 TO v-inv-type-max:
        code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
            + v-inv-type-array[xx] + ' '.
    END.

    v-asi-excel = IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN TRUE ELSE FALSE.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*was 3*/
        "Statement date" AT 50   "Account#" AT 65 SKIP
        ws_addr[2]    AT 11
        "--------------" AT 50   "--------" AT 65 SKIP
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(1)
        "=============================== S T A T E M E N T ============================"
        SKIP
        WITH FRAME stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date 
        tt-inv.type FORM "x(3)"
        tt-inv.inv-no FORMAT ">>>>>>>9"
        tt-inv.description FORM "x(25)"
        tt-inv.old-day FORMAT ">>>9"
        tt-inv.amount      
        v-balance       
        WITH FRAME stmt-line NO-BOX STREAM-IO WIDTH 85 DOWN NO-LABELS.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*3*/
        ws_addr[2]    AT 11
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(4)
        SKIP
        WITH FRAME no-stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date
        tt-inv.type
        tt-inv.inv-no FORMAT ">>>>>>>9"
        tt-inv.DESCRIPTION FORM "x(25)"
        tt-inv.amount
        v-balance
        WITH FRAME no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME no-stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(3)
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME no-stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    DEFINE VARIABLE ls-image1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-full-img1 AS cha       FORM "x(200)" NO-UNDO.

    IF v-stmt-char EQ "RFC" OR v-stmt-char EQ "Badger" THEN 
    DO:
        RUN FileSys_GetBusinessFormLogo(cocode, "" /* cust */ , "" /* location */ , OUTPUT cRtnChar, OUTPUT lValid, OUTPUT cMessage).
        IF NOT lValid THEN
        DO:
	        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        END.   
    END.

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

    ASSIGN 
        ls-image1           = IF v-stmt-char = "Premier" THEN "images\premierinv.jpg"
                   ELSE IF v-stmt-char = "LoyLang" THEN "images\loystmt.jpg"
                   ELSE IF v-stmt-char = "Printers" THEN "images\loyprinters.jpg"
                 /*  ELSE IF v-stmt-char = "Badger" THEN "images\badger statement.JPG" */
                   ELSE IF v-stmt-char = "RFC" THEN cRtnChar 
                   ELSE IF v-stmt-char = "Badger" THEN cRtnChar 
                   ELSE IF v-stmt-char = "StmtPrint-Mex" THEN "images\premierinv.jpg"
                   ELSE "images\asilogo.jpg"
        FILE-INFO:FILE-NAME = ls-image1
        ls-full-img1        = FILE-INFO:FULL-PATHNAME + ">".
    /*
    IF v-use-cust THEN
       FIND FIRST cust WHERE
            cust.company EQ cocode AND
            cust.active  EQ "S"
            NO-LOCK NO-ERROR.*/


    IF v-print-hdr AND AVAILABLE company THEN 
    DO:
        yy = 1.

        IF company.name    NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.name
                yy                = yy + 1.

        IF company.addr[1] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[1]
                yy                = yy + 1.

        IF company.addr[2] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[2]
                yy                = yy + 1.

        IF company.city    NE "" OR
            company.state   NE "" OR
            company.zip     NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
                yy                = yy + 1.

        DO xx = 1 TO 6:
            IF ws_letterhead[xx] GT '' THEN
                ws_letterhead[xx] = FILL(" ", int((80 - length(ws_letterhead[xx])) / 2))
                    + ws_letterhead[xx].
        END.
    END.

    ASSIGN
        str-tit2    = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 56}

        v-stmt-date = stmt-date
        v-msg       = stmt-msg
        v-detail    = tb_detailed
        v-past-due  = tb_past-due.

    IF ip-sys-ctrl-shipto THEN
        ASSIGN
            v-lo-cust = ip-cust-no
            v-hi-cust = ip-cust-no.
    ELSE
        ASSIGN
            v-lo-cust = ""
            v-hi-cust = "" .

        {sys/inc/print1.i}

    {sys/inc/outprint.i  value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF NOT v-asi-excel THEN
        is-xprint-form = YES.
    ELSE
        is-xprint-form = NO.

    IF is-xprint-form THEN 
    DO:
        CASE rd-dest :
            WHEN 1 THEN 
                DO:
                    IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN 
                        PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                    ELSE PUT "<PRINTER?>" FORM "x(30)".
                END.
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN 
                    DO:      
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><MODAL=NO>" FORM "x(120)". 
                        ELSE
                            PUT "<PREVIEW><MODAL=NO>" FORM "x(120)". 
                    END.
                    ELSE 
                    DO:
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                        ELSE PUT "<PREVIEW>" FORM "x(30)".
                    END.
                END.
            WHEN 4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
                    PUT UNFORMATTED 
                        "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
                END.        
            WHEN 5 THEN 
                DO:
                    IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN 
                    DO:
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW><PDF-LEFT=" + trim(STRING(3 + d-print-fmt-dec)) + "mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
                        ELSE PUT "<PREVIEW><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
                    END.
                    ELSE 
                    DO:
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW=PDF><PDF-LEFT=" + trim(STRING(3 + d-print-fmt-dec)) + "mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                        ELSE PUT "<PREVIEW=PDF><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                    END.
                END.
        END CASE.
        PUT "</PROGRESS>".
    END.


    SESSION:SET-WAIT-STATE ("general").
    v-first = YES.

    EMPTY TEMP-TABLE tt-inv.
    EMPTY TEMP-TABLE tt-cust-excel.

    FOR EACH ttCustList
        WHERE ttCustList.log-fld
        NO-LOCK,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ ttCustList.cust-no
        AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
        AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
        AND ((cust.acc-bal NE 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        BREAK BY cust.cust-no
        TRANSACTION:

        IF NOT v-asi-excel THEN
            EMPTY TEMP-TABLE tt-inv.

        ASSIGN 
            v-last-amt     = 0
            v-last-ref#    = ""
            v-last-paydate = ?.

        IF v-past-due THEN
        DO:
            FIND FIRST ar-inv WHERE ar-inv.company EQ cust.company    AND
                ar-inv.cust-no EQ cust.cust-no AND
                ar-inv.posted                AND
                ar-inv.due NE 0              AND
                ar-inv.inv-date LE v-stmt-date AND
                ar-inv.due-date LE v-stmt-date NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ar-inv THEN NEXT.
        END.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cust.company
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.posted   EQ YES
            /*and ar-inv.due      ne 0           */
            AND ar-inv.terms    NE "CASH"      
            AND ar-inv.inv-date LE v-stmt-date
            NO-LOCK:

            FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                ld-due = ar-inv.net.
            ELSE
                ld-due = ar-inv.gross.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES  
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-stmt-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 THEN
                        ld-due = ld-due - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                            ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END.

            IF ld-due NE 0 THEN 
            DO:
                FIND FIRST tt-inv 
                    WHERE tt-inv.cust-no EQ cust.cust-no
                    AND tt-inv.DESCRIPTION EQ "No Balance Due"
                    NO-ERROR.
                IF AVAILABLE tt-inv THEN DELETE tt-inv.
                CREATE tt-inv.
                ASSIGN
                    tt-inv.cust-no    = cust.cust-no
                    tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
                    tt-inv.inv-date   = ar-inv.inv-date
                    tt-inv.trans-date = ar-inv.inv-date
                    tt-inv.old-day    = (TODAY - ar-inv.inv-date)
                    tt-inv.inv-no     = ar-inv.inv-no
                    tt-inv.type       = IF ar-inv.type GT ' ' THEN ar-inv.type ELSE 'I'
                    tt-inv.amount     = IF v-detail THEN
                           IF ar-inv.net     EQ
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt THEN ar-inv.net ELSE ar-inv.gross
                          ELSE ar-inv.due
                    tt-inv.po-no      = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
                    tt-inv.bol-no     = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "").

                IF v-stmt-char = "Printers" THEN
                    tt-inv.bol-no = (IF AVAILABLE ar-invl AND ar-invl.job-no NE "" 
                        THEN "  " + ar-invl.job-no
                        ELSE IF AVAILABLE ar-invl AND ar-invl.ord-no <> 0 
                        THEN STRING(ar-invl.ord-no,">>>>>>>>") 
                        ELSE "").
                IF v-detail THEN
                    FOR EACH ar-cashl
                        WHERE ar-cashl.company  EQ ar-inv.company
                        AND ar-cashl.posted   EQ YES
                        AND ar-cashl.cust-no  EQ ar-inv.cust-no
                        AND ar-cashl.inv-no   EQ ar-inv.inv-no
                        USE-INDEX inv-no NO-LOCK,

                        EACH ar-cash
                        WHERE ar-cash.c-no       EQ ar-cashl.c-no
                        AND ar-cash.check-date LE v-stmt-date
                        USE-INDEX c-no NO-LOCK:

                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
                            tt-inv.inv-date    = ar-inv.inv-date
                            tt-inv.old-day     = (TODAY - ar-inv.inv-date)
                            tt-inv.trans-date  = ar-cash.check-date
                            tt-inv.inv-no      = ar-cashl.inv-no
                            tt-inv.description = ar-cashl.dscr
                            tt-inv.po-no       = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
                            tt-inv.bol-no      = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "").

                        IF v-stmt-char = "Printers" THEN
                            tt-inv.bol-no = (IF AVAILABLE ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no 
                                ELSE IF AVAILABLE ar-invl AND ar-invl.ord-no <> 0 
                                THEN STRING(ar-invl.ord-no,">>>>>>>>") 
                                ELSE "").

                        IF ar-cashl.memo THEN
                            IF ar-cashl.amt-disc NE 0 THEN
                                ASSIGN
                                    tt-inv.type   = "R"
                                    tt-inv.amount = ar-cashl.amt-disc * -1.

                            ELSE  
                                IF ar-cashl.amt-paid + ar-cashl.amt-disc LE 0 THEN
                                    ASSIGN
                                        tt-inv.type   = "CM"
                                        tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

                                ELSE
                                    ASSIGN
                                        tt-inv.type   = "DM"
                                        tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

                        ELSE
                            ASSIGN
                                tt-inv.type   = "P"
                                tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

                    END.
            END.
            ELSE IF tb_curr-bal THEN 
                DO:
                    IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
                        THEN 
                    DO: 
                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.DESCRIPTION = "No Balance Due"
                            .
                    END.
                END.
        END.

        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.inv-no     EQ 0
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.amt-paid   NE 0
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no     = cust.cust-no
                tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
                tt-inv.inv-date    = ar-cashl.inv-date
                tt-inv.old-day     = (TODAY - ar-cashl.inv-date)
                tt-inv.trans-date  = ar-cash.check-date
                tt-inv.inv-no      = ar-cashl.inv-no
                tt-inv.description = ar-cashl.dscr
                tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/ /* on-account dont display PO# */
                tt-inv.bol-no      = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "").

            IF ar-cashl.memo THEN
                ASSIGN
                    tt-inv.amount = ar-cashl.amt-paid
                    tt-inv.type   = IF tt-inv.amount LT 0 THEN "CR" ELSE "DR".

            ELSE
                ASSIGN
                    tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
                    tt-inv.type   = "P".

        /*IF tt-inv.TYPE = "P" THEN
           IF tt-inv.trans-date > v-last-paydate OR v-last-paydate = ? THEN 
              ASSIGN v-last-amt     = tt-inv.amount
                     v-last-ref#    = string(tt-inv.inv-no)
                     v-last-paydate = tt-inv.trans-date.  
        */                 
        END.                                                

        /* to get last payment amt, check, date */
        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            /*and ar-cashl.on-account eq YES*/
            AND ar-cashl.amt-paid   NE 0
            AND ar-cashl.memo       EQ NO
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
                ASSIGN v-last-amt     = ar-cashl.amt-paid
                    v-last-ref#    = STRING(ar-cash.check-no)
                    v-last-paydate = ar-cash.check-date.  
            ELSE
                IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
                    ASSIGN v-last-amt = v-last-amt + ar-cashl.amt-paid.
        END.

        ASSIGN
            v-balance = 0 /* reset running balance */
            v-aged    = 0. /* clear aging buckets */
        CLEAR FRAME stmt-header NO-PAUSE.
        CLEAR FRAME stmt-line ALL NO-PAUSE.
        CLEAR FRAME stmt-total NO-PAUSE.
        ws_addr = ''.
        IF AVAILABLE cust THEN
            ASSIGN
                ws_addr[1] = cust.name
                ws_addr[2] = cust.addr[1]
                ws_addr[3] = cust.addr[2]
                ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
        DO yy = 1 TO 6:
            DO zz = yy + 1 TO 6:
                IF ws_addr[yy] EQ '' AND ws_addr[zz] GT ''
                    THEN
                    ASSIGN ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
            END.
        END.

        FOR EACH tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
            BREAK BY "1"
            BY tt-inv.cust-no
            BY tt-inv.inv-date
            BY tt-inv.sort-fld
            BY tt-inv.trans-date:

            IF v-asi-excel AND tt-inv.cust-no NE cust.cust-no THEN
                NEXT.

            IF AVAILABLE cust THEN
                FIND FIRST terms WHERE terms.company EQ cocode
                    AND terms.t-code EQ cust.terms NO-LOCK NO-ERROR .
            IF AVAILABLE terms THEN
                ASSIGN terms_dscr = terms.dscr .

            IF NOT v-asi-excel AND (FIRST-OF ("1") OR (LINE-COUNTER GT ln-total)) THEN 
            DO:
                IF NOT v-first THEN PAGE.
                IF v-stmt-char = "Premier" THEN
                    ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                        v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                        v-remitto[3] = "<C40>          CHICAGO, IL 60686".
                ELSE IF v-stmt-char = "StmtPrint-Mex" THEN
                        ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                            v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                            v-remitto[3] = "<C40>          CHICAGO, IL 60686".
                    ELSE v-remitto = "".

                IF v-stmt-char = "Premier" THEN
                    PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                        "<=1><R+6><C+54><B><P22>Statement</B><P12>" SKIP
                        "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
                        "<=1><R+9><C+68><FROM><C+10><LINE>" 
                        "<=1><R+9><C+52>" v-stmt-date
                        "<=1><R+9><C+68>" cust.cust-no SKIP
                        "<=1><R+11><C1>" ws_addr[1] v-remitto[1] SKIP
                        "<=1><R+12><C1>" ws_addr[2] v-remitto[2] SKIP 
                        "<=1><R+13><C1>" ws_addr[3] v-remitto[3] SKIP
                        "<=1><R+14><C1>" ws_addr[4] v-remitto[4] SKIP
                        /*"<=1><R+15><C1>" ws_addr[5] skip*/
                        "<=1><R+15><C1>Terms : " terms_dscr SKIP
                        "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
                        "<=1><R+18><FROM><C+80><LINE>"
                        . 

                ELSE IF v-stmt-char = "StmtPrint-Mex" THEN
                        PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                            "<=1><R+6><C+54><B><P22>Declaración</B><P12>" SKIP
                            "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
                            "<=1><R+9><C+68><FROM><C+10><LINE>" 
                            "<=1><R+9><C+52>" v-stmt-date
                            "<=1><R+9><C+68>" cust.cust-no SKIP
                            "<=1><R+11><C1>" ws_addr[1] v-remitto[1] SKIP
                            "<=1><R+12><C1>" ws_addr[2] v-remitto[2] SKIP 
                            "<=1><R+13><C1>" ws_addr[3] v-remitto[3] SKIP
                            "<=1><R+14><C1>" ws_addr[4] v-remitto[4] SKIP
                            /*"<=1><R+15><C1>" ws_addr[5] skip*/
                            "<=1><R+15><C1>Condiciones : " terms_dscr SKIP
                            "<=1><R+17>Fecha    Código Ref# Descripción   <C54>Cantidad     Equilibrar" SKIP
                            "<=1><R+18><FROM><C+80><LINE>"
                            . 

                    ELSE IF v-stmt-char = "LoyLang" THEN
                            PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                "<=1><R+10><C+52>" v-stmt-date
                                "<=1><R+10><C+68>" cust.cust-no SKIP
                                "<=1><R+11><C1>" ws_addr[1] SKIP
                                "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
                                "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
                                "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                "<=1><R+13><C40><FROM><C+17><LINE>"
                                "<=1><R+13><C58><FROM><C+8><LINE>" 
                                "<=1><R+13><C67><FROM><C+10><LINE>"      SKIP 
                                "<=1><R+13><C40>" v-last-amt 
                                "<=1><R+13><C58>" v-last-paydate
                                "<=1><R+13><C69>" v-last-ref#            SKIP
                                "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                "<=1><R+17>         Trans"               SKIP
                                "<=1><R+18>Date     Type   Ref#     BOL#   Customer PO#    <C56>Amount        Balance" SKIP
                                "<=1><R+19><FROM><C+80><LINE>"
                                .
                        ELSE IF v-stmt-char = "Printers" THEN
                                PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                    "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                    "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                    "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                    "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                    "<=1><R+10><C+52>" v-stmt-date
                                    "<=1><R+10><C+68>" cust.cust-no SKIP
                                    "<=1><R+11><C1>" ws_addr[1] SKIP
                                    "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
                                    "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
                                    "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                    "<=1><R+13><C40><FROM><C+17><LINE>"
                                    "<=1><R+13><C58><FROM><C+8><LINE>" 
                                    "<=1><R+13><C67><FROM><C+10><LINE>"      SKIP 
                                    "<=1><R+13><C40>" v-last-amt 
                                    "<=1><R+13><C58>" v-last-paydate
                                    "<=1><R+13><C69>" v-last-ref#            SKIP
                                    "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                    "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                    "<=1><R+17>         Trans"               SKIP
                                    "<=1><R+18>Date     Type   Inv#     Job#   Customer PO#    <C56>Amount        Balance" SKIP
                                    "<=1><R+19><FROM><C+80><LINE>"
                                    . 
                            ELSE IF v-stmt-char = "Badger" THEN
                                    PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                        "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                        "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                        "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                        "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                        "<=1><R+10><C+52>" v-stmt-date
                                        "<=1><R+10><C+68>" cust.cust-no SKIP
                                        "<=1><R+11><C1>" ws_addr[1] SKIP
                                        "<=1><R+12><C1>" ws_addr[2] v-remitto[1] SKIP 
                                        "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                        "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                        "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                        "<=1><R+17>Date     Code  Inv# Description               Days <C60>Amount        Balance" SKIP
                                        "<=1><R+18><FROM><C+82><LINE>"
                                        . 

                                ELSE IF v-stmt-char = "RFC" THEN          /* task 12231305 */
                                        PUT "<FMS Sans Serif><R2><C1><#1><R+12><C+25><IMAGE#1=" ls-full-img1 SKIP
                                            /*"<=1><R+3><C+26> 2066 S. East Avenue" 
                                            "<=1><R+4><C+26> Vineland, NJ 08360" 
                                            "<=1><R+5><C+26> Phone: 856-692-0404" 
                                            "<=1><R+6><C+26> Fax: (856) 692-2085" */
                                            "<=1><R+10><C+54><B><P22>Statement</B><P12>" SKIP
                                            "<=1><R+12>" "<C53>Statement Date           Account #" SKIP
                                            "<=1><R+13><C+52><FROM><C+13><LINE>" SKIP
                                            "<=1><R+13><C+68><FROM><C+10><LINE>" 
                                            "<=1><R+13><C+52>" v-stmt-date
                                            "<=1><R+13><C+68>" cust.cust-no SKIP
                                            "<=1><R+12><C18>Attn:" cust.contact SKIP
                                            "<=1><R+13><C18>" ws_addr[1] SKIP                    /*Task# 01031416*/
                                            "<=1><R+14><C18>" ws_addr[2] v-remitto[1] SKIP 
                                            "<=1><R+15><C18>" ws_addr[3] v-remitto[2] SKIP
                                            "<=1><R+16><C18>" ws_addr[4] v-remitto[3] SKIP
                                            "<=1><R+17><C18>" ws_addr[5] v-remitto[4] SKIP
                                            "<=1><R+20><C1>Date        <C9>Code      <C16>Ref#     <C25>Description   <C57>Amount       <C71>Balance" SKIP(1)
                                            "<=1><R+21><FROM><C+80><LINE>"
                                            . 
                                    ELSE IF v-stmt-char = "ASIXprnt-CAN" THEN
                                            PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                                "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                                "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                                "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                                "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                                "<=1><R+10><C+52>" v-stmt-date
                                                "<=1><R+10><C+68>" cust.cust-no SKIP
                                                "<=1><R+11><C1>" ws_addr[1] SKIP
                                                "<=1><R+12><C1>" ws_addr[2] v-remitto[1] SKIP 
                                                "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                                "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                                "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                                "<=1><R+17>Date     Code  Ref#  Description   <C49.5>Montant/Amount       Balance" SKIP
                                                "<=1><R+18><FROM><C+80><LINE>"
                                                . 
                                        ELSE
                                            PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                                "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                                "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                                "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                                "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                                "<=1><R+10><C+52>" v-stmt-date
                                                "<=1><R+10><C+68>" cust.cust-no SKIP
                                                "<=1><R+11><C1>" ws_addr[1] SKIP
                                                "<=1><R+12><C1>" ws_addr[2] v-remitto[1] SKIP 
                                                "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                                "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                                "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                                "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
                                                "<=1><R+18><FROM><C+80><LINE>"
                                                . 

                v-first = NO.
            END.

            IF tt-inv.description EQ '' THEN 
            DO:
                msgx = LOOKUP(tt-inv.type,v-inv-type-list).
                IF msgx EQ 0 THEN
                    msgx = 1.    /* assume invoice */
                tt-inv.description =
                    IF msgx GT 0 AND msgx LE v-inv-type-max
                    THEN
                    v-inv-type-array[msgx]
                    ELSE
                    ''.
            END.

            v-balance = v-balance + tt-inv.amount.

            IF NOT v-asi-excel THEN
            DO:
                IF v-stmt-char = "Badger" THEN 
                DO:
                    DISPLAY
                        tt-inv.trans-date
                        tt-inv.type
                        tt-inv.inv-no  
                        WHEN tt-inv.inv-no GT 0                                        
                        (IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN tt-inv.bol-no + " " + string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description
                        /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description*/
                        tt-inv.old-day
                        tt-inv.amount
                        v-balance 
                        WITH FRAME stmt-line .
                    DOWN 1 WITH FRAME stmt-line.
                END.
                ELSE IF v-stmt-char = "RFC" THEN 
                    DO:
                        PUT
                            "<C1>" tt-inv.trans-date
                            "<C9>" tt-inv.type
                            "<C15>" tt-inv.inv-no  /*when tt-inv.inv-no gt 0*/
                            /*tt-inv.description*/
                            "<C25>" tt-inv.description
                            /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description)*/
                            "<C54>" tt-inv.amount
                            "<C68>" v-balance  SKIP.
                    /*with frame no-stmt-line.
                  down 1 with frame no-stmt-line.*/
                    END.
                    ELSE 
                    DO:
                        DISPLAY
                            tt-inv.trans-date
                            tt-inv.type
                            tt-inv.inv-no  
                            WHEN tt-inv.inv-no GT 0
                            /*tt-inv.description*/
                            (IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN tt-inv.bol-no + " " + string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description
                            /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description)*/
                            tt-inv.amount
                            v-balance
                            WITH FRAME no-stmt-line.
                        DOWN 1 WITH FRAME no-stmt-line.
                    END.
            END.

            v-age = v-stmt-date - tt-inv.inv-date.
            IF v-age = ? OR v-age LT 0 THEN v-age = 0.
            IF v-stmt-char = "Badger" THEN 
                v-per = trunc(v-age / (v-days-in-per + 1), 0) + 1.
            ELSE        
                v-per = trunc(v-age / v-days-in-per, 0) + 1.
            IF v-per GT 5 THEN
                v-per = 5.
            v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

            IF LAST-OF ("1") THEN 
            DO:

                IF NOT v-asi-excel THEN
                DO:
                    PUT SKIP(1).

                    IF v-print-hdr THEN
                        DISPLAY
                            v-msg
                            v-balance
                            WITH FRAME stmt-total-line.

                    ELSE
                        DISPLAY
                            v-msg
                            v-balance
                            WITH FRAME no-stmt-total-line.

                    IF v-stmt-char = "Badger" THEN 
                        PUT "<R57><C1><#2>"SKIP
                            "<=2>      Current             31 - 60             61 - 90            >90 Days" SKIP
                            "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                            "<=2><R+2>" v-aged[1] AT 12 v-aged[2] AT 30  v-aged[3] AT 50  (v-aged[4] + v-aged[5]) AT 70
                            SKIP(1).
                    ELSE IF v-stmt-char = "StmtPrint-Mex" THEN
                            PUT "<R57><C1><#2>"SKIP
                                "<=2>      Corriente         30 dias         60 dias         90 dias        >90 dias" SKIP
                                "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                                "<=2><R+2>" v-aged[1 for 5]
                                SKIP(1).
                        ELSE
                            PUT "<R57><C1><#2>"SKIP
                                "<=2>      Current         30 Days         60 Days         90 Days        >90 Days" SKIP
                                "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                                "<=2><R+2>" v-aged[1 for 5]
                                SKIP(1).

                    IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
                        PUT "<R62><C1><#3>"SKIP
                            "<=3><R+1><C1>" code-legend SKIP
                            "<R+1><C+80><RECT#3>" 
                            SKIP. 

                END.
                ELSE
                DO:
                    CREATE tt-cust-excel.

                    ASSIGN 
                        tt-cust-excel.cust-no = tt-inv.cust-no
                        tt-cust-excel.contact = cust.contact
                        tt-cust-excel.terms   = cust.terms
                        tt-cust-excel.addr[1] = ws_addr[1]
                        tt-cust-excel.addr[2] = ws_addr[2]
                        tt-cust-excel.addr[3] = ws_addr[3]
                        tt-cust-excel.addr[4] = ws_addr[4]
                        tt-cust-excel.addr[5] = ws_addr[5]
                        tt-cust-excel.aged[1] = v-aged[1]
                        tt-cust-excel.aged[2] = v-aged[2]
                        tt-cust-excel.aged[3] = v-aged[3]
                        tt-cust-excel.aged[4] = v-aged[4]
                        tt-cust-excel.aged[5] = v-aged[5].
                    FIND FIRST terms NO-LOCK
                        WHERE terms.company = cust.company
                        AND terms.t-code EQ cust.terms NO-ERROR.
                    IF AVAILABLE terms THEN
                        tt-cust-excel.terms =  terms.dscr .
                    RELEASE tt-cust-excel.
                END.
            END.
        END.  /* for each tt-inv */

    END. /* for each cust record */

    IF v-asi-excel THEN 
    DO:
        IF v-stmt-char EQ "ASIExcel" THEN
            RUN arrep\asiexlstmt.p(INPUT v-stmt-date, INPUT v-msg).
        ELSE IF v-stmt-char EQ "SouleExcel" THEN
                RUN arrep\SouleExlStmt.p(INPUT v-stmt-date, INPUT v-msg).
    END.   

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-asistmt-mail C-Win 
PROCEDURE run-asistmt-mail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-first   AS LOG NO-UNDO.
    DEFINE VARIABLE v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

{sys/form/r-top.f}

    DEFINE VARIABLE v-stmt-date      AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Statement Date".
    DEFINE VARIABLE v-lo-cust        LIKE cust.cust-no LABEL "From Customer#" NO-UNDO.
    DEFINE VARIABLE v-hi-cust        LIKE cust.cust-no LABEL "Thru Customer#" NO-UNDO.
    DEFINE VARIABLE v-msg            AS CHARACTER NO-UNDO FORMAT 'x(40)' LABEL "Statement Message".
    DEFINE VARIABLE v-detail         AS LOG       FORMAT "yes/no" LABEL "Print Detail?" NO-UNDO.
    DEFINE VARIABLE v-past-due       AS LOG       NO-UNDO FORMAT "yes/no" LABEL "Print Past Due Only?".

    DEFINE VARIABLE xx               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE yy               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE zz               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-print-align    AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-align-ok       AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-print          LIKE ar-inv.printed NO-UNDO.
    DEFINE VARIABLE save_id          AS RECID     NO-UNDO.
    DEFINE VARIABLE v-balance        AS DECIMAL   LABEL "Balance" FORMAT '->>,>>>,>>>.99CR'.
    DEFINE VARIABLE v-age            AS INTEGER   NO-UNDO. /* number of days old */
    DEFINE VARIABLE v-per            AS INTEGER   NO-UNDO. /* hash of v-age into aging periods */
    DEFINE VARIABLE v-aged           AS DECIMAL   NO-UNDO EXTENT 5 FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE v-days-in-per    AS INTEGER   NO-UNDO INIT 30.
    DEFINE VARIABLE ln-total         AS INTEGER   NO-UNDO INIT 48.
    DEFINE VARIABLE adv              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ws_letterhead    AS CHARACTER FORMAT 'x(80)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE ws_addr          AS CHARACTER FORMAT 'x(35)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE code-legend      AS CHARACTER FORMAT 'X(80)' NO-UNDO.

    DEFINE VARIABLE ld-due           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-last-amt       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-last-ref#      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-last-paydate   AS DATE      NO-UNDO.

    DEFINE VARIABLE terms_dscr       AS CHARACTER FORMAT "x(30)" NO-UNDO.

    /* 07.11.95 by CAH @ASI:
    1.  There is no ar transaction type file in system, so the following
    vars have been added to support structured definition via lookups.
    */
    DEFINE VARIABLE msgx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-type-descr AS CHARACTER FORMAT 'x(30)' NO-UNDO.
    DEFINE VARIABLE v-inv-type-list  AS CHARACTER NO-UNDO INIT "I,CR,DR,P,DA,FC,R".
    DEFINE VARIABLE v-inv-type-max   AS INTEGER   NO-UNDO.
    v-inv-type-max = NUM-ENTRIES(v-inv-type-list).
    DEFINE VARIABLE v-inv-type-array AS CHARACTER NO-UNDO EXTENT 7 INIT
        ["Invoice",
        "CR Memo",
        "DR Memo",
        "Payment",
        "Disc Allowed",
        "Finance Chg",
        "Return"].

    code-legend = "CODES: ".
    DO xx = 1 TO v-inv-type-max:
        code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
            + v-inv-type-array[xx] + ' '.
    END.

    IF v-stmt-char EQ "RFC" OR v-stmt-char EQ "Badger" THEN 
    DO:
        RUN FileSys_GetBusinessFormLogo(cocode, "" /* cust */ , "" /* location */ , OUTPUT cRtnChar, OUTPUT lValid, OUTPUT cMessage).
        IF NOT lValid THEN
        DO:
	        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        END.
    END.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*was 3*/
        "Statement date" AT 50   "Account#" AT 65 SKIP
        ws_addr[2]    AT 11
        "--------------" AT 50   "--------" AT 65 SKIP
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(1)
        "=============================== S T A T E M E N T ============================"
        SKIP
        WITH FRAME stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date 
        tt-inv.type   FORM "x(3)"  
        tt-inv.inv-no FORMAT ">>>>>>>9"
        tt-inv.description FORM "x(25)"
        tt-inv.old-day FORMAT ">>>9"
        tt-inv.amount      
        v-balance       
        WITH FRAME stmt-line NO-BOX STREAM-IO WIDTH 85 DOWN NO-LABELS.

    FORM
        tt-inv.trans-date 
        tt-inv.type FORM "x(3)"
        tt-inv.inv-no FORMAT ">>>>>>>9"
        tt-inv.description FORM "x(20)"
        tt-inv.old-day FORMAT ">>>9"
        tt-inv.amount      
        v-balance       
        WITH FRAME stmt-line-badger NO-BOX STREAM-IO WIDTH 80 DOWN NO-LABELS.



    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME stmt-total-line NO-BOX NO-LABELS STREAM-IO.
    /*
    form
      skip(2)
      "      Current         30 Days         60 Days         90 Days        >90 Days"
      skip
      v-aged[1 for 5] skip(1)
      code-legend skip
      with frame stmt-total no-box no-labels stream-io width 80.
    */
    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*3*/
        ws_addr[2]    AT 11
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(4)
        SKIP
        WITH FRAME no-stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date
        tt-inv.type
        tt-inv.inv-no FORMAT ">>>>>>>9"
        tt-inv.DESCRIPTION FORM "x(25)"
        tt-inv.amount
        v-balance
        WITH FRAME no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME no-stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(3)
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME no-stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    DEFINE VARIABLE ls-image1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-image2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-full-img1 AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE ls-full-img2 AS cha       FORM "x(200)" NO-UNDO.

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
      
    ASSIGN 
        ls-image1 = IF v-stmt-char = "Premier" THEN "images\premierinv.jpg"
                         ELSE IF v-stmt-char = "LoyLang" THEN "images\loystmt.jpg"
                         ELSE IF v-stmt-char = "Printers" THEN "images\loyprinters.jpg"
                     /*    ELSE IF v-stmt-char = "Badger" THEN "images\badger statement.JPG" */
                         ELSE IF v-stmt-char = "RFC" THEN cRtnChar
                         ELSE IF v-stmt-char = "Badger"  THEN cRtnChar
                         ELSE IF v-stmt-char = "StmtPrint-Mex" THEN "images\premierinv.jpg"
                         ELSE "images\asilogo.jpg" .
     
    FILE-INFO:FILE-NAME = ls-image1.
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
    FILE-INFO:FILE-NAME = ls-image2.

    ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

    /*
    IF v-use-cust THEN
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.active  EQ "S"
        NO-LOCK NO-ERROR.*/


    IF v-print-hdr AND AVAILABLE company THEN 
    DO:
        yy = 1.

        IF company.name    NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.name
                yy                = yy + 1.

        IF company.addr[1] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[1]
                yy                = yy + 1.

        IF company.addr[2] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[2]
                yy                = yy + 1.

        IF company.city    NE "" OR
            company.state   NE "" OR
            company.zip     NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
                yy                = yy + 1.

        DO xx = 1 TO 6:
            IF ws_letterhead[xx] GT '' THEN
                ws_letterhead[xx] = FILL(" ", int((80 - length(ws_letterhead[xx])) / 2))
                    + ws_letterhead[xx].
        END.
    END.

    ASSIGN
        str-tit2    = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 56}

        v-stmt-date = stmt-date
        v-lo-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE /*begin_cust-no*/ ""
        v-hi-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE /*end_cust-no*/ ""
        v-msg       = stmt-msg
        v-detail    = tb_detailed
        v-past-due  = tb_past-due.

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.
    is-xprint-form = YES.

    IF is-xprint-form THEN 
    DO:
        CASE rd-dest :
            WHEN 1 THEN 
                DO:
                    IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN 
                        PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                    ELSE PUT "<PRINTER?>" FORM "x(30)".
                END.
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN 
                    DO:      
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><MODAL=NO>" FORM "x(120)". 
                        ELSE
                            PUT "<PREVIEW><MODAL=NO>" FORM "x(30)". 
                    END.
                    ELSE 
                    DO:
                        IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                        ELSE PUT "<PREVIEW>" FORM "x(30)".
                    END.
                END.
            WHEN 4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
                    PUT UNFORMATTED 
                        "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
                END.        
            WHEN 5 THEN 
                DO:
                    IF LOOKUP(v-stmt-char,"stmtprint 1,stmtprint 2") > 0 THEN
                        PUT "<PDF=DIRECT><PDF-LEFT=" + trim(STRING(3 + d-print-fmt-dec)) + "mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
                    ELSE PUT "<PDF=DIRECT><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
                END.
        END CASE.
        PUT "</PROGRESS>".
    END.

    SESSION:SET-WAIT-STATE ("general").
    v-first = YES.
    FOR EACH ttCustlist 
        WHERE ttCustList.log-fld
        NO-LOCK,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode 
        AND cust.cust-no EQ ttCustList.cust-no
        AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
        AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
        AND ((cust.acc-bal NE 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        BREAK BY cust.cust-no
        TRANSACTION:

        FOR EACH tt-inv:
            DELETE tt-inv.
        END.    /* clear workfile */

        ASSIGN 
            v-last-amt     = 0
            v-last-ref#    = ""
            v-last-paydate = ?.

        IF v-past-due THEN
        DO:
            FIND FIRST ar-inv WHERE ar-inv.company EQ cust.company    AND
                ar-inv.cust-no EQ cust.cust-no AND
                ar-inv.posted                AND
                ar-inv.due NE 0              AND
                ar-inv.inv-date LE v-stmt-date AND
                ar-inv.due-date LE v-stmt-date NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ar-inv THEN NEXT.
        END.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cust.company
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.posted   EQ YES
            /*and ar-inv.due      ne 0*/
            AND ar-inv.terms    NE "CASH"
            AND ar-inv.inv-date LE v-stmt-date
            NO-LOCK:

            FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                ld-due = ar-inv.net.
            ELSE
                ld-due = ar-inv.gross.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-stmt-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 THEN
                        ld-due = ld-due - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                            ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END.

            IF ld-due NE 0 THEN 
            DO:
                FIND FIRST tt-inv 
                    WHERE tt-inv.cust-no EQ cust.cust-no
                    AND tt-inv.DESCRIPTION EQ "No Balance Due"
                    NO-ERROR.
                IF AVAILABLE tt-inv THEN DELETE tt-inv.
                CREATE tt-inv.
                ASSIGN
                    tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
                    tt-inv.inv-date   = ar-inv.inv-date
                    tt-inv.old-day    = (TODAY - ar-inv.inv-date)
                    tt-inv.trans-date = ar-inv.inv-date
                    tt-inv.inv-no     = ar-inv.inv-no
                    tt-inv.type       = IF ar-inv.type GT ' ' THEN ar-inv.type ELSE 'I'
                    tt-inv.amount     = IF v-detail THEN
                           IF ar-inv.net     EQ
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt THEN ar-inv.net ELSE ar-inv.gross
                          ELSE ar-inv.due
                    tt-inv.po-no      = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").                              .

                IF v-detail THEN
                    FOR EACH ar-cashl
                        WHERE ar-cashl.company  EQ ar-inv.company
                        AND ar-cashl.posted   EQ YES
                        AND ar-cashl.cust-no  EQ ar-inv.cust-no
                        AND ar-cashl.inv-no   EQ ar-inv.inv-no
                        USE-INDEX inv-no NO-LOCK,

                        EACH ar-cash
                        WHERE ar-cash.c-no       EQ ar-cashl.c-no
                        AND ar-cash.check-date LE v-stmt-date
                        USE-INDEX c-no NO-LOCK:

                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
                            tt-inv.inv-date    = ar-inv.inv-date
                            tt-inv.old-day     = (TODAY - ar-inv.inv-date)
                            tt-inv.trans-date  = ar-cash.check-date
                            tt-inv.inv-no      = ar-cashl.inv-no
                            tt-inv.description = ar-cashl.dscr
                            tt-inv.po-no       = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

                        IF ar-cashl.memo THEN
                            IF ar-cashl.amt-disc NE 0 THEN
                                ASSIGN
                                    tt-inv.type   = "R"
                                    tt-inv.amount = ar-cashl.amt-disc * -1.

                            ELSE  
                                IF ar-cashl.amt-paid + ar-cashl.amt-disc LE 0 THEN
                                    ASSIGN
                                        tt-inv.type   = "CM"
                                        tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

                                ELSE
                                    ASSIGN
                                        tt-inv.type   = "DM"
                                        tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

                        ELSE
                            ASSIGN
                                tt-inv.type   = "P"
                                tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

                    END.
            END.
            ELSE IF tb_curr-bal THEN 
                DO:
                    IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
                        THEN 
                    DO: 
                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.DESCRIPTION = "No Balance Due"
                            .
                    END.
                END.
        END.

        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.inv-no     EQ 0
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.amt-paid   NE 0
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            CREATE tt-inv.
            ASSIGN
                tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
                tt-inv.inv-date    = ar-cashl.inv-date
                tt-inv.old-day     = (TODAY - ar-cashl.inv-date)
                tt-inv.trans-date  = ar-cash.check-date
                tt-inv.inv-no      = ar-cashl.inv-no
                tt-inv.description = ar-cashl.dscr
                tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

            IF ar-cashl.memo THEN
                ASSIGN
                    tt-inv.amount = ar-cashl.amt-paid
                    tt-inv.type   = IF tt-inv.amount LT 0 THEN "CR" ELSE "DR".

            ELSE
                ASSIGN
                    tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
                    tt-inv.type   = "P".                             
        END.

        /* to get last payment amt, check, date */
        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            /*and ar-cashl.on-account eq YES*/
            AND ar-cashl.amt-paid   NE 0
            AND ar-cashl.memo       EQ NO
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
                ASSIGN v-last-amt     = ar-cashl.amt-paid
                    v-last-ref#    = STRING(ar-cash.check-no)
                    v-last-paydate = ar-cash.check-date.  
            ELSE
                IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
                    ASSIGN v-last-amt = v-last-amt + ar-cashl.amt-paid.

        END.

        v-balance = 0. /* reset running balance */
        v-aged = 0. /* clear aging buckets */
        CLEAR FRAME stmt-header NO-PAUSE.
        CLEAR FRAME stmt-line ALL NO-PAUSE.
        CLEAR FRAME stmt-line-badger ALL NO-PAUSE.
        CLEAR FRAME stmt-total NO-PAUSE.
        ws_addr = ''.
        IF AVAILABLE cust THEN
            ASSIGN
                ws_addr[1] = cust.name
                ws_addr[2] = cust.addr[1]
                ws_addr[3] = cust.addr[2]
                ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
        DO yy = 1 TO 6:
            DO zz = yy + 1 TO 6:
                IF ws_addr[yy] EQ '' AND ws_addr[zz] GT ''
                    THEN
                    ASSIGN ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
            END.
        END.  

        FOR EACH tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
            BREAK BY "1"
            BY tt-inv.cust-no
            BY tt-inv.inv-date
            BY tt-inv.sort-fld
            BY tt-inv.trans-date:

            IF AVAILABLE cust THEN
                FIND FIRST terms WHERE terms.company EQ cocode
                    AND terms.t-code EQ cust.terms NO-LOCK NO-ERROR .
            IF AVAILABLE terms THEN
                ASSIGN terms_dscr = terms.dscr .

            IF FIRST-OF ("1") OR (LINE-COUNTER GT ln-total) THEN 
            DO:
                IF NOT v-first THEN PAGE.
                IF v-stmt-char = "Premier" THEN
                    ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                        v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                        v-remitto[3] = "<C40>          CHICAGO, IL 60686".
                ELSE IF v-stmt-char = "StmtPrint-Mex" THEN
                        ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                            v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                            v-remitto[3] = "<C40>          CHICAGO, IL 60686".
                    ELSE v-remitto = "".

                IF v-stmt-char = "Premier" THEN
                    PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                        "<=1><R+6><C+54><B><P22>Statement</B><P12>" SKIP
                        /*"<=1><R+8>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP*/
                        "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
                        "<=1><R+9><C+68><FROM><C+10><LINE>" 
                        "<=1><R+9><C+52>" v-stmt-date
                        "<=1><R+9><C+68>" cust.cust-no SKIP
                        "<=1><R+11><C1>" ws_addr[1] v-remitto[1] SKIP
                        "<=1><R+12><C1>" ws_addr[2] v-remitto[2] SKIP 
                        "<=1><R+13><C1>" ws_addr[3] v-remitto[3] SKIP
                        "<=1><R+14><C1>" ws_addr[4] v-remitto[4] SKIP
                        /*  "<=1><R+15><C1>" ws_addr[5] skip*/
                        "<=1><R+15><C1>Terms : " terms_dscr SKIP
                        "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
                        "<=1><R+18><FROM><C+80><LINE>"
                        . 

                ELSE IF v-stmt-char = "StmtPrint-Mex" THEN
                        PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                            "<=1><R+6><C+54><B><P22>Declaración</B><P12>" SKIP
                            /*"<=1><R+8>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP*/
                            "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
                            "<=1><R+9><C+68><FROM><C+10><LINE>" 
                            "<=1><R+9><C+52>" v-stmt-date
                            "<=1><R+9><C+68>" cust.cust-no SKIP
                            "<=1><R+11><C1>" ws_addr[1] v-remitto[1] SKIP
                            "<=1><R+12><C1>" ws_addr[2] v-remitto[2] SKIP 
                            "<=1><R+13><C1>" ws_addr[3] v-remitto[3] SKIP
                            "<=1><R+14><C1>" ws_addr[4] v-remitto[4] SKIP
                            /*  "<=1><R+15><C1>" ws_addr[5] skip*/
                            "<=1><R+15><C1>Condiciones : " terms_dscr SKIP
                            "<=1><R+17>Fecha    Código Ref# Descripción   <C54>Cantidad     Equilibrar" SKIP
                            "<=1><R+18><FROM><C+80><LINE>"
                            . 

                    ELSE IF v-stmt-char = "LoyLang" THEN
                            PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                "<=1><R+10><C+52>" v-stmt-date
                                "<=1><R+10><C+68>" cust.cust-no SKIP
                                "<=1><R+11><C1>" ws_addr[1] SKIP
                                "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
                                "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
                                "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                "<=1><R+13><C40><FROM><C+17><LINE>"
                                "<=1><R+13><C58><FROM><C+8><LINE>" 
                                "<=1><R+13><C67><FROM><C+10><LINE>"      SKIP 
                                "<=1><R+13><C40>" v-last-amt 
                                "<=1><R+13><C58>" v-last-paydate
                                "<=1><R+13><C69>" v-last-ref#            SKIP
                                "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                "<=1><R+17>         Trans"               SKIP
                                "<=1><R+18>Date     Type   Ref#     BOL#   Customer PO#    <C56>Amount        Balance" SKIP
                                "<=1><R+19><FROM><C+80><LINE>"
                                . 
                        ELSE IF v-stmt-char = "Printers" THEN
                                PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                    "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                    "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                    "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                    "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                    "<=1><R+10><C+52>" v-stmt-date
                                    "<=1><R+10><C+68>" cust.cust-no SKIP
                                    "<=1><R+11><C1>" ws_addr[1] SKIP
                                    "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
                                    "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
                                    "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                    "<=1><R+13><C40><FROM><C+17><LINE>"
                                    "<=1><R+13><C58><FROM><C+8><LINE>" 
                                    "<=1><R+13><C67><FROM><C+10><LINE>"      SKIP 
                                    "<=1><R+13><C40>" v-last-amt 
                                    "<=1><R+13><C58>" v-last-paydate
                                    "<=1><R+13><C69>" v-last-ref#            SKIP
                                    "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                    "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                    "<=1><R+17>         Trans"               SKIP
                                    "<=1><R+18>Date     Type   Inv#     Job#   Customer PO#    <C56>Amount        Balance" SKIP
                                    "<=1><R+19><FROM><C+80><LINE>"
                                    . 

                            ELSE IF v-stmt-char = "Badger" THEN
                                    PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                        "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                        "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                        "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                        "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                        "<=1><R+10><C+52>" v-stmt-date
                                        "<=1><R+10><C+68>" cust.cust-no SKIP
                                        "<=1><R+11><C1>" ws_addr[1] SKIP
                                        "<=1><R+12><C1>" ws_addr[2] v-remitto[1] SKIP 
                                        "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                        "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                        "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                        "<=1><R+17>Date     Code  Inv# Description          Days <C55>Amount        Balance" SKIP
                                        "<=1><R+18><FROM><C+80><LINE>"
                                        . 

                                ELSE IF v-stmt-char = "RFC" THEN          /* task 12231305 */
                                        PUT "<R2><C1><#1><R+12><C+25><IMAGE#1=" ls-full-img1 SKIP
                                            /*"<=1><R+3><C+26> 2066 S. East Avenue" 
                                            "<=1><R+4><C+26> Vineland, NJ 08360" 
                                            "<=1><R+5><C+26> Phone: 856-692-0404" 
                                            "<=1><R+6><C+26> Fax: (856) 692-2085" */
                                            "<=1><R+10><C+54><B><P22>Statement</B><P12>" SKIP
                                            "<=1><R+12>" "<C53>Statement Date           Account #" SKIP
                                            "<=1><R+13><C+52><FROM><C+13><LINE>" SKIP
                                            "<=1><R+13><C+68><FROM><C+10><LINE>" 
                                            "<=1><R+13><C+52>" v-stmt-date
                                            "<=1><R+13><C+68>" cust.cust-no SKIP
                                            "<=1><R+12><C18>Attn:" cust.contact SKIP
                                            "<=1><R+13><C18>" ws_addr[1] SKIP                        /*Task# 01031416*/
                                            "<=1><R+14><C18>" ws_addr[2] v-remitto[1] SKIP 
                                            "<=1><R+15><C18>" ws_addr[3] v-remitto[2] SKIP
                                            "<=1><R+16><C18>" ws_addr[4] v-remitto[3] SKIP
                                            "<=1><R+17><C18>" ws_addr[5] v-remitto[4] SKIP
                                            "<=1><R+20><C1>Date        <C9>Code      <C16>Ref#     <C25>Description   <C57>Amount       <C71>Balance" SKIP(1)
                                            "<=1><R+21><FROM><C+80><LINE>"
                                            . 

                                    ELSE
                                        PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
                                            "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
                                            "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
                                            "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                                            "<=1><R+10><C+68><FROM><C+10><LINE>" 
                                            "<=1><R+10><C+52>" v-stmt-date
                                            "<=1><R+10><C+68>" cust.cust-no SKIP
                                            "<=1><R+11><C1>" ws_addr[1] SKIP
                                            "<=1><R+12><C1>" ws_addr[2] v-remitto[1] SKIP 
                                            "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
                                            "<=1><R+14><C1>" ws_addr[4] v-remitto[3] SKIP
                                            "<=1><R+15><C1>" ws_addr[5] v-remitto[4] SKIP
                                            "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
                                            "<=1><R+18><FROM><C+80><LINE>"
                                            . 

                v-first = NO.
            END.

            IF tt-inv.description EQ '' THEN 
            DO:
                msgx = LOOKUP(tt-inv.type,v-inv-type-list).
                IF msgx EQ 0 THEN
                    msgx = 1.    /* assume invoice */
                tt-inv.description =
                    IF msgx GT 0 AND msgx LE v-inv-type-max
                    THEN
                    v-inv-type-array[msgx]
                    ELSE
                    ''.
            END.

            v-balance = v-balance + tt-inv.amount.

            IF v-stmt-char = "Badger" THEN 
            DO:
                DISPLAY
                    tt-inv.trans-date
                    tt-inv.type
                    tt-inv.inv-no  
                    WHEN tt-inv.inv-no GT 0
                    tt-inv.description
                    tt-inv.old-day
                    tt-inv.amount
                    v-balance 
                    WITH FRAME stmt-line-badger .
                DOWN 1 WITH FRAME stmt-line-badger.
            END.
            ELSE IF v-stmt-char = "RFC" THEN 
                DO:
                    PUT
                        "<C1>" tt-inv.trans-date
                        "<C9>" tt-inv.type
                        "<C15>" tt-inv.inv-no  /*when tt-inv.inv-no gt 0*/
                        /*tt-inv.description*/
                        "<C25>" tt-inv.description
                        /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description)*/
                        "<C54>" tt-inv.amount
                        "<C68>" v-balance  SKIP.
                /*with frame no-stmt-line.
              down 1 with frame no-stmt-line.*/
                END.
                ELSE 
                DO:
                    DISPLAY
                        tt-inv.trans-date
                        tt-inv.type
                        tt-inv.inv-no  
                        WHEN tt-inv.inv-no GT 0
                        tt-inv.description
                        tt-inv.amount
                        v-balance
                        WITH FRAME no-stmt-line.
                    DOWN 1 WITH FRAME no-stmt-line.
                END.

            v-age = v-stmt-date - tt-inv.inv-date.
            IF v-age = ? OR v-age LT 0 THEN v-age = 0.
            IF v-stmt-char = "Badger" THEN 
                v-per = trunc(v-age / (v-days-in-per + 1), 0) + 1.
            ELSE        
                v-per = trunc(v-age / v-days-in-per, 0) + 1.
            IF v-per GT 5 THEN
                v-per = 5.
            v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

            IF LAST-OF ("1") THEN 
            DO:
                /*adv = ln-total - line-counter.
                put skip(adv).
                */
                PUT SKIP(1).

                IF v-print-hdr THEN
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME stmt-total-line.

                ELSE
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME no-stmt-total-line.
                /*
                      if v-print-hdr then
                      display
                        v-aged[1 for 5]
                        code-legend
                        with frame stmt-total.
                
                      else
                      display
                        v-aged[1 for 5]
                        code-legend
                        with frame no-stmt-total.
                */
                IF v-stmt-char = "Badger" THEN 
                    PUT "<R57><C1><#2>"SKIP
                        "<=2>      Current             31 - 60             61 - 90            >90 Days" SKIP
                        "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                        "<=2><R+2>" v-aged[1] AT 12 v-aged[2] AT 30  v-aged[3] AT 50  (v-aged[4] + v-aged[5]) AT 70
                        SKIP(1).
                ELSE IF v-stmt-char = "StmtPrint-Mex" THEN 
                        PUT "<R57><C1><#2>"SKIP
                            "<=2>      Corriente        30 dias         60 dias         90 dias        >90 dias" SKIP
                            "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                            "<=2><R+2>" v-aged[1 for 5]
                            SKIP(1).
                    ELSE 
                        PUT "<R57><C1><#2>"SKIP
                            "<=2>      Current         30 Days         60 Days         90 Days        >90 Days" SKIP
                            "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                            "<=2><R+2>" v-aged[1 for 5]
                            SKIP(1).

                IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
                    PUT "<R62><C1><#3>"SKIP
                        "<=3><R+1><C1>" code-legend SKIP
                        "<R+1><C+80><RECT#3>" 
                        SKIP. 

            END.
        END.  /* for each tt-inv */

    END. /* for each cust record */

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-protagonstmt C-Win 
PROCEDURE run-protagonstmt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER ipl-email AS LOG NO-UNDO.

    DEFINE VARIABLE v-first   AS LOG NO-UNDO.
    DEFINE VARIABLE v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

{sys/form/r-top.f}

    DEFINE VARIABLE v-stmt-date    AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Statement Date".
    DEFINE VARIABLE v-lo-cust      LIKE cust.cust-no LABEL "From Customer#" NO-UNDO.
    DEFINE VARIABLE v-hi-cust      LIKE cust.cust-no LABEL "Thru Customer#" NO-UNDO.
    DEFINE VARIABLE v-msg          AS CHARACTER NO-UNDO FORMAT 'x(40)' LABEL "Statement Message".
    DEFINE VARIABLE v-detail       AS LOG       FORMAT "yes/no" LABEL "Print Detail?" NO-UNDO.
    DEFINE VARIABLE v-past-due     AS LOG       NO-UNDO FORMAT "yes/no" LABEL "Print Past Due Only?".

    DEFINE VARIABLE xx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE yy             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE zz             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-print-align  AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-align-ok     AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE save_id        AS RECID     NO-UNDO.
    DEFINE VARIABLE v-balance      AS DECIMAL   LABEL "Balance" FORMAT '->>,>>>,>>>.99CR' NO-UNDO.
    DEFINE VARIABLE v-age          AS INTEGER   NO-UNDO. /* number of days old */
    DEFINE VARIABLE v-per          AS INTEGER   NO-UNDO. /* hash of v-age into aging periods */
    DEFINE VARIABLE v-aged         AS DECIMAL   NO-UNDO EXTENT 5 FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE dAged          AS DECIMAL   NO-UNDO EXTENT 5 FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE v-days-in-per  AS INTEGER   NO-UNDO INIT 30.
    DEFINE VARIABLE ln-total       AS INTEGER   NO-UNDO INIT 51.
    DEFINE VARIABLE adv            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ws_letterhead  AS CHARACTER FORMAT 'x(80)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE ws_addr        AS CHARACTER FORMAT 'x(35)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE code-legend    AS CHARACTER FORMAT 'X(80)' NO-UNDO.
    DEFINE VARIABLE v-asi-excel    AS LOG       NO-UNDO.

    DEFINE VARIABLE v-last-amt     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-last-ref#    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-last-paydate AS DATE      NO-UNDO.
    DEFINE VARIABLE cCheckPo       AS CHARACTER NO-UNDO .

    DEFINE VARIABLE ld-due         AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE lv-curr        LIKE currency.c-desc NO-UNDO.
    DEFINE VARIABLE lv-terms       LIKE terms.dscr NO-UNDO.
    DEFINE VARIABLE lc-attn        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckCaseInv  AS LOGICAL   INITIAL NO NO-UNDO .

    DEFINE BUFFER lb-cust FOR cust.
    DEFINE VARIABLE msgx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-type-descr AS CHARACTER FORMAT 'x(30)' NO-UNDO.
    DEFINE VARIABLE v-inv-type-list  AS CHARACTER NO-UNDO INIT "I,CR,DR,P,DA,FC,R".
    DEFINE VARIABLE v-inv-type-max   AS INTEGER   NO-UNDO.
    v-inv-type-max = NUM-ENTRIES(v-inv-type-list).
    DEFINE VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-inv-type-array AS CHARACTER NO-UNDO EXTENT 7 INIT
        ["Invoice",
        "CR Memo",
        "DR Memo",
        "Payment",
        "Disc Allowed",
        "Finance Chg",
        "Return"].

    code-legend = "CODES: ".
    DO xx = 1 TO v-inv-type-max:
        code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
            + v-inv-type-array[xx] + ' '.
    END.

    v-asi-excel = IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN TRUE ELSE FALSE.

    IF v-stmt-char EQ "StdStatement10" OR v-stmt-char EQ "StdStatement2" OR v-stmt-char EQ "ARStmt3C" THEN 
    DO:                                 
        RUN FileSys_GetBusinessFormLogo(cocode, "" /* cust */ , "" /* location */ , OUTPUT cRtnChar, OUTPUT lValid, OUTPUT cMessage).
        IF NOT lValid THEN
        DO:
	        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        END.
    END.

    FORM
        tt-inv.trans-date COLUMN-LABEL "Date"
        tt-inv.inv-no COLUMN-LABEL "Ref#"
        tt-inv.description FORM "x(10)"  COLUMN-LABEL "Desc."
        tt-inv.po-no COLUMN-LABEL "Customer PO"
        tt-inv.inv-amt COLUMN-LABEL "Original!Invoice"
        tt-inv.amount COLUMN-LABEL "Invoice!Balance"
        v-balance COLUMN-LABEL "Balance"
        WITH FRAME stmt-line NO-BOX STREAM-IO WIDTH 90 DOWN NO-LABELS.

    FORM SPACE(1)
        tt-inv.trans-date FORMAT "99/99/99" COLUMN-LABEL "Date"
        tt-inv.inv-no FORMAT ">>>>>>>9" COLUMN-LABEL "Invoice #" SPACE(1)
        tt-inv.description FORMAT "x(14)" COLUMN-LABEL "Description"
        tt-inv.po-no FORMAT "x(12)" COLUMN-LABEL "Cust PO#"
        dAged[1] FORMAT "->>>>>>>9.99" COLUMN-LABEL "0-30"
        dAged[2] FORMAT "->>>>>>>9.99" COLUMN-LABEL "31-60"
        dAged[3] FORMAT "->>>>>>>9.99" COLUMN-LABEL "61-90"
        dAged[4] FORMAT "->>>>>>>9.99" COLUMN-LABEL "90+"
        WITH FRAME lanco-stmt-line NO-BOX STREAM-IO WIDTH 98 DOWN NO-LABELS.

    FORM SPACE(1)
        tt-inv.trans-date FORMAT "99/99/99" COLUMN-LABEL "Date"
        tt-inv.inv-no FORMAT ">>>>>>>9" COLUMN-LABEL "Invoice #" SPACE(1)
        tt-inv.description FORMAT "x(14)" COLUMN-LABEL "Description"
        tt-inv.po-no FORMAT "x(12)" COLUMN-LABEL "Cust PO#"
        dAged[1] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "0-30"
        dAged[2] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "31-60"
        dAged[3] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "61-90"
        dAged[4] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "90+"
        v-age FORMAT "->>>>>" COLUMN-LABEL "Age"
        WITH FRAME lanco-ar-stmt-line NO-BOX STREAM-IO WIDTH 112 DOWN NO-LABELS.

    FORM
        v-msg AT 15
        v-balance  AT 73
        WITH FRAME stmt-total-line NO-BOX NO-LABELS STREAM-IO WIDTH 90.

    FORM 
        tt-inv.trans-date COLUMN-LABEL "Date"
        tt-inv.inv-no COLUMN-LABEL "Ref#"
        tt-inv.description FORM "x(12)"  COLUMN-LABEL "Description"
        tt-inv.po-no COLUMN-LABEL "Customer PO"
        tt-inv.inv-amt COLUMN-LABEL "Original!Invoice"
        tt-inv.amount COLUMN-LABEL "Invoice!Balance"
        v-balance COLUMN-LABEL "Balance"  
        WITH FRAME no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 90 DOWN.

    FORM SPACE(1)
        tt-inv.trans-date FORMAT "99/99/99" COLUMN-LABEL "Date"
        tt-inv.inv-no FORMAT ">>>>>>>9" COLUMN-LABEL "Invoice #" SPACE(1)
        tt-inv.description FORMAT "x(14)" COLUMN-LABEL "Description"
        tt-inv.po-no FORMAT "x(12)" COLUMN-LABEL "Cust PO#"
        dAged[1] FORMAT "->>>>>>>9.99" COLUMN-LABEL "0-30"
        dAged[2] FORMAT "->>>>>>>9.99" COLUMN-LABEL "31-60"
        dAged[3] FORMAT "->>>>>>>9.99" COLUMN-LABEL "61-90"
        dAged[4] FORMAT "->>>>>>>9.99" COLUMN-LABEL "90+"
        WITH FRAME lanco-no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 98 DOWN.

    FORM SPACE(1)
        tt-inv.trans-date FORMAT "99/99/99" COLUMN-LABEL "Date"
        tt-inv.inv-no FORMAT ">>>>>>>9" COLUMN-LABEL "Invoice #" SPACE(1)
        tt-inv.description FORMAT "x(14)" COLUMN-LABEL "Description"
        tt-inv.po-no FORMAT "x(12)" COLUMN-LABEL "Cust PO#"
        dAged[1] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "0-30"
        dAged[2] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "31-60"
        dAged[3] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "61-90"
        dAged[4] FORMAT "$->>>>>>>9.99" COLUMN-LABEL "90+"
        v-age FORMAT "->>>>>" COLUMN-LABEL "Age"
        WITH FRAME lanco-ar-no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 112 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 73
        WITH FRAME no-stmt-total-line NO-BOX NO-LABELS STREAM-IO WIDTH 90.

    FORM
        SKIP(3)
        SKIP
        v-aged[1 for 4] SKIP(1)
        code-legend SKIP
        WITH FRAME no-stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    DEFINE VARIABLE ls-image1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-full-img1 AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE ls-image2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-full-img2 AS cha       FORM "x(200)" NO-UNDO.
    ASSIGN 
        ls-image1 = (IF v-stmt-char = "Protagon" THEN "images\protinv.jpg"
                   ELSE IF v-stmt-char = "SouleMed" THEN "images\Soulemedical.jpg" 
                   ELSE IF v-stmt-char =  "StdStatement10" THEN cRtnChar 
                   ELSE IF v-stmt-char =  "StdStatement2" THEN cRtnChar
                   ELSE IF v-stmt-char =  "ARStmt3C" THEN cRtnChar
                    ELSE "images\Soule.jpg") .                    

    ASSIGN
        FILE-INFO:FILE-NAME = ls-image1
        ls-full-img1        = FILE-INFO:FULL-PATHNAME + ">".

    ASSIGN 
        ls-image2           = "images\protinvfoot.jpg"
        FILE-INFO:FILE-NAME = ls-image2
        ls-full-img2        = FILE-INFO:FULL-PATHNAME + ">".
    /*
    IF v-use-cust THEN
       FIND FIRST cust WHERE
            cust.company EQ cocode AND
            cust.active  EQ "S"
            NO-LOCK NO-ERROR.*/


    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

    IF v-print-hdr AND AVAILABLE company THEN 
    DO:
        yy = 1.

        IF company.name    NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.name
                yy                = yy + 1.

        IF company.addr[1] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[1]
                yy                = yy + 1.

        IF company.addr[2] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[2]
                yy                = yy + 1.

        IF company.city    NE "" OR
            company.state   NE "" OR
            company.zip     NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
                yy                = yy + 1.

        DO xx = 1 TO 6:
            IF ws_letterhead[xx] GT '' THEN
                ws_letterhead[xx] = FILL(" ", int((80 - length(ws_letterhead[xx])) / 2))
                    + ws_letterhead[xx].
        END.
    END.

    ASSIGN
        str-tit2    = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 56}

        v-stmt-date = stmt-date
        v-msg       = stmt-msg
        v-detail    = tb_detailed
        v-past-due  = tb_past-due.

    IF ip-sys-ctrl-shipto OR ipl-email THEN
        ASSIGN
            v-lo-cust = ip-cust-no
            v-hi-cust = ip-cust-no.
    ELSE
        ASSIGN
            v-lo-cust = ""
            v-hi-cust = "".
 
{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF NOT v-asi-excel THEN
        is-xprint-form = YES.
    ELSE
        is-xprint-form = NO.

    IF is-xprint-form THEN 
    DO:
        CASE rd-dest :
            WHEN 1 THEN 
                DO: 
                    IF LOOKUP(v-stmt-char,"StdStatement10,StdStatement2,ARStmt3C") > 0 THEN 
                        PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                    ELSE PUT "<PRINTER?>" FORM "x(30)".
                END.
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN 
                    DO:    
                        IF LOOKUP(v-stmt-char,"StdStatement10,StdStatement2,ARStmt3C") > 0 THEN 
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><MODAL=NO>" FORM "x(120)". 
                        ELSE PUT "<PREVIEW><MODAL=NO>" FORM "x(30)".
                    END.
                    ELSE 
                    DO:
                        IF LOOKUP(v-stmt-char,"StdStatement10,StdStatement2,ARStmt3C") > 0 THEN
                            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORM "x(120)".
                        ELSE PUT "<PREVIEW>" FORM "x(30)".
                    END.
                END.
            WHEN 4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
                    PUT UNFORMATTED 
                        "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
                END.        
            WHEN 5 THEN 
                DO:
                    IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN 
                    DO:
                        IF LOOKUP(v-stmt-char,"StdStatement10,StdStatement2,ARStmt3C") > 0 THEN
                            PUT "<PREVIEW><PDF-LEFT=" + trim(STRING(3 + d-print-fmt-dec)) + "mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".  
                        ELSE PUT "<PREVIEW><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".  
                    END.
                    ELSE 
                    DO:
                        IF LOOKUP(v-stmt-char,"StdStatement10,StdStatement2,ARStmt3C") > 0 THEN
                            PUT "<PREVIEW=PDF><PDF-LEFT=" + trim(STRING(3 + d-print-fmt-dec)) + "mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                        ELSE PUT "<PREVIEW=PDF><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                    END.
                END.
        END CASE.
        PUT "</PROGRESS>".
    END.


    SESSION:SET-WAIT-STATE ("general").
    v-first = YES.

    EMPTY TEMP-TABLE tt-inv.
    EMPTY TEMP-TABLE tt-cust-excel.

    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ ttCustList.cust-no
        AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")             
        AND (cust.cust-no EQ v-hi-cust OR v-hi-cust = "")            
        AND ((cust.acc-bal NE 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        BREAK BY cust.cust-no
        TRANSACTION:

        IF NOT v-asi-excel THEN
            EMPTY TEMP-TABLE tt-inv.

        ASSIGN 
            v-last-amt     = 0
            v-last-ref#    = ""
            v-last-paydate = ?.

        IF v-past-due THEN
        DO:
            FIND FIRST ar-inv WHERE ar-inv.company EQ cust.company    AND
                ar-inv.cust-no EQ cust.cust-no AND
                ar-inv.posted                AND
                ar-inv.due NE 0              AND
                ar-inv.inv-date LE v-stmt-date AND
                ar-inv.due-date LE v-stmt-date NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ar-inv THEN NEXT.
        END.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cust.company
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.posted   EQ YES
            /*and ar-inv.due      ne 0*/
            AND ar-inv.terms    NE "CASH"
            AND ar-inv.inv-date LE v-stmt-date
            NO-LOCK:

            FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                ld-due = ar-inv.net.
            ELSE
                ld-due = ar-inv.gross.
            ASSIGN 
                lCheckCaseInv = NO .
            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-stmt-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 THEN
                        ld-due = ld-due - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                            ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
                ASSIGN 
                    lCheckCaseInv = YES .
            END.

            IF ld-due NE 0 THEN 
            DO:
                FIND FIRST tt-inv 
                    WHERE tt-inv.cust-no EQ cust.cust-no
                    AND tt-inv.DESCRIPTION EQ "No Balance Due"
                    NO-ERROR.
                IF AVAILABLE tt-inv THEN DELETE tt-inv.
                CREATE tt-inv.
                ASSIGN
                    tt-inv.cust-no    = cust.cust-no
                    tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
                    tt-inv.inv-date   = ar-inv.inv-date
                    tt-inv.trans-date = ar-inv.inv-date
                    tt-inv.inv-no     = ar-inv.inv-no
                    tt-inv.type       = IF ar-inv.type GT ' ' THEN ar-inv.type ELSE 'I'
                    tt-inv.inv-amt    = ar-inv.gross
                    tt-inv.amount     = IF v-detail OR  NOT lCheckCaseInv THEN
                           IF ar-inv.net     EQ
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt THEN ar-inv.net ELSE ar-inv.gross
                          ELSE ar-inv.due
                    tt-inv.po-no      = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
                    tt-inv.bol-no     = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "")
                    tt-inv.charge     = ar-inv.paid + ar-inv.due 
                    tt-inv.credits    = ar-inv.paid                              .

                IF v-stmt-char = "Printers" THEN
                    tt-inv.bol-no = (IF AVAILABLE ar-invl AND ar-invl.job-no NE "" 
                        THEN "  " + ar-invl.job-no
                        ELSE IF AVAILABLE ar-invl AND ar-invl.ord-no <> 0 
                        THEN STRING(ar-invl.ord-no,">>>>>>>>") 
                        ELSE "").
                IF v-detail THEN
                    FOR EACH ar-cashl
                        WHERE ar-cashl.company  EQ ar-inv.company
                        AND ar-cashl.posted   EQ YES
                        AND ar-cashl.cust-no  EQ ar-inv.cust-no
                        AND ar-cashl.inv-no   EQ ar-inv.inv-no
                        USE-INDEX inv-no NO-LOCK,

                        EACH ar-cash
                        WHERE ar-cash.c-no       EQ ar-cashl.c-no
                        AND ar-cash.check-date LE v-stmt-date
                        USE-INDEX c-no NO-LOCK:

                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
                            tt-inv.inv-date    = ar-inv.inv-date
                            tt-inv.trans-date  = ar-cash.check-date
                            tt-inv.inv-no      = ar-cashl.inv-no
                            tt-inv.inv-amt     = ar-inv.gross
                            tt-inv.description = ar-cashl.dscr
                            tt-inv.po-no       = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
                            tt-inv.bol-no      = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "")
                            tt-inv.check-no    = STRING(ar-cash.check-no)
                            tt-inv.charge      = ar-inv.paid + ar-inv.due 
                            tt-inv.credits     = ar-inv.paid .

                        IF v-stmt-char = "Printers" THEN
                            tt-inv.bol-no = (IF AVAILABLE ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no 
                                ELSE IF AVAILABLE ar-invl AND ar-invl.ord-no <> 0 
                                THEN STRING(ar-invl.ord-no,">>>>>>>>") 
                                ELSE "").

                        IF ar-cashl.memo THEN
                            IF ar-cashl.amt-disc NE 0 THEN
                                ASSIGN
                                    tt-inv.type   = "R"
                                    tt-inv.amount = ar-cashl.amt-disc * -1.

                            ELSE  
                                IF ar-cashl.amt-paid + ar-cashl.amt-disc LE 0 THEN
                                    ASSIGN
                                        tt-inv.type   = "CM"
                                        tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

                                ELSE
                                    ASSIGN
                                        tt-inv.type   = "DM"
                                        tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

                        ELSE
                            ASSIGN
                                tt-inv.type   = "P"
                                tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

                    END.
            END.
            ELSE IF tb_curr-bal THEN 
                DO:
                    IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
                        THEN 
                    DO: 
                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.DESCRIPTION = "No Balance Due"
                            .
                    END.
                END.
        END.

        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.inv-no     EQ 0
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.amt-paid   NE 0
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no     = cust.cust-no
                tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
                tt-inv.inv-date    = ar-cashl.inv-date
                tt-inv.trans-date  = ar-cash.check-date
                tt-inv.inv-no      = ar-cashl.inv-no
                tt-inv.description = ar-cashl.dscr
                tt-inv.po-no       = (IF AVAILABLE ar-invl THEN ar-invl.po-no ELSE "")
                tt-inv.bol-no      = (IF AVAILABLE ar-invl AND ar-invl.bol-no <> 0 THEN STRING(ar-invl.bol-no,">>>>>>>>") ELSE "")
                tt-inv.check-no    = STRING(ar-cash.check-no) 
                tt-inv.charge      = ar-cashl.amt-paid + ar-cashl.amt-due 
                tt-inv.credits     = ar-cashl.amt-paid .

            IF ar-cashl.memo THEN
                ASSIGN
                    tt-inv.amount = ar-cashl.amt-paid
                    tt-inv.type   = IF tt-inv.amount LT 0 THEN "CR" ELSE "DR".

            ELSE
                ASSIGN
                    tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
                    tt-inv.type   = "P".
      
        END.                                                

        /* to get last payment amt, check, date */
        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            /*and ar-cashl.on-account eq YES*/
            AND ar-cashl.amt-paid   NE 0
            AND ar-cashl.memo       EQ NO
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
                ASSIGN v-last-amt     = ar-cashl.amt-paid
                    v-last-ref#    = STRING(ar-cash.check-no)
                    v-last-paydate = ar-cash.check-date.  
            ELSE
                IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
                    ASSIGN v-last-amt = v-last-amt + ar-cashl.amt-paid.
        END.

        ASSIGN
            v-balance = 0 /* reset running balance */
            v-aged    = 0. /* clear aging buckets */
        CLEAR FRAME stmt-header NO-PAUSE.
        CLEAR FRAME stmt-line ALL NO-PAUSE.
        CLEAR FRAME stmt-total NO-PAUSE.
        ws_addr = ''.
        IF AVAILABLE cust THEN
            ASSIGN
                ws_addr[1] = cust.name
                ws_addr[2] = cust.addr[1]
                ws_addr[3] = cust.addr[2]
                ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
        FIND FIRST currency WHERE currency.company = cust.company 
            AND currency.c-code = cust.curr-code NO-LOCK NO-ERROR.
        IF AVAILABLE currency THEN
            lv-curr = currency.c-desc.
        FIND FIRST terms WHERE terms.company = cust.company 
            AND terms.t-code = cust.terms NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
            lv-terms = terms.dscr.
        DO yy = 1 TO 6:
            DO zz = yy + 1 TO 6:
                IF ws_addr[yy] EQ '' AND ws_addr[zz] GT ''
                    THEN
                    ASSIGN ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
            END.
        END.

        FOR EACH tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
            BREAK BY "1"
            BY tt-inv.cust-no
            BY tt-inv.inv-date
            BY tt-inv.sort-fld
            BY tt-inv.trans-date:

            IF v-asi-excel AND tt-inv.cust-no NE cust.cust-no THEN
                NEXT.
            IF fi_contact NE "" AND begin_cust-no = end_cust-no THEN 
            DO:
                FIND FIRST lb-cust WHERE lb-cust.company = cocode 
                    AND lb-cust.cust-no = begin_cust-no.
                IF AVAILABLE lb-cust THEN lb-cust.contact = fi_contact.
                RELEASE lb-cust.
                lc-attn = fi_contact.

            END.
            ELSE
                lc-attn = cust.contact.
            IF NOT v-asi-excel AND (FIRST-OF ("1") OR (LINE-COUNTER GT ln-total)) THEN 
            DO:
                IF NOT v-first THEN PAGE.
                v-remitto = "".
                IF v-stmt-char = "Protagon" THEN
                    PUT "<C1><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
                        "<=1><R+7><C+54><B><P22>Statement</B><P11>" SKIP
                        "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
                        "<=1><R+10><C+68><FROM><C+10><LINE>" 
                        "<=1><R+9><C53>Statement Date   Customer #" SKIP
                        "<=1><R+10><C+52>" formatDate(v-stmt-date) FORMAT "X(20)"
                        "<=1><R+10><C+68>" cust.cust-no SKIP
                        "<=1><R+10><C1>" ws_addr[1] SKIP
                        "<=1><R+11><C1>" ws_addr[2] v-remitto[1] "<C62>Terms" SKIP 
                        "<=1><R+12><C+52><FROM><C+26><LINE>"
                        "<=1><R+12><C1>" ws_addr[3] v-remitto[2] "<C53>" lv-terms SKIP
                        "<=1><R+13><C1>" ws_addr[4] v-remitto[3] "<C62>Funds" SKIP
                        "<=1><R+14><C+52><FROM><C+26><LINE>"
                        "<=1><R+14><C1>" ws_addr[5] v-remitto[4] "<C53>" lv-curr SKIP
                        "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
                        "<=1><R+16>                                                Original<C60>Invoice" SKIP
                        "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
                        "<=1><R+18><FROM><C+80><LINE>"
                        . 
                IF v-stmt-char = "Soule" OR v-stmt-char = "SouleMed"  THEN
                    PUT "<C1><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
                        "<=1><R+4><C+52><B><P22>Statement</B><P11>" SKIP
                        "<=1><R+7><C+50><FROM><C+31><LINE>" SKIP
                        "<=1><R+6><C51><b>Statement Date:</b> " formatDate(v-stmt-date) FORMAT "X(20)" SKIP
                        "<=1><R+7.5><C+50><b>Customer #:</b>" cust.cust-no SKIP
                        "<=1><R+8.5><C+50><FROM><C+31><LINE>"
                        "<=1><R+9><C+50><b>Terms:</b> " lv-terms SKIP
                        "<=1><R+10><C+50><FROM><C+33><LINE>"
                        "<=1><R+10><C1>" ws_addr[1] SKIP
                        "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  SKIP 
                        "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  SKIP
                        "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  SKIP
                        "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  SKIP
                        "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
                        "<=1><R+16>                                                Original<C60>Invoice" SKIP
                        "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
                        "<=1><R+18><FROM><C+80><LINE>"
                        .
       
                IF v-stmt-char = "StdStatement10" THEN 
                DO:
                    PUT "<C2><R2><#1><R+8><C+47><IMAGE#1=" ls-full-img1 SKIP
                        "<P11><R4><C50><#3><FROM><R7><C80><RECT><||3>" SKIP
                        "<R5><C50><FROM><R5><C80><LINE><||3>" SKIP
                        "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
                        "<R4><C65><FROM><R5><C65><LINE><||3>" SKIP
                        "<R5><C65><FROM><R6><C65><LINE><||3>" SKIP
                        "<R6><C65><FROM><R7><C65><LINE><||3>" SKIP.
                    PUT "<P22><=#3><C50><R-2> <B><P22>Statement</B> <P11>" " <B> PAGE: </B>" STRING(PAGE-NUMBER,">>9") SKIP
                        "<=#3><R+0>  Customer ID      " cust.cust-no
                        "<=#3><R+1>  Terms            " lv-terms
                        "<=#3><R+2>  Statement Date   " v-stmt-date . 
          
                    PUT "<=1><R+10><C1>" ws_addr[1] SKIP
                        "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  SKIP 
                        "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  SKIP
                        "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  SKIP
                        "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  SKIP
                        "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
                        "<=1><R+16>                                                Original<C60>Invoice" SKIP
                        "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
                        "<=1><R+18><FROM><C+80><LINE>"
                        .
                END.

                IF v-stmt-char = "StdStatement2" THEN 
                DO: 
                    PUT "<C2><R2><#1><R+8><C+47><IMAGE#1=" ls-full-img1 SKIP
                        "<P11><R4><C50><#3><FROM><R7><C80><RECT><||3>" SKIP
                        "<R5><C50><FROM><R5><C80><LINE><||3>" SKIP
                        "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
                        "<R4><C65><FROM><R5><C65><LINE><||3>" SKIP
                        "<R5><C65><FROM><R6><C65><LINE><||3>" SKIP
                        "<R6><C65><FROM><R7><C65><LINE><||3>" SKIP.
                    PUT "<P22><=#3><C50><R-2> <B><P22>Statement</B> <P11>" " <B> PAGE: </B>" STRING(PAGE-NUMBER,">>9") SKIP
                        "<=#3><R+0>  Customer ID      " cust.cust-no
                        "<=#3><R+1>  Terms            " lv-terms
                        "<=#3><R+2>  Statement Date   " v-stmt-date . 
          
                    PUT "<=1><R+10><C1>" ws_addr[1] SKIP
                        "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  SKIP 
                        "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  SKIP
                        "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  SKIP
                        "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  SKIP
                        /*"<=1><R+15>Attn: " lc-attn FORMAT "x(30)" SKIP*/
                        /*"<=1><R+15>                                                Original<C60>Invoice" SKIP*/
                        "<=1><R+15><C2>Date <c8.5>Invoice <C16>Description <C28>Cust PO# <c46>0-30 <C56>31-60 <C67>61-90  <C78> 90+" SKIP
                        "<=1><R+16><FROM><C+80><LINE><P10>"
                        .
                END.

                IF v-stmt-char = "ARStmt3C" THEN 
                DO: 
                    PUT "<C2><R2><#1><R+8><C+47><IMAGE#1=" ls-full-img1 SKIP
                        "<P11><R4><C50><#3><FROM><R7><C80><RECT><||3>" SKIP
                        "<R5><C50><FROM><R5><C80><LINE><||3>" SKIP
                        "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
                        "<R4><C65><FROM><R5><C65><LINE><||3>" SKIP
                        "<R5><C65><FROM><R6><C65><LINE><||3>" SKIP
                        "<R6><C65><FROM><R7><C65><LINE><||3>" SKIP.
                    PUT "<P22><=#3><C50><R-2> <B><P22>Statement</B> <P11>" " <B> PAGE: </B>" STRING(PAGE-NUMBER,">>9") SKIP
                        "<=#3><R+0>  Customer ID      " cust.cust-no
                        "<=#3><R+1>  Terms            " lv-terms
                        "<=#3><R+2>  Statement Date   " v-stmt-date . 
          
                    PUT "<=1><R+10><C1>" ws_addr[1] SKIP
                        "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  SKIP 
                        "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  SKIP
                        "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  SKIP
                        "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  SKIP
                        /*"<=1><R+15>Attn: " lc-attn FORMAT "x(30)" SKIP*/
                        /*"<=1><R+15>                                                Original<C60>Invoice" SKIP*/
                        "<=1><R+15><P10><C2>Date <c8>Invoice <C15>Description <C26>Cust PO# <c42>0-30 <C51.5>31-60 <C62>61-90  <C73.5> 90+  <C78>  Age" SKIP
                        "<=1><R+16><FROM><C+80><LINE><P9>"
                        .
                END.

                v-first = NO.
            END.

            IF tt-inv.description EQ '' THEN 
            DO:
                msgx = LOOKUP(tt-inv.type,v-inv-type-list).
                IF msgx EQ 0 THEN
                    msgx = 1.    /* assume invoice */
                tt-inv.description =
                    IF msgx GT 0 AND msgx LE v-inv-type-max
                    THEN
                    v-inv-type-array[msgx]
                    ELSE
                    ''.
            END.

            v-balance = v-balance + tt-inv.amount.

            IF NOT v-asi-excel THEN
            DO:
                IF v-stmt-char = "StdStatement2" THEN 
                DO:

                    dAged = 0 .
                    v-age = v-stmt-date - tt-inv.inv-date.
                    IF v-age = ? OR v-age LT 0 THEN v-age = 0.
                    v-per = trunc(v-age / v-days-in-per, 0) + 1.
                    IF v-per GT 4 THEN
                        v-per = 4.
                    dAged[v-per] = /*v-aged[v-per] +*/ tt-inv.amount.

                       
                    IF tt-inv.check-no NE "" THEN
                        cCheckPo = tt-inv.check-no.
                    ELSE cCheckPo = tt-inv.po-no .
                    IF v-print-hdr THEN 
                    DO:  
                        DISPLAY
                            tt-inv.trans-date
                            tt-inv.inv-no  
                            WHEN tt-inv.inv-no GT 0
                            tt-inv.DESCRIPTION FORMAT "x(14)"
                            tt-inv.po-no FORMAT "x(12)"
                            dAged[1]
                            dAged[2]
                            dAged[3]
                            dAged[4]
                            WITH FRAME lanco-stmt-line .
                        DOWN 1 WITH FRAME lanco-stmt-line.
                    END.
                    ELSE 
                    DO:
                        DISPLAY
                            tt-inv.trans-date
                            tt-inv.inv-no  
                            WHEN tt-inv.inv-no GT 0
                            tt-inv.DESCRIPTION FORMAT "x(14)"
                            tt-inv.po-no FORMAT "x(12)"
                            dAged[1]
                            dAged[2]
                            dAged[3]
                            dAged[4]
                            WITH FRAME lanco-no-stmt-line.
                        DOWN 1 WITH FRAME lanco-no-stmt-line.
                    END.

                END.  /* StdStatement2 */

                ELSE IF v-stmt-char = "ARStmt3C" THEN 
                    DO:

                        dAged = 0 .
                        v-age = v-stmt-date - tt-inv.inv-date.
                        IF v-age = ? OR v-age LT 0 THEN v-age = 0.
                        v-per = trunc(v-age / v-days-in-per, 0) + 1.
                        IF v-per GT 4 THEN
                            v-per = 4.
                        dAged[v-per] = /*v-aged[v-per] +*/ tt-inv.amount.

                       
                        IF tt-inv.check-no NE "" THEN
                            cCheckPo = tt-inv.check-no.
                        ELSE cCheckPo = tt-inv.po-no .
                        IF v-print-hdr THEN 
                        DO:  
                            DISPLAY
                                tt-inv.trans-date
                                tt-inv.inv-no  
                                WHEN tt-inv.inv-no GT 0
                                tt-inv.DESCRIPTION FORMAT "x(14)"
                                tt-inv.po-no FORMAT "x(12)"
                                dAged[1]
                                dAged[2]
                                dAged[3]
                                dAged[4]
                                v-age
                                WITH FRAME lanco-ar-stmt-line .
                            DOWN 1 WITH FRAME lanco-ar-stmt-line.
                        END.
                        ELSE 
                        DO:
                            DISPLAY
                                tt-inv.trans-date
                                tt-inv.inv-no  
                                WHEN tt-inv.inv-no GT 0
                                tt-inv.DESCRIPTION FORMAT "x(14)"
                                tt-inv.po-no FORMAT "x(12)"
                                dAged[1]
                                dAged[2]
                                dAged[3]
                                dAged[4]
                                v-age
                                WITH FRAME lanco-ar-no-stmt-line.
                            DOWN 1 WITH FRAME lanco-ar-no-stmt-line.
                        END.

                    END.  /* ARStmt3C */

                    ELSE 
                    DO:
                        IF v-print-hdr THEN 
                        DO:
                            DISPLAY
                                tt-inv.trans-date
                                tt-inv.inv-no  
                                WHEN tt-inv.inv-no GT 0  
                                tt-inv.DESCRIPTION
                                tt-inv.po-no
                                tt-inv.inv-amt
                                tt-inv.amount
                                v-balance
                                WITH FRAME stmt-line .
                            DOWN 1 WITH FRAME stmt-line.
                        END.
                        ELSE 
                        DO:
                            DISPLAY
                                tt-inv.trans-date
                                tt-inv.inv-no  
                                WHEN tt-inv.inv-no GT 0
                                tt-inv.DESCRIPTION
                                tt-inv.po-no
                                tt-inv.inv-amt
                                tt-inv.amount
                                v-balance
                                WITH FRAME no-stmt-line.
                            DOWN 1 WITH FRAME no-stmt-line.
                        END.
                    END.
            END.

            v-age = v-stmt-date - tt-inv.inv-date.
            IF v-age = ? OR v-age LT 0 THEN v-age = 0.
            v-per = trunc(v-age / v-days-in-per, 0) + 1. 
            IF v-per GT 4 THEN v-per = 4.

            v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

            IF LAST-OF ("1") THEN 
            DO:

                IF NOT v-asi-excel THEN
                DO:
                    PUT SKIP(1).
                    IF v-stmt-char = "StdStatement2" THEN 
                    DO:
                        PUT "<R-1><C2><FROM><C+80><LINE>" SKIP
                            "<R-1><C26>"
                            "<C40>" v-aged[1] FORMAT "->>>>>>9.99"
                            "<C51>" v-aged[2] FORMAT "->>>>>>9.99"
                            "<C62>" v-aged[3] FORMAT "->>>>>>9.99"
                            "<C73>" v-aged[4] FORMAT "->>>>>>9.99" .

                        PUT SKIP(2) "<C61>Total Balance:" (v-aged[1] + v-aged[2] + v-aged[3] + v-aged[4] ) FORMAT "->>>>>>>9.99" . 
            
                        PUT "<C14><R59.5><#3><R+4><C+7> <b> THANK YOU - YOUR BUSINESS IS APPRECIATED </b>"  SKIP.
              
                    END.
                    ELSE  IF v-stmt-char = "ARStmt3C" THEN 
                        DO:
                            PUT "<R-1><C2><FROM><C+80><LINE>" SKIP
                                "<R-1><C26>"
                                "<C36.3>" v-aged[1] FORMAT "$->>>>>>9.99"
                                "<C46.8>" v-aged[2] FORMAT "$->>>>>>9.99"
                                "<C57.2>" v-aged[3] FORMAT "$->>>>>>9.99"
                                "<C67.8>" v-aged[4] FORMAT "$->>>>>>9.99" .

                            PUT SKIP(2) "<C61>Total Balance:" (v-aged[1] + v-aged[2] + v-aged[3] + v-aged[4] ) FORMAT "$->>>>>>>9.99" . 
            
                            PUT "<C14><R59.5><#3><R+4><C+7> <b> THANK YOU - YOUR BUSINESS IS APPRECIATED </b>"  SKIP.
              
                        END.
                        ELSE 
                        DO:

                            IF v-print-hdr THEN
                                DISPLAY
                                    v-msg
                                    v-balance
                                    WITH FRAME stmt-total-line.

                            ELSE
                                DISPLAY
                                    v-msg
                                    v-balance
                                    WITH FRAME no-stmt-total-line.

                            PUT "<R56><C1><#2>"SKIP
                                "<=2><C13>    0-30 Days     31-60 Days     61-90 Days       >90 Days" SKIP
                                "<=2><R+1.3><FROM><C+80><LINE>" SKIP
                                "<=2><R+2><C13>" v-aged[1 for 4]
                                SKIP.
                            IF v-stmt-char = "Protagon" THEN
                                PUT "<C14><R59.5><#3><R+6><C+53><IMAGE#3=" ls-full-img2 SKIP.

                            IF v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "SouleMed" THEN
                                PUT "<C14><R59.5><#3><R+4><C+7> <b> THANK YOU - YOUR BUSINESS IS APPRECIATED </b>"  SKIP.

                            IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
                                PUT "<R62><C1><#3>"SKIP
                                    "<=3><R+1><C1>" code-legend SKIP
                                    "<R+1><C+80><RECT#3>" 
                                    SKIP. 
                        END.
                END.
                ELSE
                DO:
                    CREATE tt-cust-excel.

                    ASSIGN 
                        tt-cust-excel.cust-no = tt-inv.cust-no
                        tt-cust-excel.contact = cust.contact
                        tt-cust-excel.addr[1] = ws_addr[1]
                        tt-cust-excel.addr[2] = ws_addr[2]
                        tt-cust-excel.addr[3] = ws_addr[3]
                        tt-cust-excel.addr[4] = ws_addr[4]
                        tt-cust-excel.addr[5] = ws_addr[5]
                        tt-cust-excel.aged[1] = v-aged[1]
                        tt-cust-excel.aged[2] = v-aged[2]
                        tt-cust-excel.aged[3] = v-aged[3]
                        tt-cust-excel.aged[4] = v-aged[4]
                        tt-cust-excel.aged[5] = v-aged[5].
                    RELEASE tt-cust-excel.
                END.
            END.
        END.  /* for each tt-inv */

    END. /* for each cust record */

    IF v-asi-excel THEN 
    DO:
        IF v-stmt-char EQ "ASIExcel" THEN
            RUN arrep\asiexlstmt.p(INPUT v-stmt-date, INPUT v-msg).
        ELSE IF v-stmt-char EQ "SouleExcel" THEN
                RUN arrep\SouleExlStmt.p(INPUT v-stmt-date, INPUT v-msg).
    END.   

    IF NOT ipl-email THEN SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ ar/rep/stmt.p 07/95 CAH   */
    /* A/R Statment Print Program - A/R Module                                    */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    IF LOOKUP(v-stmt-char,"ASIXprnt,ASIXprnt-CAN,stmtprint 1,stmtprint 2,RFC,Premier,StmtPrint-Mex,ASIExcel,SouleExcel,Loylang,Printers,Badger") > 0 THEN 
    DO:
        RUN run-asistmt(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
        RETURN.
    END.

    IF LOOKUP(v-stmt-char,"Protagon,Soule,StdStatement10,StdStatement2,ARStmt3C,SouleMed") > 0 THEN 
    DO:
        RUN run-protagonstmt (ip-cust-no, ip-sys-ctrl-shipto, NO).
        RETURN.
    END.

{sys/form/r-top.f}

    DEFINE VARIABLE v-stmt-date      AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Statement Date".
    DEFINE VARIABLE v-lo-cust        LIKE cust.cust-no LABEL "From Customer#" NO-UNDO.
    DEFINE VARIABLE v-hi-cust        LIKE cust.cust-no LABEL "Thru Customer#" NO-UNDO.
    DEFINE VARIABLE v-msg            AS CHARACTER NO-UNDO FORMAT 'x(40)' LABEL "Statement Message".
    DEFINE VARIABLE v-detail         AS LOG       FORMAT "yes/no" LABEL "Print Detail?" NO-UNDO.
    DEFINE VARIABLE v-past-due       AS LOG       NO-UNDO FORMAT "yes/no" LABEL "Print Past Due Only?".

    DEFINE VARIABLE xx               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE yy               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE zz               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-print-align    AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-align-ok       AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE save_id          AS RECID     NO-UNDO.
    DEFINE VARIABLE v-balance        AS DECIMAL   LABEL "Balance" FORMAT '->>,>>>,>>>.99CR'.
    DEFINE VARIABLE v-age            AS INTEGER   NO-UNDO. /* number of days old */
    DEFINE VARIABLE v-per            AS INTEGER   NO-UNDO. /* hash of v-age into aging periods */
    DEFINE VARIABLE v-aged           AS DECIMAL   NO-UNDO EXTENT 5
        FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE v-days-in-per    AS INTEGER   NO-UNDO INIT 30.
    DEFINE VARIABLE ln-total         AS INTEGER   NO-UNDO INIT 51.
    DEFINE VARIABLE adv              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ws_letterhead    AS CHARACTER FORMAT 'x(80)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE ws_addr          AS CHARACTER FORMAT 'x(35)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE code-legend      AS CHARACTER FORMAT 'X(80)' NO-UNDO.

    DEFINE VARIABLE ld-due           AS DECIMAL   NO-UNDO.

    /* 07.11.95 by CAH @ASI:
    1.  There is no ar transaction type file in system, so the following
    vars have been added to support structured definition via lookups.
    */
    DEFINE VARIABLE msgx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-type-descr AS CHARACTER FORMAT 'x(30)' NO-UNDO.
    DEFINE VARIABLE v-inv-type-list  AS CHARACTER NO-UNDO INIT "I,CR,DR,P,DA,FC,R".
    DEFINE VARIABLE v-inv-type-max   AS INTEGER   NO-UNDO.
    v-inv-type-max = NUM-ENTRIES(v-inv-type-list).
    DEFINE VARIABLE v-inv-type-array AS CHARACTER NO-UNDO EXTENT 7 INIT
        ["Invoice",
        "CR Memo",
        "DR Memo",
        "Payment",
        "Disc Allowed",
        "Finance Chg",
        "Return"].

    code-legend = "CODES: ".
    DO xx = 1 TO v-inv-type-max:
        code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
            + v-inv-type-array[xx] + ' '.
    END.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*was 3*/
        "Statement date" AT 50   "Account#" AT 65 SKIP
        ws_addr[2]    AT 11
        "--------------" AT 50   "--------" AT 65 SKIP
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(1)
        "=============================== S T A T E M E N T ============================"
        SKIP
        WITH FRAME stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date COLUMN-LABEL "Date"
        tt-inv.type     COLUMN-LABEL "Code"
        tt-inv.inv-no  COLUMN-LABEL "Ref no"
        tt-inv.description COLUMN-LABEL "Description"
        tt-inv.amount      COLUMN-LABEL "Amount"
        v-balance       COLUMN-LABEL "Balance"
        WITH FRAME stmt-line NO-BOX STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(2)
        "      Current         30 Days         60 Days         90 Days        >90 Days"
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*3*/
        ws_addr[2]    AT 11
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(4)
        SKIP
        WITH FRAME no-stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date
        tt-inv.type
        tt-inv.inv-no
        tt-inv.description
        tt-inv.amount
        v-balance
        WITH FRAME no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME no-stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(3)
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME no-stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    /*if v-use-cust then
       find first cust WHERE
            cust.company eq cocode AND
            cust.active  eq "S"
            no-lock no-error.*/

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

    IF v-print-hdr AND AVAILABLE company THEN 
    DO:
        yy = 1.

        IF company.name    NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.name
                yy                = yy + 1.

        IF company.addr[1] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[1]
                yy                = yy + 1.

        IF company.addr[2] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[2]
                yy                = yy + 1.

        IF company.city    NE "" OR
            company.state   NE "" OR
            company.zip     NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
                yy                = yy + 1.

        DO xx = 1 TO 6:
            IF ws_letterhead[xx] GT '' THEN
                ws_letterhead[xx] = FILL(" ", int((80 - length(ws_letterhead[xx])) / 2))
                    + ws_letterhead[xx].
        END.
    END.

    ASSIGN
        str-tit2    = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 56}

        v-stmt-date = stmt-date
        v-msg       = stmt-msg
        v-detail    = tb_detailed
        v-past-due  = tb_past-due.

    IF lAsiUser THEN 
    DO:
        ASSIGN 
            v-stmt-char   = run_format
            vcDefaultForm = v-stmt-char.
     
    /* viDefaultLinesPerPage = lines-per-page.*/
    END.

    IF ip-sys-ctrl-shipto THEN
        ASSIGN
            v-lo-cust = ip-cust-no
            v-hi-cust = ip-cust-no.
    ELSE
        ASSIGN
            v-lo-cust = ""
            v-hi-cust = "".

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    PAGE.

    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ ttCustList.cust-no
        AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
        AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
        AND ((cust.acc-bal NE 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        TRANSACTION:

        FOR EACH tt-inv:
            DELETE tt-inv.
        END.    /* clear workfile */

        IF v-past-due THEN
        DO:
            FIND FIRST ar-inv WHERE ar-inv.company EQ cust.company    AND
                ar-inv.cust-no EQ cust.cust-no AND
                ar-inv.posted                AND
                ar-inv.due NE 0              AND
                ar-inv.inv-date LE v-stmt-date AND
                ar-inv.due-date LE v-stmt-date NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ar-inv THEN NEXT.
        END.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cust.company
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.posted   EQ YES
            AND ar-inv.terms    NE "CASH"
            AND ar-inv.inv-date LE v-stmt-date
            NO-LOCK:

            FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                ld-due = ar-inv.net.
            ELSE
                ld-due = ar-inv.gross.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-stmt-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 THEN
                        ld-due = ld-due - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                            ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END.

            IF ld-due NE 0 THEN 
            DO:
                FIND FIRST tt-inv 
                    WHERE tt-inv.cust-no EQ cust.cust-no
                    AND tt-inv.DESCRIPTION EQ "No Balance Due"
                    NO-ERROR.
                IF AVAILABLE tt-inv THEN DELETE tt-inv.
                CREATE tt-inv.
                ASSIGN
                    tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
                    tt-inv.inv-date   = ar-inv.inv-date
                    tt-inv.trans-date = ar-inv.inv-date
                    tt-inv.inv-no     = ar-inv.inv-no
                    tt-inv.type       = IF ar-inv.type GT ' ' THEN ar-inv.type ELSE 'I'
                    tt-inv.amount     = IF v-detail THEN
                           IF ar-inv.net     EQ
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt THEN ar-inv.net ELSE ar-inv.gross
                          ELSE ar-inv.due
                    tt-inv.po-no      = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").                              

                IF v-detail THEN
                    FOR EACH ar-cashl
                        WHERE ar-cashl.company  EQ ar-inv.company
                        AND ar-cashl.posted   EQ YES
                        AND ar-cashl.cust-no  EQ ar-inv.cust-no
                        AND ar-cashl.inv-no   EQ ar-inv.inv-no
                        USE-INDEX inv-no NO-LOCK,

                        EACH ar-cash
                        WHERE ar-cash.c-no       EQ ar-cashl.c-no
                        AND ar-cash.check-date LE v-stmt-date
                        USE-INDEX c-no NO-LOCK:

                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
                            tt-inv.inv-date    = ar-inv.inv-date
                            tt-inv.trans-date  = ar-cash.check-date
                            tt-inv.inv-no      = ar-cashl.inv-no
                            tt-inv.description = ar-cashl.dscr
                            tt-inv.po-no       = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

                        IF ar-cashl.memo THEN
                            IF ar-cashl.amt-disc NE 0 THEN
                                ASSIGN
                                    tt-inv.type   = "R"
                                    tt-inv.amount = ar-cashl.amt-disc * -1.

                            ELSE  
                                IF ar-cashl.amt-paid + ar-cashl.amt-disc LE 0 THEN
                                    ASSIGN
                                        tt-inv.type   = "CM"
                                        tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

                                ELSE
                                    ASSIGN
                                        tt-inv.type   = "DM"
                                        tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

                        ELSE
                            ASSIGN
                                tt-inv.type   = "P"
                                tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.
                    END.
            END.
            ELSE IF tb_curr-bal THEN 
                DO:
                    IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
                        THEN 
                    DO: 
                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.DESCRIPTION = "No Balance Due"
                            .
                    END.
                END.
        END.

        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.inv-no     EQ 0
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.amt-paid   NE 0
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            CREATE tt-inv.
            ASSIGN
                tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
                tt-inv.inv-date    = ar-cashl.inv-date
                tt-inv.trans-date  = ar-cash.check-date
                tt-inv.inv-no      = ar-cashl.inv-no
                tt-inv.description = ar-cashl.dscr
                tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

            IF ar-cashl.memo THEN
                ASSIGN
                    tt-inv.amount = ar-cashl.amt-paid
                    tt-inv.type   = IF tt-inv.amount LT 0 THEN "CR" ELSE "DR".

            ELSE
                ASSIGN
                    tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
                    tt-inv.type   = "P".
        END.

        ASSIGN
            v-balance = 0 /* reset running balance */
            v-aged    = 0. /* clear aging buckets */
        CLEAR FRAME stmt-header NO-PAUSE.
        CLEAR FRAME stmt-line ALL NO-PAUSE.
        CLEAR FRAME stmt-total NO-PAUSE.
        ws_addr = ''.
        IF AVAILABLE cust THEN
            ASSIGN
                ws_addr[1] = cust.name
                ws_addr[2] = cust.addr[1]
                ws_addr[3] = cust.addr[2]
                ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
        DO yy = 1 TO 6:
            DO zz = yy + 1 TO 6:
                IF ws_addr[yy] EQ '' AND ws_addr[zz] GT ''
                    THEN
                    ASSIGN ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
            END.
        END.

        FOR EACH tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
            BREAK BY "1"
            BY tt-inv.inv-date
            BY tt-inv.sort-fld
            BY tt-inv.trans-date:
            IF FIRST-OF ("1") OR (LINE-COUNTER GT ln-total) THEN 
            DO:

                PAGE.
                IF v-print-hdr THEN
                    DISPLAY
                        ws_letterhead[1]
                        ws_letterhead[2]
                        ws_letterhead[3]
                        ws_letterhead[4]
                        ws_letterhead[5]
                        ws_addr[1]
                        ws_addr[2]
                        ws_addr[3]
                        ws_addr[4]
                        ws_addr[5]
                        v-stmt-date
                        cust.cust-no
                        WITH FRAME stmt-header.
                ELSE
                    DISPLAY
                        ws_letterhead[1]
                        ws_letterhead[2]
                        ws_letterhead[3]
                        ws_letterhead[4]
                        ws_letterhead[5]
                        ws_addr[1]
                        ws_addr[2]
                        ws_addr[3]
                        ws_addr[4]
                        ws_addr[5]
                        v-stmt-date
                        cust.cust-no
                        WITH FRAME no-stmt-header.

            END.

            IF tt-inv.description EQ '' THEN 
            DO:
                msgx = LOOKUP(tt-inv.type,v-inv-type-list).
                IF msgx EQ 0 THEN
                    msgx = 1.    /* assume invoice */
                tt-inv.description =
                    IF msgx GT 0 AND msgx LE v-inv-type-max THEN
                    v-inv-type-array[msgx]
                    ELSE
                    ''.
            END.

            v-balance = v-balance + tt-inv.amount.

            IF v-print-hdr THEN 
            DO:
                DISPLAY
                    tt-inv.trans-date
                    tt-inv.type
                    tt-inv.inv-no  
                    WHEN tt-inv.inv-no GT 0
                    tt-inv.description
                    tt-inv.amount
                    v-balance
                    WITH FRAME stmt-line.
                DOWN 1 WITH FRAME stmt-line.
            END.
            ELSE 
            DO:
                DISPLAY
                    tt-inv.trans-date
                    tt-inv.type
                    tt-inv.inv-no  
                    WHEN tt-inv.inv-no GT 0
                    tt-inv.description
                    tt-inv.amount
                    v-balance
                    WITH FRAME no-stmt-line.
                DOWN 1 WITH FRAME no-stmt-line.
            END.

            v-age = v-stmt-date - tt-inv.inv-date.
            IF v-age = ? OR v-age LT 0 THEN v-age = 0.
            v-per = trunc(v-age / v-days-in-per, 0) + 1.
            IF v-per GT 5 THEN
                v-per = 5.

            v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

            IF LAST-OF ("1") THEN 
            DO:
                adv = ln-total - LINE-COUNTER.

                PUT SKIP(adv).

                IF v-print-hdr THEN
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME stmt-total-line.

                ELSE
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME no-stmt-total-line.

                IF v-print-hdr THEN
                    DISPLAY
                        v-aged[1 for 5]
                        code-legend
                        WITH FRAME stmt-total.

                ELSE
                    DISPLAY
                        v-aged[1 for 5]
                        code-legend
                        WITH FRAME no-stmt-total.

                PUT SKIP(1).
            END.
        END.  /* for each tt-inv */

    END. /* for each cust record */

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail C-Win 
PROCEDURE run-report-mail :
    /* ------------------------------------------------ ar/rep/stmt.p 07/95 CAH   */
    /* A/R Statment Print Program - A/R Module                                    */
    /* -------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER icCustNo   AS CHARACTER NO-UNDO.

    IF LOOKUP(v-stmt-char,"ASIXprnt,stmtprint 1,stmtprint 2,Loylang,RFC,Premier,StmtPrint-Mex,Badger,Printers") > 0 THEN 
    DO:
        RUN run-asistmt-mail (icCustNo).
        RETURN.
    END.
    IF LOOKUP(v-stmt-char,"Protagon,Soule,StdStatement10,StdStatement2,ARStmt3C,SouleMed") > 0 THEN 
    DO:
        RUN run-protagonstmt (icCustNo, NO, YES).
        RETURN.
    END.


{sys/form/r-top.f}

    DEFINE VARIABLE v-stmt-date      AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Statement Date".
    DEFINE VARIABLE v-lo-cust        LIKE cust.cust-no LABEL "From Customer#" NO-UNDO.
    DEFINE VARIABLE v-hi-cust        LIKE cust.cust-no LABEL "Thru Customer#" NO-UNDO.
    DEFINE VARIABLE v-msg            AS CHARACTER NO-UNDO FORMAT 'x(40)' LABEL "Statement Message".
    DEFINE VARIABLE v-detail         AS LOG       FORMAT "yes/no" LABEL "Print Detail?" NO-UNDO.
    DEFINE VARIABLE v-past-due       AS LOG       NO-UNDO FORMAT "yes/no" LABEL "Print Past Due Only?".

    DEFINE VARIABLE xx               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE yy               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE zz               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-print-align    AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-align-ok       AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE save_id          AS RECID     NO-UNDO.
    DEFINE VARIABLE v-balance        AS DECIMAL   LABEL "Balance" FORMAT '->>,>>>,>>>.99CR'.
    DEFINE VARIABLE v-age            AS INTEGER   NO-UNDO. /* number of days old */
    DEFINE VARIABLE v-per            AS INTEGER   NO-UNDO. /* hash of v-age into aging periods */
    DEFINE VARIABLE v-aged           AS DECIMAL   NO-UNDO EXTENT 5
        FORMAT ">>,>>>,>>>.99CR" .   /* aging buckets */
    DEFINE VARIABLE v-days-in-per    AS INTEGER   NO-UNDO INIT 30.
    DEFINE VARIABLE ln-total         AS INTEGER   NO-UNDO INIT 51.
    DEFINE VARIABLE adv              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ws_letterhead    AS CHARACTER FORMAT 'x(80)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE ws_addr          AS CHARACTER FORMAT 'x(35)' NO-UNDO EXTENT 6.
    DEFINE VARIABLE code-legend      AS CHARACTER FORMAT 'X(80)' NO-UNDO.

    DEFINE VARIABLE ld-due           AS DECIMAL   NO-UNDO.

    /* 07.11.95 by CAH @ASI:
    1.  There is no ar transaction type file in system, so the following
    vars have been added to support structured definition via lookups.
    */
    DEFINE VARIABLE msgx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-type-descr AS CHARACTER FORMAT 'x(30)' NO-UNDO.
    DEFINE VARIABLE v-inv-type-list  AS CHARACTER NO-UNDO INIT "I,CR,DR,P,DA,FC,R".
    DEFINE VARIABLE v-inv-type-max   AS INTEGER   NO-UNDO.
    v-inv-type-max = NUM-ENTRIES(v-inv-type-list).
    DEFINE VARIABLE v-inv-type-array AS CHARACTER NO-UNDO EXTENT 7 INIT
        ["Invoice",
        "CR Memo",
        "DR Memo",
        "Payment",
        "Disc Allowed",
        "Finance Chg",
        "Return"].

    code-legend = "CODES: ".
    DO xx = 1 TO v-inv-type-max:
        code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
            + v-inv-type-array[xx] + ' '.
    END.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*was 3*/
        "Statement date" AT 50   "Account#" AT 65 SKIP
        ws_addr[2]    AT 11
        "--------------" AT 50   "--------" AT 65 SKIP
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(1)
        "=============================== S T A T E M E N T ============================"
        SKIP
        WITH FRAME stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date COLUMN-LABEL "Date"
        tt-inv.type     COLUMN-LABEL "Code"
        tt-inv.inv-no  COLUMN-LABEL "Ref no"
        tt-inv.description COLUMN-LABEL "Description"
        tt-inv.amount      COLUMN-LABEL "Amount"
        v-balance       COLUMN-LABEL "Balance"
        WITH FRAME stmt-line NO-BOX STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(2)
        "      Current         30 Days         60 Days         90 Days        >90 Days"
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        ws_letterhead[1]    SKIP
        ws_letterhead[2]    SKIP
        ws_letterhead[3]    SKIP
        ws_letterhead[4]    SKIP
        ws_letterhead[5]    SKIP(1)
        SKIP(5)
        ws_addr[1]    AT 11 /*3*/
        ws_addr[2]    AT 11
        ws_addr[3]    AT 11
        v-stmt-date      AT 53    cust.cust-no AT 65 SKIP
        ws_addr[4]    AT 11 SKIP
        ws_addr[5]    AT 11 SKIP
        SKIP(4)
        SKIP
        WITH FRAME no-stmt-header NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    FORM
        tt-inv.trans-date
        tt-inv.type
        tt-inv.inv-no
        tt-inv.description
        tt-inv.amount
        v-balance
        WITH FRAME no-stmt-line NO-BOX NO-LABELS STREAM-IO WIDTH 80 DOWN.

    FORM
        v-msg AT 15
        v-balance  AT 63
        WITH FRAME no-stmt-total-line NO-BOX NO-LABELS STREAM-IO.

    FORM
        SKIP(3)
        SKIP
        v-aged[1 for 5] SKIP(1)
        code-legend SKIP
        WITH FRAME no-stmt-total NO-BOX NO-LABELS STREAM-IO WIDTH 80.

    /*if v-use-cust then
       find first cust WHERE
            cust.company eq cocode AND
            cust.active  eq "S"
            no-lock no-error.*/

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

    IF v-print-hdr AND AVAILABLE company THEN 
    DO:
        yy = 1.

        IF company.name    NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.name
                yy                = yy + 1.

        IF company.addr[1] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[1]
                yy                = yy + 1.

        IF company.addr[2] NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.addr[2]
                yy                = yy + 1.

        IF company.city    NE "" OR
            company.state   NE "" OR
            company.zip     NE "" THEN
            ASSIGN
                ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
                yy                = yy + 1.

        DO xx = 1 TO 6:
            IF ws_letterhead[xx] GT '' THEN
                ws_letterhead[xx] = FILL(" ", int((80 - length(ws_letterhead[xx])) / 2))
                    + ws_letterhead[xx].
        END.
    END.

    ASSIGN
        str-tit2    = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 56}

        v-stmt-date = stmt-date
        v-lo-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE begin_cust-no
        v-hi-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE end_cust-no
        v-msg       = stmt-msg
        v-detail    = tb_detailed
        v-past-due  = tb_past-due.

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    PAGE.

    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode 
        AND cust.cust-no EQ ttCustList.cust-no
        /*     cust.cust-no ge v-lo-cust and */
        /*     cust.cust-no le v-hi-cust and */
        AND ((cust.acc-bal NE 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        TRANSACTION:

        FOR EACH tt-inv:
            DELETE tt-inv.
        END.    /* clear workfile */

        IF v-past-due THEN
        DO:
            FIND FIRST ar-inv WHERE ar-inv.company EQ cust.company    AND
                ar-inv.cust-no EQ cust.cust-no AND
                ar-inv.posted                AND
                ar-inv.due NE 0              AND
                ar-inv.inv-date LE v-stmt-date AND
                ar-inv.due-date LE v-stmt-date NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ar-inv THEN NEXT.
        END.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cust.company
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.posted   EQ YES
            AND ar-inv.terms    NE "CASH"
            AND ar-inv.inv-date LE v-stmt-date
            NO-LOCK:

            FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                ld-due = ar-inv.net.
            ELSE
                ld-due = ar-inv.gross.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-stmt-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.amt-disc NE 0 THEN
                        ld-due = ld-due - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                            ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END.

            IF ld-due NE 0 THEN 
            DO:
                FIND FIRST tt-inv 
                    WHERE tt-inv.cust-no EQ cust.cust-no
                    AND tt-inv.DESCRIPTION EQ "No Balance Due"
                    NO-ERROR.
                IF AVAILABLE tt-inv THEN DELETE tt-inv.
                CREATE tt-inv.
                ASSIGN
                    tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
                    tt-inv.inv-date   = ar-inv.inv-date
                    tt-inv.trans-date = ar-inv.inv-date
                    tt-inv.inv-no     = ar-inv.inv-no
                    tt-inv.type       = IF ar-inv.type GT ' ' THEN ar-inv.type ELSE 'I'
                    tt-inv.amount     = IF v-detail THEN
                           IF ar-inv.net     EQ
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt THEN ar-inv.net ELSE ar-inv.gross
                          ELSE ar-inv.due
                    tt-inv.po-no      = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

                IF v-detail THEN
                    FOR EACH ar-cashl
                        WHERE ar-cashl.company  EQ ar-inv.company
                        AND ar-cashl.posted   EQ YES
                        AND ar-cashl.cust-no  EQ ar-inv.cust-no
                        AND ar-cashl.inv-no   EQ ar-inv.inv-no
                        USE-INDEX inv-no NO-LOCK,

                        EACH ar-cash
                        WHERE ar-cash.c-no       EQ ar-cashl.c-no
                        AND ar-cash.check-date LE v-stmt-date
                        USE-INDEX c-no NO-LOCK:

                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
                            tt-inv.inv-date    = ar-inv.inv-date
                            tt-inv.trans-date  = ar-cash.check-date
                            tt-inv.inv-no      = ar-cashl.inv-no
                            tt-inv.description = ar-cashl.dscr
                            tt-inv.po-no       = (IF AVAILABLE ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

                        IF ar-cashl.memo THEN
                            IF ar-cashl.amt-disc NE 0 THEN
                                ASSIGN
                                    tt-inv.type   = "R"
                                    tt-inv.amount = ar-cashl.amt-disc * -1.

                            ELSE  
                                IF ar-cashl.amt-paid + ar-cashl.amt-disc LE 0 THEN
                                    ASSIGN
                                        tt-inv.type   = "CM"
                                        tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

                                ELSE
                                    ASSIGN
                                        tt-inv.type   = "DM"
                                        tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

                        ELSE
                            ASSIGN
                                tt-inv.type   = "P"
                                tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.
                    END.
            END.
            ELSE IF tb_curr-bal THEN 
                DO:
                    IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
                        THEN 
                    DO: 
                        CREATE tt-inv.
                        ASSIGN
                            tt-inv.cust-no     = cust.cust-no
                            tt-inv.DESCRIPTION = "No Balance Due"
                            .
                    END.
                END.
        END.

        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.inv-no     EQ 0
            AND (ar-cashl.inv-date  LE v-stmt-date OR
            ar-cashl.inv-date  EQ ?)
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.amt-paid   NE 0
            NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-stmt-date
            USE-INDEX c-no NO-LOCK:

            CREATE tt-inv.
            ASSIGN
                tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
                tt-inv.inv-date    = ar-cashl.inv-date
                tt-inv.trans-date  = ar-cash.check-date
                tt-inv.inv-no      = ar-cashl.inv-no
                tt-inv.description = ar-cashl.dscr
                tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

            IF ar-cashl.memo THEN
                ASSIGN
                    tt-inv.amount = ar-cashl.amt-paid
                    tt-inv.type   = IF tt-inv.amount LT 0 THEN "CR" ELSE "DR".

            ELSE
                ASSIGN
                    tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
                    tt-inv.type   = "P".
        END.

        ASSIGN
            v-balance = 0 /* reset running balance */
            v-aged    = 0. /* clear aging buckets */
        CLEAR FRAME stmt-header NO-PAUSE.
        CLEAR FRAME stmt-line ALL NO-PAUSE.
        CLEAR FRAME stmt-total NO-PAUSE.
        ws_addr = ''.
        IF AVAILABLE cust THEN
            ASSIGN
                ws_addr[1] = cust.name
                ws_addr[2] = cust.addr[1]
                ws_addr[3] = cust.addr[2]
                ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
        DO yy = 1 TO 6:
            DO zz = yy + 1 TO 6:
                IF ws_addr[yy] EQ '' AND ws_addr[zz] GT ''
                    THEN
                    ASSIGN ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
            END.
        END.

        FOR EACH tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
            BREAK BY "1"
            BY tt-inv.inv-date
            BY tt-inv.sort-fld
            BY tt-inv.trans-date:
            IF FIRST-OF ("1") OR (LINE-COUNTER GT ln-total) THEN 
            DO:

                PAGE.
                IF v-print-hdr THEN
                    DISPLAY
                        ws_letterhead[1]
                        ws_letterhead[2]
                        ws_letterhead[3]
                        ws_letterhead[4]
                        ws_letterhead[5]
                        ws_addr[1]
                        ws_addr[2]
                        ws_addr[3]
                        ws_addr[4]
                        ws_addr[5]
                        v-stmt-date
                        cust.cust-no
                        WITH FRAME stmt-header.
                ELSE
                    DISPLAY
                        ws_letterhead[1]
                        ws_letterhead[2]
                        ws_letterhead[3]
                        ws_letterhead[4]
                        ws_letterhead[5]
                        ws_addr[1]
                        ws_addr[2]
                        ws_addr[3]
                        ws_addr[4]
                        ws_addr[5]
                        v-stmt-date
                        cust.cust-no
                        WITH FRAME no-stmt-header.

            END.

            IF tt-inv.description EQ '' THEN 
            DO:
                msgx = LOOKUP(tt-inv.type,v-inv-type-list).
                IF msgx EQ 0 THEN
                    msgx = 1.    /* assume invoice */
                tt-inv.description =
                    IF msgx GT 0 AND msgx LE v-inv-type-max THEN
                    v-inv-type-array[msgx]
                    ELSE
                    ''.
            END.

            v-balance = v-balance + tt-inv.amount.

            IF v-print-hdr THEN 
            DO:
                DISPLAY
                    tt-inv.trans-date
                    tt-inv.type
                    tt-inv.inv-no  
                    WHEN tt-inv.inv-no GT 0
                    tt-inv.description
                    tt-inv.amount
                    v-balance
                    WITH FRAME stmt-line.
                DOWN 1 WITH FRAME stmt-line.
            END.
            ELSE 
            DO:
                DISPLAY
                    tt-inv.trans-date
                    tt-inv.type
                    tt-inv.inv-no  
                    WHEN tt-inv.inv-no GT 0
                    tt-inv.description
                    tt-inv.amount
                    v-balance
                    WITH FRAME no-stmt-line.
                DOWN 1 WITH FRAME no-stmt-line.
            END.

            v-age = v-stmt-date - tt-inv.inv-date.
            IF v-age = ? OR v-age LT 0 THEN v-age = 0.
            v-per = trunc(v-age / v-days-in-per, 0) + 1.
            IF v-per GT 5 THEN
                v-per = 5.

            v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

            IF LAST-OF ("1") THEN 
            DO:
                adv = ln-total - LINE-COUNTER.

                PUT SKIP(adv).

                IF v-print-hdr THEN
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME stmt-total-line.

                ELSE
                    DISPLAY
                        v-msg
                        v-balance
                        WITH FRAME no-stmt-total-line.

                IF v-print-hdr THEN
                    DISPLAY
                        v-aged[1 for 5]
                        code-legend
                        WITH FRAME stmt-total.

                ELSE
                    DISPLAY
                        v-aged[1 for 5]
                        code-legend
                        WITH FRAME no-stmt-total.

                PUT SKIP(1).
            END.
        END.  /* for each tt-inv */

    END. /* for each cust record */



/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 C-Win 
PROCEDURE SendMail-1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icFileName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vcSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcMailBody AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcErrorMsg AS CHARACTER NO-UNDO.

    IF v-stmt-char EQ "ASIExcel" OR v-stmt-char EQ "SouleExcel" THEN
        ASSIGN icFileName = v-dir + "\stmt.xls"      .

    ASSIGN 
        vcSubject  = "STATEMENT" + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcMailBody = "Please review attached statement(s).".

    RUN custom/xpmail2.p   (INPUT   icRecType,
        INPUT   'R-STMT.',
        INPUT   icFileName,
        INPUT   icIdxKey,
        INPUT   vcSubject,
        INPUT   vcMailBody,
        OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttentionDefault C-Win 
PROCEDURE setAttentionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER lbf-cust FOR cust.

    IF (v-stmt-char = "Protagon" OR v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "StdStatement2" OR v-stmt-char = "ARStmt3C" OR v-stmt-char = "SouleMed")
        AND begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ end_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
        AND begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN 
    DO:
        FIND FIRST lbf-cust WHERE lbf-cust.company = cocode
            AND lbf-cust.cust-no = begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
        IF AVAILABLE lbf-cust THEN 
            ASSIGN
                fi_contact:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lbf-cust.contact
                fi_contact                                     = lbf-cust.contact.
    END.
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
            begin_cust-no:SENSITIVE = NOT iplChecked
            end_cust-no:SENSITIVE   = NOT iplChecked
            begin_cust-no:VISIBLE   = NOT iplChecked
            end_cust-no:VISIBLE     = NOT iplChecked
            btnCustList:SENSITIVE   = iplChecked
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEmailBoxes C-Win 
PROCEDURE SetEmailBoxes :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' THEN
    DO:
        ASSIGN 
            tb_BatchMail:SENSITIVE  = YES
            tb_HideDialog:SENSITIVE = YES.
    END.

    ELSE
        ASSIGN tb_BatchMail:SENSITIVE  = NO
            tb_BatchMail:CHECKED    = NO
            tb_HideDialog:SENSITIVE = NO
            tb_HideDialog:CHECKED   = NO
            .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetStmtForm C-Win 
PROCEDURE SetStmtForm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icPrintFormat AS CHARACTER NO-UNDO.

    v-stmt-char = icPrintFormat.





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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunFormatValueChanged C-Win 
PROCEDURE pRunFormatValueChanged :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
       
        IF (v-stmt-char EQ "Protagon" OR v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "StdStatement2" OR v-stmt-char = "ARStmt3C" OR v-stmt-char = "SouleMed") THEN 
        DO:
            fi_contact:HIDDEN IN FRAME {&FRAME-NAME} = NO.
            RUN setAttentionDefault.
        END.
        ELSE
            fi_contact:HIDDEN IN FRAME {&FRAME-NAME} = YES.
       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION formatDate C-Win 
FUNCTION formatDate RETURNS CHARACTER
    ( INPUT ip-date AS DATE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE out-date AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMonth   AS CHARACTER EXTENT 12 NO-UNDO INIT
        [ "January", "February", "March", 
        "April", "May", "June", 
        "July", "August", "September",
        "October", "November", "December" ]. 

    out-date = cmonth[MONTH(ip-date)] + " " + STRING(DAY(ip-date)) + " " + STRING(YEAR(ip-date)).
    RETURN out-date.  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

