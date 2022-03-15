&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-chkreg.w

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
DEFINE VARIABLE list-name    AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

{api/ttAPIOutboundEvent.i}

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE NEW SHARED VARIABLE v-trnum      AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-unline     AS CHARACTER FORMAT "x(80)" INIT
    "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
DEFINE            VARIABLE time_stamp   AS ch        NO-UNDO.
DEFINE            VARIABLE v-postable   AS LOG       INIT NO NO-UNDO.
DEFINE            VARIABLE v-invalid    AS LOG       NO-UNDO.
DEFINE            VARIABLE save_id      AS RECID     NO-UNDO.
DEFINE            VARIABLE pct-paid     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE gtot0        LIKE ap-sel.inv-bal NO-UNDO.
DEFINE            VARIABLE gtot1        LIKE ap-sel.disc-amt NO-UNDO.
DEFINE            VARIABLE gtot2        LIKE ap-sel.amt-paid NO-UNDO.
DEFINE            VARIABLE ndisc        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE wckdate      AS DATE      NO-UNDO.
DEFINE            VARIABLE bal          AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE bal1         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-of-inv   AS de        FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v-fst-chk    AS LOG       NO-UNDO.
DEFINE            VARIABLE v-lst-chk    AS LOG       NO-UNDO.
DEFINE            VARIABLE tot0         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot1         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot2         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE c1           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE op           AS LOG       FORMAT "Yes/No" NO-UNDO.
DEFINE            VARIABLE ctr          AS INTEGER   LABEL "NUMBER OF CHECKS WRITTEN " NO-UNDO.
DEFINE            VARIABLE credit       AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE tdisc        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE wcash        AS CHARACTER FORMAT "x(25)" LABEL "CASH ACCOUNT" NO-UNDO.
DEFINE            VARIABLE v-frt-acct   LIKE ap-ctrl.freight NO-UNDO.
DEFINE            VARIABLE ap-acct      LIKE ap-ctrl.payables NO-UNDO.
DEFINE            VARIABLE v-loop-count AS INTEGER   NO-UNDO INITIAL 10 .
DEFINE            VARIABLE post-manual  AS LOG       FORMAT "Manual/Automatic"
    LABEL "Post Manual or Automatic (M/A)?" INITIAL NO NO-UNDO.
DEFINE            VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.

/* gdm - 05210901 */
DEFINE            VARIABLE v-fileFormat AS CHARACTER NO-UNDO.
DEFINE STREAM checkFile.
DEFINE VARIABLE v-amt-paid                LIKE ap-sel.amt-paid NO-UNDO.

DEFINE VARIABLE lLocked                   AS LOG       NO-UNDO.
DEFINE VARIABLE cUsr                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cName                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDevice                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cApCheckTextFile          AS CHARACTER NO-UNDO .
DEFINE VARIABLE cApCheckCsvFile           AS CHARACTER NO-UNDO .

DEFINE VARIABLE hdOutboundProcs           AS HANDLE    NO-UNDO.
DEFINE VARIABLE lBankTransmitRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBankTransmitRecValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBankTransmitFullFilePath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBankTransmitFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBankTransmitSysCtrlName  AS CHARACTER NO-UNDO INITIAL "BankTransmittalLocation".
DEFINE VARIABLE cRecValue                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iAPCheckFile              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lBankTransmittalLocation  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName                 AS CHARACTER NO-UNDO.

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

DEFINE STREAM excel.
DEFINE STREAM checkFile.
DEFINE STREAM ap-excel.

DEFINE TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id    AS ROWID
    FIELD ex-rate   LIKE currency.ex-rate INIT 1
    FIELD curr-bal  LIKE ap-sel.inv-bal
    FIELD curr-disc LIKE ap-sel.disc-amt
    FIELD curr-paid LIKE ap-sel.amt-paid
    FIELD actnum    LIKE account.actnum.

FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode
    NO-LOCK NO-WAIT NO-ERROR.
IF AVAILABLE ap-ctrl THEN
    ASSIGN wcash      = ap-ctrl.cash-act
        ap-acct    = ap-ctrl.payables
        v-frt-acct = ap-ctrl.freight.

RELEASE ap-ctrl.


DO TRANSACTION:
    {sys/inc/postdate.i}
    {sys/inc/aplockbx.i}   
    
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS".
    END.

    lv-audit-dir = sys-ctrl.char-fld.

    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

    RELEASE sys-ctrl.
   
    RUN sys/ref/nk1look.p (
        INPUT  cocode,                   /* Company Code */
        INPUT  "APCheckFile",            /* sys-ctrl name */
        INPUT  "I",                      /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                    /* Use ship-to */
        INPUT  FALSE,                    /* ship-to vendor */
        INPUT  "",                       /* ship-to vendor value */
        INPUT  "",                       /* shi-id value */
        OUTPUT cRecValue,
        OUTPUT lRecFound
        ).
    IF lRecFound THEN
        iAPCheckFile = INTEGER(cRecValue) NO-ERROR.          
END.

DEFINE VARIABLE lAPInvoiceLength AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value        AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = LOGICAL(cNK1Value) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "BankTransmittalLocation", "l" /* Logical */, NO /* check by cust */,
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lBankTransmittalLocation = LOGICAL(cNK1Value) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date rd_sort tb_prt-acc ~
tb_void rd_print-apfile tb_APcheckFile fi_CheckFile tbTransmitFile ~
fiTransmitFile rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period lbl_sort rd_sort ~
tb_prt-acc tb_void rd_print-apfile tb_APcheckFile fi_CheckFile ~
tbTransmitFile rd-dest fi_file tb_OpenCSV tbAutoClose 

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
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE fiTransmitFile  AS CHARACTER FORMAT "X(100)" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 55 BY .81.

DEFINE VARIABLE fi_CheckFile    AS CHARACTER FORMAT "X(80)" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 55 BY .81.

DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\APChecksRegister.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort        AS CHARACTER FORMAT "X(256)":U INITIAL "Post Automatic or Manual Checks?" 
    VIEW-AS FILL-IN 
    SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page  AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name    AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no      AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Post Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period     AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt         AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest         AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To CSV", 3
    SIZE 15.2 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_print-apfile AS CHARACTER INITIAL "Text" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Text", "Text",
    "CSV", "CSV"
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort         AS CHARACTER INITIAL "Automatic" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Manual", "Manual",
    "Automatic", "Automatic"
    SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 4.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 10.24.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tbTransmitFile AS LOGICAL INITIAL NO 
    LABEL "Transmit File" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_APcheckFile AS LOGICAL INITIAL NO 
    LABEL "Create AP Check File?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-acc     AS LOGICAL INITIAL NO 
    LABEL "Print Invoice & GL Account Detail?" 
    VIEW-AS TOGGLE-BOX
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_void        AS LOGICAL INITIAL NO 
    LABEL "Void Skipped Checks?" 
    VIEW-AS TOGGLE-BOX
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 2.24 COL 41 COLON-ALIGNED
    tran-period AT ROW 3.43 COL 41 COLON-ALIGNED
    lbl_sort AT ROW 4.86 COL 13 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 4.86 COL 51 NO-LABELS
    tb_prt-acc AT ROW 6.1 COL 20.8
    tb_void AT ROW 7.29 COL 20.8
    rd_print-apfile AT ROW 8.48 COL 54.8 NO-LABELS WIDGET-ID 6
    tb_APcheckFile AT ROW 8.57 COL 21 WIDGET-ID 4
    fi_CheckFile AT ROW 9.62 COL 36.8 COLON-ALIGNED HELP
    "Enter File Name" NO-LABELS WIDGET-ID 2
    tbTransmitFile AT ROW 10.38 COL 21 WIDGET-ID 12
    fiTransmitFile AT ROW 10.57 COL 36.8 COLON-ALIGNED HELP
    "Enter File Name" NO-LABELS WIDGET-ID 10
    lv-font-no AT ROW 12.33 COL 32.8 COLON-ALIGNED
    lv-ornt AT ROW 12.33 COL 43.4 NO-LABELS
    lines-per-page AT ROW 12.33 COL 87.6 COLON-ALIGNED
    rd-dest AT ROW 12.43 COL 4.8 NO-LABELS
    lv-font-name AT ROW 13.29 COL 29.6 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14.1 COL 27.8
    tb_excel AT ROW 14.19 COL 92 RIGHT-ALIGNED
    fi_file AT ROW 15.05 COL 25.8 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 15.14 COL 92 RIGHT-ALIGNED
    tbAutoClose AT ROW 16.71 COL 28 WIDGET-ID 64
    btn-ok AT ROW 17.71 COL 27.8
    btn-cancel AT ROW 17.71 COL 48.4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 11.76 COL 4
    RECT-6 AT ROW 12.19 COL 3
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 20.48
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
        TITLE              = "A/P Checks Register"
        HEIGHT             = 18.24
        WIDTH              = 96
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
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN fiTransmitFile IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
    fiTransmitFile:READ-ONLY IN FRAME FRAME-A    = TRUE
    fiTransmitFile:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_CheckFile:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

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
    rd_print-apfile:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/P Checks Register */
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
ON WINDOW-CLOSE OF C-Win /* A/P Checks Register */
    DO:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
          
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
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DEFINE VARIABLE lv-post      AS LOG       NO-UNDO.
        DEFINE VARIABLE lv-bank-file AS CHARACTER NO-UNDO.
  
        IF rd_print-apfile:SCREEN-VALUE EQ "Text" THEN ASSIGN 
                fi_CheckFile:SCREEN-VALUE = cApCheckTextFile.
        ELSE ASSIGN 
                fi_CheckFile:SCREEN-VALUE = cApCheckCsvFile.

        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        post-manual = rd_sort EQ "Manual".

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            REPEAT:
                FIND FIRST gl-ctrl NO-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                ASSIGN 
                    lLocked = NO
                    cUsr    = 0
                    cName   = ""
                    cDevice = "".
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    RUN checkLock(INPUT "gl-ctrl",
                        INPUT ROWID(gl-ctrl),
                        OUTPUT lLocked,
                        OUTPUT cUsr,
                        OUTPUT cName,
                        OUTPUT cDevice).
                    IF lLocked THEN 
                    DO:
                        /* to dispaly regular progress lock message */
                        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                            WHERE gl-ctrl.company EQ cocode .
                        RELEASE gl-ctrl.
                    /*             MESSAGE "The General Ledger Control Record is locked by " cName SKIP */
                    /*                     "Press OK to try again."                                     */
                    /*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                 */
                    END.
                END. /* avail gl-ctrl */

                RELEASE gl-ctrl.

                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode 
                    NO-ERROR NO-WAIT.

                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        v-trnum       = gl-ctrl.trnum + 1 
                        gl-ctrl.trnum = v-trnum.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.

        RUN run-report.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
        END CASE.

        FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ap-ctrl THEN RETURN.
        IF AVAILABLE ap-ctrl  AND ap-ctrl.payables EQ "" THEN 
        DO:
            MESSAGE "No AP control record found. " VIEW-AS ALERT-BOX .
            RETURN.
        END.

        IF v-postable THEN 
        DO:    
            lv-post = NO.

            MESSAGE "Post to Vendor & G/L Files?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:  
                IF aplockbx-log THEN 
                DO:
                    RUN create-bank-file (OUTPUT lv-bank-file).
                    MESSAGE "Check Register/Lock Box file is created into " 
                        aplockbx-path + lv-bank-file
                        VIEW-AS ALERT-BOX INFORMATION.
                END.
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

            END.
            ELSE RUN undo-trnum.
        END.
        ELSE 
        DO:
            MESSAGE "No A/P Checks available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTransmitFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTransmitFile C-Win
ON LEAVE OF fiTransmitFile IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_CheckFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_CheckFile C-Win
ON LEAVE OF fi_CheckFile IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_print-apfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print-apfile C-Win
ON VALUE-CHANGED OF rd_print-apfile IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        IF rd_print-apfile:SCREEN-VALUE EQ "Text" THEN
            fi_CheckFile:SCREEN-VALUE = cApCheckTextFile.
        ELSE
            fi_CheckFile:SCREEN-VALUE = cApCheckCsvFile.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbTransmitFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbTransmitFile C-Win
ON VALUE-CHANGED OF tbTransmitFile IN FRAME FRAME-A /* Transmit File */
    DO:
        fiTransmitFile:HIDDEN = NOT SELF:CHECKED.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_APcheckFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_APcheckFile C-Win
ON VALUE-CHANGED OF tb_APcheckFile IN FRAME FRAME-A /* Create AP Check File? */
    DO:
        ASSIGN {&self-name}.

        IF TRIM(v-fileFormat) NE "" 
            AND {&self-name}:CHECKED THEN ASSIGN 
                fi_CheckFile:HIDDEN    = NO
                rd_print-apfile:HIDDEN = NO.
        ELSE ASSIGN 
                fi_CheckFile:HIDDEN    = YES
                rd_print-apfile:HIDDEN = YES .
     
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


&Scoped-define SELF-NAME tb_prt-acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-acc C-Win
ON VALUE-CHANGED OF tb_prt-acc IN FRAME FRAME-A /* Print Invoice  GL Account Detail? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_void
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_void C-Win
ON VALUE-CHANGED OF tb_void IN FRAME FRAME-A /* Void Skipped Checks? */
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
    DO:
        ASSIGN {&self-name}.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date.
            IF v-invalid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
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

    tran-date = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR6" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}

        IF postdate-log THEN 
        DO:
            tran-date:SCREEN-VALUE = STRING(TODAY).
            RUN check-date.
        END.
        ELSE ASSIGN
                tran-date:SCREEN-VALUE   = ""
                tran-period:SCREEN-VALUE = "".
    
        ASSIGN 
            tb_APcheckFile = IF tb_APcheckFile:SCREEN-VALUE EQ "YES" THEN YES ELSE NO.         
    
        APPLY "entry" TO tran-date.
    END.

    /* gdm - 05210901 */
    ASSIGN 
        tb_APcheckFile:HIDDEN IN FRAME {&FRAME-NAME}  = YES
        fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME}    = YES
        fiTransmitFile:HIDDEN IN FRAME {&FRAME-NAME}  = YES
        rd_print-apfile:HIDDEN IN FRAME {&FRAME-NAME} = YES .

    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "APCheckFile" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "APCheckFile"
            sys-ctrl.descrip = "Download Text format when printing AP Checks?"
            sys-ctrl.log-fld = YES.
    END.

    FIND FIRST sys-ctrl-shipto NO-LOCK
        WHERE sys-ctrl-shipto.company      EQ cocode
        AND sys-ctrl-shipto.NAME         EQ "APCheckFile"
        AND sys-ctrl-shipto.cust-vend    EQ YES 
        AND sys-ctrl-shipto.cust-vend-no EQ "" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl-shipto THEN 
    DO TRANSACTION:
        CREATE sys-ctrl-shipto.
        ASSIGN
            sys-ctrl-shipto.company      = cocode       
            sys-ctrl-shipto.NAME         = "APCheckFile"
            sys-ctrl-shipto.cust-vend    = YES          
            sys-ctrl-shipto.cust-vend-no = "". 

        MESSAGE 
            "System control record not found. Update APCheckFile file path."
            UPDATE sys-ctrl-shipto.char-fld FORMAT "x(50)".
    END.    

    ASSIGN 
        v-fileFormat = sys-ctrl.char-fld.

    IF AVAILABLE sys-ctrl 
        AND sys-ctrl.log-fld  
        AND TRIM(v-fileFormat) NE "" THEN 
    DO:
        IF sys-ctrl-shipto.char-fld NE "" THEN 
        DO:
            IF SUBSTR(TRIM(sys-ctrl-shipto.char-fld),LENGTH(sys-ctrl-shipto.char-fld) - 3 ,4) NE ".txt" THEN 
            DO:
                IF v-fileFormat EQ "Positive Pay-Santander" THEN ASSIGN 
                        fi_CheckFile = "PositivePay_" + 
                                STRING(YEAR(TODAY),"9999") + 
                                STRING(MONTH(TODAY),"99") + 
                                STRING(DAY(TODAY),"99") + 
                                "_" + 
                                    STRING(TIME) + ".txt".
                ELSE ASSIGN 
                        fi_CheckFile = STRING(TODAY,"99999999") + 
                                STRING(TIME) + ".txt".

                IF SUBSTR(sys-ctrl-shipto.char-fld,LENGTH(sys-ctrl-shipto.char-fld),1) NE "/" 
                    AND SUBSTR(sys-ctrl-shipto.char-fld,LENGTH(sys-ctrl-shipto.char-fld),1) NE "\" THEN ASSIGN 
                        fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + "\" + fi_CheckFile.
                ELSE ASSIGN 
                        fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + fi_CheckFile.
            END.
            ELSE ASSIGN 
                    fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld).
        END.
        ELSE ASSIGN 
                fi_CheckFile = "C:\tmp\" +
                       STRING(TODAY,"99999999") + 
                       STRING(TIME) + ".txt".       

        ASSIGN 
            tb_APcheckFile:HIDDEN IN FRAME {&FRAME-NAME}    = NO
            fi_CheckFile:HIDDEN   IN FRAME {&FRAME-NAME}    = NO
            tb_APcheckFile:HIDDEN    IN FRAME {&FRAME-NAME} = NO 
            tb_APcheckFile:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

        IF fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN ASSIGN 
                fi_CheckFile                                     = "C:\tmp\" + STRING(TODAY,"99999999") + STRING(TIME) + ".txt"
                fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi_CheckFile.
     
        ASSIGN 
            cApCheckTextFile = fi_CheckFile
            cApCheckCsvFile  = SUBSTRING(fi_CheckFile,1,INDEX(fi_CheckFile,".") - 1) + ".csv" .

        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Text" THEN ASSIGN 
                fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cApCheckTextFile.
        ELSE ASSIGN 
                fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cApCheckCsvFile.

        ASSIGN 
            fi_CheckFile:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    
        IF tb_APcheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES" THEN ASSIGN 
                fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME}    = NO
                rd_print-apfile:HIDDEN IN FRAME {&FRAME-NAME} = NO .
        ELSE ASSIGN 
                fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME}    = YES
                rd_print-apfile:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE ASSIGN 
            tb_APcheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No" .
    
    RUN sys/ref/nk1look.p (
        INPUT  cocode,                   /* Company Code */
        INPUT  cBankTransmitSysCtrlName, /* sys-ctrl name */
        INPUT  "L",                      /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                    /* Use ship-to */
        INPUT  FALSE,                    /* ship-to vendor */
        INPUT  "",                       /* ship-to vendor value */
        INPUT  "",                       /* shi-id value */
        OUTPUT cBankTransmitRecValue,
        OUTPUT lBankTransmitRecFound
        ).
    
    fiTransmitFile:HIDDEN IN FRAME {&FRAME-NAME} = lBankTransmitRecFound.
                          
    IF lBankTransmitRecFound 
        AND LOGICAL(cBankTransmitRecValue) THEN 
    DO:
        RUN sys/ref/nk1look.p (
            INPUT  cocode,                   /* Company Code */
            INPUT  cBankTransmitSysCtrlName, /* sys-ctrl name */
            INPUT  "C",                      /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
            INPUT  FALSE,                    /* Use ship-to */
            INPUT  FALSE,                    /* ship-to vendor */
            INPUT  "",                       /* ship-to vendor value */
            INPUT  "",                       /* shi-id value */
            OUTPUT cBankTransmitRecValue,
            OUTPUT lBankTransmitRecFound
            ).
        
        ASSIGN
            tbTransmitFile:CHECKED IN FRAME {&FRAME-NAME} = TRUE
            fiTransmitFile:HIDDEN IN FRAME {&FRAME-NAME}  = FALSE
            cBankTransmitFullFilePath                     = (IF cBankTransmitRecValue NE "" THEN
                                                         cBankTransmitRecValue + "/"
                                                         ELSE
                                                         "C:/Tmp/") 
            .
    END.

    ASSIGN
        cBankTransmitFullFilePath                          = (IF cBankTransmitFullFilePath NE "" THEN
                                 cBankTransmitFullFilePath
                                 ELSE
                                 "C:/Tmp/")
        cBankTransmitFileName                              = "PayablesAdvantage_" 
                                + STRING(YEAR(TODAY),"9999")
                                + STRING(MONTH(TODAY),"99")
                                + STRING(DAY(TODAY),"99")
                                + REPLACE(STRING(TIME,"hh:mm:ss"), ":", "")
                                + ".txt"
        fiTransmitFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cBankTransmitFullFilePath 
                                                         + cBankTransmitFileName
        .
    IF NOT lBankTransmittalLocation THEN ASSIGN
            tbTransmitFile:HIDDEN IN FRAME {&FRAME-NAME} = YES
            fiTransmitFile:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    RUN pChangeDest.        
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

    DO WITH FRAME {&FRAME-NAME}:
        v-invalid = NO.

        FIND FIRST period NO-LOCK                   
            WHERE period.company EQ cocode
            AND period.pst     LE DATE(tran-date:SCREEN-VALUE)
            AND period.pend    GE DATE(tran-date:SCREEN-VALUE)
            NO-ERROR.
        IF AVAILABLE period THEN 
        DO:
            IF NOT period.pstat THEN 
            DO:
                MESSAGE "Period Already Closed..." VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            IF period.subLedgerAP EQ "C" THEN 
            DO:
                MESSAGE "Payables sub ledger already closed..." VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            tran-period:SCREEN-VALUE = STRING(period.pnum).
        END.

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-date
                VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkLock C-Win 
PROCEDURE checkLock :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       Check for a locked record and return locking user
        ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprRow   AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLocked AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUsr AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDevice AS CHARACTER NO-UNDO.

    DEFINE VARIABLE wrecid AS INTEGER NO-UNDO.
    DEFINE VARIABLE wtable AS INTEGER NO-UNDO.

    /* Find the recid of the record */
    CASE ipcTable:
        WHEN "gl-ctrl" THEN 
            DO:
                FIND gl-ctrl WHERE ROWID(gl-ctrl) = iprRow NO-LOCK.
                ASSIGN 
                    wrecid = RECID(gl-ctrl).
            END.
    END CASE.


    FIND asi._file WHERE asi._file._file-name = ipcTable NO-LOCK NO-ERROR.
    IF NOT AVAILABLE asi._file THEN
        RETURN.
    ASSIGN 
        wtable = asi._file._file-num.

    /* Use repeat loop - More efficient than FIND ... WHERE
      due to lack of suitable index on _lock table */
    FIND FIRST asi._lock NO-LOCK NO-ERROR.
    REPEAT:
        IF NOT AVAILABLE asi._lock THEN
            LEAVE.

        IF _lock-recid = wrecid AND 
            _lock-table = wtable AND
            _lock-flag MATCHES "*X*" 
            /* typically we're interested in any form of exclusive lock
                so we test for X lock flag */
            THEN LEAVE.

        FIND NEXT asi._lock NO-LOCK NO-ERROR.
        IF AVAILABLE asi._lock AND asi._lock._lock-recid = ? THEN 
        DO:
            RELEASE asi._lock.
            LEAVE.
        END.

    END.

    IF AVAILABLE(asi._lock) THEN 
    DO:
        FIND FIRST asi._connect WHERE _connect-usr = _lock-usr NO-LOCK.
        IF AVAIL(asi._connect) AND asi._connect._connect-name = "" THEN
            FIND asi._user 
                WHERE asi._user._user_number EQ asi._connect._connect-usr
                NO-LOCK NO-ERROR.
        ASSIGN 
            oplLocked = YES
            opcUsr    = asi._connect._connect-usr
            opcName   = _connect-name
            opcDevice = _connect-device.
        IF asi._connect._connect-name EQ "" THEN
            FIND asi._connect WHERE asi._connect._connect-usr EQ asi._connect._connect-usr NO-LOCK.
        IF AVAIL(asi._connect) THEN
            opcName = asi._connect._connect-name.    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-report-to-audit-dir C-Win 
PROCEDURE copy-report-to-audit-dir :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE targetfile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname3   AS CHARACTER FORMAT "X(20)" NO-UNDO.

    ASSIGN 
        targetfile = lv-audit-dir + "\AP\VC3\Run#" + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AP"
        dirname3   = lv-audit-dir + "\AP\VC3".

    OS-COPY VALUE(list-name) VALUE (targetfile).

    IF SEARCH(targetfile) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
        OS-CREATE-DIR VALUE(dirname2).
        OS-CREATE-DIR VALUE(dirname3).
        OS-COPY VALUE(list-name) VALUE (targetfile).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bank-file C-Win 
PROCEDURE create-bank-file :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-data-file AS CHARACTER NO-UNDO.

    DEFINE VARIABLE targetfile          AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1            AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE v-account           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ref               AS cha       NO-UNDO.
    DEFINE VARIABLE v-check-date        AS DATE      NO-UNDO.
    DEFINE VARIABLE v-check-date-string AS cha       NO-UNDO.
    DEFINE VARIABLE v-total-amt         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-check-total-amt   AS DECIMAL   NO-UNDO.

    ASSIGN 
        targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
        dirname1   = aplockbx-path
        .

    IF SEARCH(dirname1) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
    END.

    OUTPUT TO VALUE(targetfile).
    PUT UNFORMATTED 
        "01021226C3648091" SKIP.
    v-total-amt = 0.
    FOR EACH tt-post,
        FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,
        FIRST ap-chk NO-LOCK WHERE ap-chk.company   EQ ap-sel.company
        AND ap-chk.check-no  EQ ap-sel.check-no,
        FIRST ap-inv NO-LOCK WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,    
        FIRST vend NO-LOCK WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no USE-INDEX vend 
        BREAK BY ap-sel.bank-code BY ap-sel.check-no:

        IF FIRST-OF(ap-sel.check-no) THEN v-check-total-amt = 0.

        v-check-total-amt = v-check-total-amt + ap-sel.amt-paid.
        IF LAST-OF(ap-sel.check-no) THEN 
        DO:
            FIND FIRST bank WHERE bank.company = ap-sel.company AND
                bank.bank-code = ap-sel.bank-code NO-ERROR.

            ASSIGN
                v-account           = IF AVAILABLE bank THEN bank.bk-act ELSE ""
                v-ref               = SUBSTRING(vend.name,1,12)
                v-check-date        = IF ap-sel.man-check THEN ap-sel.pre-date ELSE ap-sel.check-date
                v-check-date-string = STRING(MONTH(v-check-date),"99") +
                                STRING(DAY(v-check-date),"99") + 
                                SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
                v-total-amt         = v-total-amt + v-check-total-amt.
            PUT UNFORMATTED 
                "V"
                ap-chk.check-no FORM "9999999999"
                v-account FORM "99999999999999"
                v-check-total-amt * 100 FORM "9999999999"
                v-ref FORM  "x(12)"
                v-check-date-string FORM "x(6)"
                SKIP.
        END.

    END.
    PUT UNFORMATTED 
        "T          "
        v-account FORM "99999999999999"
        v-total-amt * 100 FORM "9999999999"
        SKIP.

    OUTPUT CLOSE.
    op-data-file = targetfile.

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
    DISPLAY tran-date tran-period lbl_sort rd_sort tb_prt-acc tb_void 
        rd_print-apfile tb_APcheckFile fi_CheckFile tbTransmitFile rd-dest 
        fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date rd_sort tb_prt-acc tb_void rd_print-apfile 
        tb_APcheckFile fi_CheckFile tbTransmitFile fiTransmitFile rd-dest 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

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

    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PositivePay C-Win 
PROCEDURE PositivePay :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes: gdm - 05210901
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtCheckDate AS DATE NO-UNDO.
    ASSIGN 
        dtCheckDate = ap-sel.check-date + iAPCheckFile.   
    
    IF tb_APcheckFile THEN 
    DO:
        FIND FIRST bank NO-LOCK
            WHERE bank.company EQ cocode
            AND bank.bank-code EQ ap-sel.bank-code NO-ERROR.
 
        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&frame-name} EQ "Text" THEN 
        DO:
            /* Output to .txt file */
            PUT STREAM checkFile UNFORMATTED
                "D"
                STRING(INT(ap-sel.bank-code),"999")                         /* Bank Number    */
                STRING(REPLACE(bank.bk-act,"-",""), "x(14)")                /* Account Number */
                STRING(INT(ap-sel.check-no),"9999999999")                   /* Check Number   */ 
                STRING(INT(REPLACE(STRING(v-amt-paid,"->>,>>>,>>9.99"),".","")), "9999999999999")  /* Amount         */ 
                IF dtCheckDate NE ? 
                THEN STRING(YEAR(dtCheckDate),"9999") +  
                STRING(MONTH(dtCheckDate),"99")  +     
                STRING(DAY(dtCheckDate),"99")   
                ELSE "00000000"                                          /* Issue Date     */
                FILL(" ",30)                                                /* Additinal data */ 
                IF TRIM(vend.name) NE ""
                THEN TRIM(vend.name) + FILL(" ",(80 - LENGTH(TRIM(vend.name))))
                ELSE FILL(" ",80)                                         /* Payee NAME     */
                IF ap-sel.vend-no EQ "VOID"
                THEN "V" ELSE FILL(" ",1)                                       /* Void Indicator */
                SKIP.
        END.
        ELSE 
        DO: /* excel output */
            /* Output to .csv file */
            PUT STREAM ap-excel UNFORMATTED
                "D" + "," +                                         /* Txn Code */ 
                STRING(INT(ap-sel.bank-code),"999") + "," +         /* Bank code */
                REPLACE(bank.bk-act,"-","") + "," +                 /* Account Number */
                STRING(INT(ap-sel.check-no),"9999999999") + "," +   /* Check Number */
                STRING(v-amt-paid,"->>>>>>>>9.99") + "," +          /* Amount */
                STRING(DATE(dtCheckDate),"99/99/9999") + "," +      /* Check Date */
                vend.name + "," +                                   /* Payee NAME */
                IF ap-sel.vend-no EQ "VOID" THEN "V" ELSE " "       /* Void Indicator */
                SKIP .
        END.

    END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PositivePay-knight C-Win 
PROCEDURE PositivePay-knight :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes: gdm - 05210901
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-check-date AS DATE NO-UNDO .
    IF tb_APcheckFile THEN 
    DO:
 
        FIND FIRST bank NO-LOCK
            WHERE bank.company EQ cocode
            AND bank.bank-code EQ ap-sel.bank-code NO-ERROR.

        v-check-date = IF ap-sel.man-check THEN ap-sel.pre-date ELSE ap-sel.check-date .
        v-check-date = v-check-date + iAPCheckFile.

        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&frame-name} EQ "Text" THEN 
        DO:
            /* Output to .txt file */
            PUT STREAM checkFile UNFORMATTED
                IF ap-sel.vend-no EQ "VOID"
                THEN "V" ELSE "I"      ","                            /* Void Indicator */
                STRING(INT(REPLACE(bank.bk-act,"-","")), "9999999999") ","    /* Account Number */
                STRING(INT(ap-sel.check-no))               ","            /* Check Number   */
                IF TRIM(vend.name) NE ""                                  /* Payee NAME     */
                THEN TRIM(vend.name) 
                ELSE FILL(" ",10)                        ","            
                TRIM(STRING(v-amt-paid,"->>>>>>>9.99"))         ","      /* Amount         */  
                IF v-check-date NE ?                                      /* Issue Date     */
                THEN STRING(v-check-date,"99/99/9999")   
                ELSE "00000000"                                        
                FILL(" ",30)                                              /* Additinal data */ 
                SKIP.
        END.
        ELSE 
        DO: /* ap excel output */
            /* Output to .csv file */
            PUT STREAM ap-excel UNFORMATTED
                IF ap-sel.vend-no EQ "VOID" THEN "V" ELSE "I" + "," +   /* Void Indicator */
                STRING(DECIMAL(REPLACE(bank.bk-act,"-","")), "99999999999999") + "," + /* Account Number */
                STRING(INT(ap-sel.check-no)) + "," +                    /* Check Number   */
                vend.name + "," +                                      /* Vendor Name */  
                STRING(v-amt-paid,"->>>>>>>9.99") + "," +               /* Amount */
                STRING(v-check-date,"99/99/9999")                       /* Issue Date */
                SKIP .
        END.
    END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PositivePay-Santander C-Win 
PROCEDURE PositivePay-Santander :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes: 
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtCheckDate AS DATE NO-UNDO.
    dtCheckDate = ap-sel.check-date + iAPCheckFile.
    IF tb_APcheckFile THEN 
    DO:

        FIND FIRST bank NO-LOCK
            WHERE bank.company EQ cocode
            AND bank.bank-code EQ ap-sel.bank-code /*"6017"*/ NO-ERROR.
        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&frame-name} EQ "Text" THEN 
        DO:
            /* Output to .txt file */
            PUT STREAM checkFile UNFORMATTED
                "6217    6017          "
                STRING(DECIMAL(REPLACE(bank.bk-act,"-","")), "99999999999999999")   /* Account Number */
                STRING(INT(ap-sel.check-no),"9999999999")                   /* Check Number   */ 
                STRING(INT(REPLACE(STRING(v-amt-paid,"->>,>>>,>>9.99"),".","")), "9999999999") /* Amount         */ 
                IF dtCheckDate NE ? 
                THEN SUBSTRING(STRING(YEAR(dtCheckDate),"9999"),3,2) +  
                STRING(MONTH(dtCheckDate),"99")  +     
                STRING(DAY(dtCheckDate),"99")   
                ELSE "000000"                                          /* Issue Date     */
                IF TRIM(vend.vend-no) NE ""
                THEN TRIM(vend.vend-no) + FILL(" ",(15 - LENGTH(TRIM(vend.vend-no))))
                ELSE FILL(" ",15)                                         /* Payee NAME     */
                FILL(" ",40)                                                /* Region/Dept Name */ 
                IF TRIM(vend.name) NE ""
                THEN TRIM(vend.name) + FILL(" ",(80 - LENGTH(TRIM(vend.name))))
                ELSE FILL(" ",80)                                         /* Payee NAME     */
                SKIP.
        END.
        ELSE 
        DO: /* excel output */
            /* Output to .txt file */
            PUT STREAM ap-excel UNFORMATTED
                "6217    6017" + "," +                              /* Bank Number    */
                STRING(DECIMAL(REPLACE(bank.bk-act,"-","")), "99999999999999") + "," + /* Account Number   */
                STRING(INT(ap-sel.check-no),"9999999999") + "," +   /* Check Number     */
                STRING(v-amt-paid,"->>>>>>>9.99") + "," +           /* Amount */
                STRING(DATE(dtCheckDate),"99/99/9999") + "," +      /* Issue Date     */
                vend.vend-no + "," +                                /* Vend no */
                " " + "," +                                         /* Filler (Region) */
                vend.name                                           /* Payee NAME    */
                SKIP .
        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-ap-pay FOR ap-pay.  

    DEFINE VARIABLE lv-check-no LIKE ap-chk.check-no NO-UNDO.

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    /*DO TRANSACTION:*/
    FOR EACH tt-post WHERE tt-post.actnum NE "",
        FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK
        BREAK BY tt-post.actnum:

        ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum). 

        IF LAST-OF(tt-post.actnum) THEN 
        DO:
            RUN GL_SpCreateGLHist(cocode,
                tt-post.actnum,
                "APCKR",
                "AP CHECK REGISTER CURRENCY GAIN/LOSS",
                tran-date,
                (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid),
                v-trnum,
                tran-period,
                "A",
                tran-date,
                "Vendor:" + string(ap-sel.vend-no,"x(8)") + " Inv:" + STRING(ap-sel.inv-no,"99999999"),
                "AP").

            RUN GL_SpCreateGLHist(cocode,
                tt-post.actnum,
                "APCKR",
                "AP CHECK REGISTER CURRENCY GAIN/LOSS",
                tran-date,
                - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid),
                v-trnum,
                tran-period,
                "A",
                tran-date,
                "Vendor:" + string(ap-sel.vend-no,"x(8)") + " Inv:" + STRING(ap-sel.inv-no,"99999999"),
                "AP").
        END.
    END.

    FOR EACH tt-post,
        FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id
        EXCLUSIVE-LOCK,
        FIRST ap-chk EXCLUSIVE-LOCK
        WHERE ap-chk.company   EQ ap-sel.company
        AND ap-chk.check-no  EQ ap-sel.check-no,
        FIRST ap-inv EXCLUSIVE-LOCK
        WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,
        FIRST vend EXCLUSIVE-LOCK
        WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
        USE-INDEX vend
        BREAK BY ap-sel.bank-code
        BY ap-sel.check-no:

        IF ap-sel.man-check AND ap-sel.check-date EQ ? THEN ASSIGN 
                ap-sel.check-date = ap-sel.pre-date.

        ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.check-no).
        ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.bank-code).

        IF FIRST-OF(ap-sel.check-no) THEN i = 0.

        ASSIGN 
            lv-check-no = ap-sel.check-no.

        IF NOT ap-sel.man-check AND tb_void THEN 
        DO:
            FIND LAST ap-pay NO-LOCK
                WHERE ap-pay.company   EQ ap-sel.company
                AND ap-pay.check-act EQ ap-sel.actnum
                AND ap-pay.check-no  LT ap-sel.check-no
                NO-ERROR.
            ASSIGN 
                lv-check-no = (IF AVAILABLE ap-pay THEN ap-pay.check-no ELSE 0) + 1.
        END.

        DO lv-check-no = lv-check-no TO ap-sel.check-no:
            FIND FIRST ap-pay
                WHERE ap-pay.company   EQ ap-sel.company
                AND ap-pay.check-act EQ ap-sel.actnum
                AND ap-pay.check-no  EQ lv-check-no
                NO-ERROR.

            IF NOT AVAILABLE ap-pay THEN 
            DO:
                FIND LAST ap-pay 
                    USE-INDEX c-no NO-ERROR.
                ASSIGN 
                    x = IF AVAILABLE ap-pay THEN ap-pay.c-no ELSE 0.

                CREATE ap-pay.
                ASSIGN
                    ap-pay.company          = cocode
                    ap-pay.check-act        = ap-sel.actnum
                    ap-pay.check-no         = lv-check-no
                    ap-pay.period           = tran-period
                    ap-pay.c-no             = x + 1
                    ap-pay.vend-no          = ap-sel.vend-no
                    ap-pay.bank-code        = ap-sel.bank-code
                    ap-pay.transactionDate  = tran-date
                    .

                IF ap-pay.check-no NE ap-sel.check-no THEN
                    ASSIGN
                        ap-pay.posted  = YES
                        ap-pay.d-no    = ap-sel.check-no
                        ap-pay.cleared = NO.
            END.
        END.

        IF ap-sel.man-check THEN ASSIGN
                ap-pay.check-date = ap-sel.pre-date
                ap-pay.man-check  = YES.
        ELSE ASSIGN 
                ap-pay.check-date = ap-sel.check-date.

        ASSIGN 
            ap-pay.posted = YES.
        
        IF ap-pay.check-date EQ ? THEN ASSIGN 
                ap-pay.check-date = TODAY.

        FOR EACH b-ap-pay EXCLUSIVE WHERE 
            b-ap-pay.company EQ ap-pay.company AND 
            b-ap-pay.d-no    EQ ap-pay.check-no AND 
            NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ b-ap-pay.c-no)
            USE-INDEX d-no:
            ASSIGN 
                b-ap-pay.check-date = ap-pay.check-date.
        END.

        FIND LAST ap-payl WHERE 
            ap-payl.c-no EQ ap-pay.c-no 
            USE-INDEX c-no NO-ERROR.
        ASSIGN 
            i = IF AVAILABLE ap-payl THEN ap-payl.line ELSE 0.

        CREATE ap-payl.
        ASSIGN
            ap-payl.posted    = YES
            ap-payl.c-no      = ap-pay.c-no
            ap-payl.check-no  = ap-sel.check-no
            ap-payl.line      = i + 1
            ap-payl.inv-no    = ap-sel.inv-no
            ap-payl.due-date  = ap-sel.due-date
            ap-payl.amt-disc  = ap-sel.disc-amt
            ap-payl.amt-paid  = ap-sel.amt-paid
            ap-payl.vend-no   = ap-sel.vend-no
            ap-payl.man-check = ap-sel.man-check
            ap-payl.actnum    = ap-sel.actnum.

        ASSIGN
            ap-inv.paid       = ap-inv.paid + ap-sel.amt-paid
            ap-inv.check-no   = ap-sel.check-no
            ap-inv.pay-date   = ap-sel.check-date
            ap-inv.disc-taken = ap-inv.disc-taken + ap-sel.disc-amt
            ap-inv.due        = (ap-inv.net + ap-inv.freight) -
                          ap-inv.paid - ap-inv.disc-taken
            ap-payl.amt-due   = ap-inv.due.

        ASSIGN 
            vend.acc-bal = vend.acc-bal - ap-sel.disc-amt - ap-sel.amt-paid.

        IF ap-inv.due LE 0 
            AND ap-inv.pay-date NE ? 
            AND ap-inv.inv-date NE ? THEN ASSIGN
                vend.avg-pay = ((vend.avg-pay * vend.num-inv) +
                            (ap-inv.pay-date - ap-inv.inv-date)) /
                            (vend.num-inv + 1)
                vend.num-inv = vend.num-inv + 1. /* number of invoices paid */

        FIND FIRST bank EXCLUSIVE WHERE
            bank.company   = cocode AND
            bank.bank-code = ap-sel.bank-code
            NO-ERROR.

        IF AVAILABLE bank THEN ASSIGN 
                bank.bal = bank.bal - ap-sel.amt-paid.

        IF LAST-OF(ap-sel.check-no) THEN 
        DO:
            ASSIGN
                vend.last-pay    = ap-sel.check-date
                ap-pay.check-amt = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
                vend.lpay        = ap-pay.check-amt
                vend.lpay-date   = ap-sel.check-date.

            CREATE ap-ledger.
            ASSIGN
                ap-ledger.company  = ap-sel.company
                ap-ledger.vend-no  = ap-sel.vend-no
                ap-ledger.refnum   = "AC" + STRING(ap-sel.check-no, "999999")
                ap-ledger.ref-date = ap-sel.check-date
                ap-ledger.tr-date  = tran-date
                ap-ledger.trnum    = v-trnum
                ap-ledger.period   = tran-period
                ap-ledger.amt      = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
                ap-ledger.actnum   = wcash.
            RELEASE ap-ledger.
        END.

        /*** Moved gltrans create here so bank's actnum can be used ***/
        IF LAST-OF(ap-sel.bank-code) THEN 
        DO:
            RUN GL_SpCreateGLHist(cocode,
                bank.actnum,
                "APCKR",
                "AP CHECK REGISTER",
                tran-date,
                - ACCUM TOTAL BY ap-sel.bank-code ap-sel.amt-paid,
                v-trnum,
                tran-period,
                "A",
                tran-date,
                "Vendor:" + string(ap-inv.vend-no,"x(8)") + " Inv:" + STRING(ap-inv.inv-no,"99999999"),
                "AP").
        END.

        DELETE ap-sel.

        IF NOT CAN-FIND(FIRST ap-sel WHERE 
            ap-sel.company   EQ ap-chk.company AND 
            ap-sel.check-no  EQ ap-chk.check-no AND 
            ap-sel.man-check EQ ap-chk.man-check) THEN 
            DELETE ap-chk.
    END. /* for each tt-post */

    RELEASE vend.
    RELEASE bank.
    RELEASE ap-pay.
    RELEASE ap-payl.
    RELEASE ap-inv.

    FOR EACH ap-sel EXCLUSIVE WHERE 
        ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.man-check    EQ post-manual
        AND TRIM(ap-sel.inv-no) LE "":
        DELETE ap-sel.
    END.

    FIND FIRST ap-ctrl NO-LOCK WHERE 
        ap-ctrl.company EQ cocode 
        NO-ERROR.

    RUN GL_SpCreateGLHist(cocode,
        ap-ctrl.payables,
        "APCKR",
        "AP CHECK REGISTER",
        tran-date,
        (gtot1 + gtot2),
        v-trnum,
        tran-period,
        "A",
        tran-date,
        "",
        "AP").
  

    IF gtot1 NE 0 THEN 
    DO:
        RUN GL_SpCreateGLHist(cocode,
            ap-ctrl.discount,
            "APCKR",
            "AP CHECK REGISTER",
            tran-date,
            - gtot1,
            v-trnum,
            tran-period,
            "A",
            tran-date,
            "",
            "AP").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */

    /* Start bank transmittal file generation */
    DEFINE VARIABLE cAPIID         AS CHARACTER NO-UNDO INITIAL "CheckTransfer".
    DEFINE VARIABLE cTriggerID     AS CHARACTER NO-UNDO INITIAL "TransmitBankFile".
    DEFINE VARIABLE cFTPType       AS CHARACTER NO-UNDO INITIAL "Generic".
    DEFINE VARIABLE cFTPCode       AS CHARACTER NO-UNDO INITIAL "CheckTransfer". 
    DEFINE VARIABLE cFTPPartner    AS CHARACTER NO-UNDO.       
    DEFINE VARIABLE cFTPScriptFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArgValues     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBankCode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdFTPProcs     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lcResponseXML  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cNotesMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempPath      AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE lCreated       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-line-amt     LIKE ap-invl.amt NO-UNDO.
    DEFINE VARIABLE v-frgt-amt     LIKE ap-inv.freight NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPFileName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPosPayProc    AS CHARACTER NO-UNDO.
    
    RUN system/ftpProcs.p PERSISTENT SET hdFTPProcs.
    
    IF tbTransmitFile:CHECKED IN FRAME {&FRAME-NAME} THEN 
    DO:
        cArgValues = cBankTransmitFileName + "," + cBankTransmitFullFilePath.
        /* Step 1. Creating bank trasmit file */
        RUN ap/APExportPayablesAdvFormat.p (
            INPUT  cocode, 
            INPUT  post-manual, 
            INPUT  cBankTransmitFullFilePath + cBankTransmitFileName,
            OUTPUT cBankCode,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.

        /* Call outbound API "CheckTransfer" to FTP the file */
        IF NOT ERROR-STATUS:ERROR AND lSuccess 
            AND cBankCode NE "" THEN 
        DO:
            ASSIGN 
                cFTPPartner = cBankCode.
            
            /* Copy the file generated to lcRequestData, which in turn will be saved to APIOutbound Request Data */
            COPY-LOB FROM FILE cBankTransmitFullFilePath + cBankTransmitFileName TO lcRequestData.

            /* Step 2. Call to "CheckTransfer" API with "TransmitBankFile" trigger. This will initiate the 
               FTP transfer for the check file generated. Once executed an Outbound Event record will be generated */
            RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                INPUT  cocode,                  /* Company Code (Mandatory) */
                INPUT  g_loc,                   /* Location Code (Mandatory) */
                INPUT  cAPIID,                  /* API ID (Mandatory) */
                INPUT  cBankCode,               /* Client ID (Optional) - Pass empty in case to make request for all clients */
                INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
                INPUT  "LocalFileName,LocalFilePath", /* Comma separated list of table names for which data being sent (Mandatory) */
                INPUT  cArgValues,              /* Comma separated list of Values for the respective key */
                INPUT  cBankCode,               /* Primary ID for which API is called for (Mandatory) */   
                INPUT  "FTP File Transfer",     /* Event's description (Optional) */
                OUTPUT lSuccess,                /* Success/Failure flag */
                OUTPUT cMessage                 /* Status message */
                ) NO-ERROR.
            
            IF lSuccess THEN 
            DO:
                RUN Outbound_GetEvents IN hdOutboundProcs (
                    OUTPUT TABLE ttAPIOutboundEvent
                    ) NO-ERROR.

                FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.                
                IF AVAILABLE ttAPIOutboundEvent THEN 
                DO:
                    /* Step 3. Fetch the file name of the script from ftpConfig, in which FTP file transfer status is available */
                    RUN FTP_GetScriptName IN hdFTPProcs (
                        INPUT  cocode,
                        INPUT  cFTPType,
                        INPUT  cFTPCode,
                        INPUT  cFTPPartner,
                        INPUT  cBankTransmitSysCtrlName,
                        OUTPUT cFTPScriptFile
                        ).
                    
                    /* Step 4. Reading the FTP transfer status xml */
                    RUN FTP_GetResponse IN hdFTPProcs (
                        INPUT  cFTPScriptFile,
                        OUTPUT lcResponseXML,
                        OUTPUT lSuccess,
                        OUTPUT cMessage
                        ).
                    
                    cNotesMessage = IF lSuccess THEN
                        "FTP Transfer Status - SUCCESS - Check file transferred successfully"
                        ELSE
                        "FTP Transfer Status - SUCCESS - " + cMessage
                        .
                                    
                    /* Step 5. Update the Outbound event's request data, response data, error message and transfer status */
                    RUN api/UpdateAPIOutboundEvent.p (
                        INPUT ttAPIOutboundEvent.apiOutboundEventID,
                        INPUT lcRequestData,
                        INPUT lcResponseXML,    
                        INPUT cNotesMessage,                                                                       
                        INPUT lSuccess,
                        INPUT cMessage
                        ).
                END.
                MESSAGE "FTP transfer for check file " + cBankTransmitFullFilePath + cBankTransmitFileName
                    "is " + TRIM(STRING(lSuccess, "successful/failed")) + ". View log files for more information." VIEW-AS ALERT-BOX.
            END.
        END.           
    END.

    DELETE OBJECT hdFTPProcs.    
    /* End bank transmittal file generation */

    ASSIGN
        time_stamp = STRING(TIME, "hh:mmam")
        tmpstore   = FILL("_",125).

    {sys/form/r-top3w.f}

    FORMAT HEADER
        "Check    Vendor"
        "Invoice                                 Discount" AT 55
        "   Check   Pre-Issued  Delivery  " AT 110 SKIP
        "Number   Number   Name       Check Date"
        "Date      Number                  Due      Taken" AT 55
        "Amt Paid   Date        Method"       AT 110 SKIP
        FILL("_",140) FORMAT "x(140)"      SKIP
        WITH FRAME f-top WIDTH 152 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO.
    
    FORMAT HEADER
        "Check    Vendor"
        "Invoice                                           Discount" AT 55
        "   Check   Pre-Issued Delivery " AT 120 SKIP
        "Number   Number   Name       Check Date"
        "Date      Number                            Due      Taken" AT 55
        "Amt Paid   Date       Method"       AT 120 SKIP
        FILL("_",150) FORMAT "x(150)"      SKIP
        WITH FRAME f-top3 WIDTH 162 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO.


    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "A/P CHECKS REGISTER " + STRING(v-trnum)
        str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
        x        = (112 - LENGTH(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - LENGTH(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - LENGTH(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3
        gtot0    = 0
        gtot1    = 0
        gtot2    = 0
        ctr      = 0.

    EMPTY TEMP-TABLE tt-post.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        excelheader = "Check Number,Vendor Number,Name,Check Date,Invoice Date,"
            + "Number,Due,Discount Taken,Check Amt Paid,Pre-Issued Date,Delivery Method".
        PUT STREAM excel UNFORMATTED 
            excelheader SKIP.
    END.

    /* gdm - 05210901 */
    IF tb_APcheckFile THEN 
    DO:
        IF v-fileFormat EQ "Positive Pay" THEN
            excelheader = "Bank No,Bank Code,Account Number,Check Number,Amount,Issue Date,Payee Name,Void Indicator" .
        IF v-fileFormat EQ "Positive Pay-Santander" THEN
            excelheader = "Fixed Code,Account Number,Check Number,Amount,Issue Date,VendorID,Filler,Payee Name" .
        ELSE IF v-fileFormat EQ "StandardCSV" THEN
                excelheader = "Check Number,Vendor Name,Check Date,Check Amount Paid" .
            ELSE
                excelheader = "Void Indicator,Account Number,Check Number,Payee Name,Amount,Issue Date" .

        IF rd_print-apfile EQ "Text" THEN   /* One of three positive pay text formats */
        DO:
            cTempPath =  SUBSTRING(fi_CheckFile,1,R-INDEX(fi_CheckFile,"\") - 1) .
            RUN FileSys_CreateDirectory(INPUT  cTempPath,
                OUTPUT lCreated,
                OUTPUT cMessage
                ) NO-ERROR.
            OUTPUT STREAM checkFile TO VALUE(fi_CheckFile).
        END.    
        ELSE    /* CSV format chosen */
        DO:
            RUN sys/ref/ExcelNameExt.p (INPUT fi_CheckFile,OUTPUT cAPFileName) .
            OUTPUT STREAM ap-excel TO VALUE(cAPFileName).
            /* Display the header values in the .csv */ 
            PUT STREAM ap-excel UNFORMATTED excelheader SKIP.
        END.
    END. /* gdm - 05210901 end */

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.
    IF lAPInvoiceLength THEN DISPLAY "" WITH FRAME f-top3.
    ELSE DISPLAY "" WITH FRAME f-top.
                      
    FOR EACH ap-sel NO-LOCK WHERE 
        ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.check-no     GT 0
        AND ap-sel.man-check    EQ post-manual
        AND TRIM(ap-sel.inv-no) GT ""
        AND CAN-FIND(FIRST ap-chk
        WHERE ap-chk.company   EQ ap-sel.company
        AND ap-chk.check-no  EQ ap-sel.check-no
        AND ap-chk.man-check EQ ap-sel.man-check),

        FIRST ap-inv NO-LOCK WHERE 
        ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,

        FIRST vend NO-LOCK WHERE 
        vend.company EQ ap-sel.company
        AND vend.vend-no EQ ap-sel.vend-no

        BREAK BY ap-sel.check-no 
        WITH STREAM-IO WIDTH 142 NO-BOX NO-ATTR-SPACE NO-LABELS:

        IF FIRST-OF(ap-sel.check-no) THEN v-fst-chk = YES.
        IF LAST-OF(ap-sel.check-no)  THEN V-lst-chk = YES.

        ASSIGN 
            cBankCode = ap-sel.bank-code .
  
        IF ap-sel.vend-no EQ "VOID" THEN 
        DO:
            DISPLAY 
                ap-sel.check-no    FORMAT "zzzzzzz9"
                ap-sel.vend-no
                SKIP(1)
                WITH FRAME vv NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO.

            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    STRING(ap-sel.check-no,"zzzzzzz9") + "," +
                    ap-sel.vend-no
                    SKIP.
            
            /* Output record to Positive Pay file */
            IF tb_APcheckFile THEN 
            DO:
                ASSIGN 
                    cPosPayProc = REPLACE(v-fileFormat," ","").
                RUN VALUE(cPosPayProc).
            END.  
            ASSIGN 
                v-amt-paid = 0.

            NEXT.
        END.

        CREATE tt-post.
        ASSIGN
            tt-post.row-id    = ROWID(ap-sel)
            tt-post.curr-bal  = ap-sel.inv-bal
            tt-post.curr-disc = ap-sel.disc-amt
            tt-post.curr-paid = ap-sel.amt-paid.

        RELEASE currency.
        IF lv-comp-curr NE "" 
            AND lv-comp-curr NE ap-inv.curr-code[1] THEN FIND FIRST currency NO-LOCK WHERE 
                currency.company     EQ ap-inv.company
                AND currency.c-code      EQ ap-inv.curr-code[1]
                AND currency.ar-ast-acct NE ""
                AND currency.ex-rate     GT 0
                NO-ERROR.

        IF AVAILABLE currency THEN ASSIGN
                tt-post.actnum    = currency.ar-ast-acct
                tt-post.ex-rate   = currency.ex-rate
                tt-post.curr-disc = tt-post.curr-disc * tt-post.ex-rate
                tt-post.curr-paid = tt-post.curr-paid * tt-post.ex-rate
                tt-post.curr-bal  = tt-post.curr-bal  * tt-post.ex-rate.

        v-postable = YES.

        IF v-fst-chk THEN
        DO:
            IF lAPInvoiceLength THEN DISPLAY 
                    TRIM(STRING(ap-sel.check-no,">>>>>>>>")) FORMAT "x(8)"
                    vend.vend-no
                    vend.name
                    SPACE(6)
                    WITH FRAME a STREAM-IO.
            ELSE DISPLAY 
                    TRIM(STRING(ap-sel.check-no,">>>>>>>>")) FORMAT "x(8)"
                    vend.vend-no
                    vend.name
                    SPACE(6)
                    WITH FRAME a1 STREAM-IO.
    
            IF tb_excel THEN
            DO:
                FIND FIRST ap-chk WHERE
                    ap-chk.company  EQ ap-sel.company AND
                    ap-chk.check-no EQ ap-sel.check-no
                    NO-LOCK NO-ERROR.

                PUT STREAM excel UNFORMATTED
                    TRIM(STRING(ap-sel.check-no,">>>>>>>>")) + "," +
                    vend.vend-no + "," +
                    vend.NAME + "," +
                    STRING(ap-chk.check-date,"99/99/9999") + ","
                    .
            END.
        END.
        ELSE IF tb_excel THEN /* No header on second or later checks */
                PUT STREAM excel UNFORMATTED
                    " ".

        IF lAPInvoiceLength THEN DISPLAY 
            ap-inv.inv-date           FORMAT "99/99/99"
            SPACE(2)
            ap-sel.inv-no
            tt-post.curr-bal  TO 101
            tt-post.curr-disc TO 112
            tt-post.curr-paid TO 127
            WITH FRAME a STREAM-IO WIDTH 152 NO-LABELS NO-BOX.
        ELSE DISPLAY 
            ap-inv.inv-date           FORMAT "99/99/99"
            SPACE(2)
            ap-sel.inv-no
            tt-post.curr-bal  TO 91
            tt-post.curr-disc TO 102
            tt-post.curr-paid TO 117
            WITH FRAME a1 STREAM-IO WIDTH 142 NO-LABELS NO-BOX.

        IF tb_excel THEN PUT STREAM excel UNFORMATTED
                STRING(ap-inv.inv-date,"99/99/9999") + "," +
                ap-sel.inv-no + "," +
                STRING(tt-post.curr-bal,"->>>>>>>9.99") + "," +
                STRING(tt-post.curr-disc,"->>>>9.99 ") + "," +
                STRING(tt-post.curr-paid,"->>>>>>>9.99").

        IF ap-sel.man-check THEN
        DO:
            IF lAPInvoiceLength THEN DISPLAY 
                    ap-sel.pre-date TO 140 WITH FRAME a.
            ELSE DISPLAY 
                    ap-sel.pre-date TO 130 WITH FRAME a1.
            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    STRING(ap-sel.pre-date,"99/99/9999").
        END.
             
        IF lAPInvoiceLength THEN DISPLAY 
           ap-sel.deliveryMethod TO 151 WITH FRAME a.
        ELSE DISPLAY 
           ap-sel.deliveryMethod TO 142 WITH FRAME a1.
        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
              STRING(ap-sel.deliveryMethod).

        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED SKIP.

        ASSIGN
            tot0 = tot0 + tt-post.curr-bal 
            tot1 = tot1 + tt-post.curr-disc
            tot2 = tot2 + tt-post.curr-paid.

        ASSIGN 
            v-amt-paid = v-amt-paid + ap-sel.amt-paid.

        IF v-lst-chk THEN 
        DO:
            FIND FIRST ap-chk NO-LOCK WHERE 
                ap-chk.company  EQ ap-sel.company
                AND ap-chk.check-no EQ ap-sel.check-no
                NO-ERROR.
            IF lAPInvoiceLength THEN DISPLAY 
                    ap-chk.check-date AT 30     FORMAT "99/99/99"
                    "** CHECK TOTAL"  AT 51
                    tot0              TO 101
                    tot1              TO 112
                    tot2              TO 127 "*"
                    SKIP(1)
                    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME ctot WIDTH 142 STREAM-IO.
            ELSE DISPLAY 
                    ap-chk.check-date AT 30     FORMAT "99/99/99"
                    "** CHECK TOTAL"  AT 51
                    tot0              TO 91
                    tot1              TO 102
                    tot2              TO 117 "*"
                    SKIP(1)
                    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME ctot1 WIDTH 132 STREAM-IO.

            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    SKIP(1)
                    "" + "," +
                    "" + "," +
                    "" + "," +
                    "" + "," +
                    "** CHECK TOTAL" + "," +
                    "" + "," +
                    STRING(tot0,"->>>>>>>9.99") + "," +
                    STRING(tot1,"->>>>9.99 ") + "," +
                    STRING(tot2,"->>>>>>>9.99") + "," +
                    "*"
                    SKIP(1).


            IF tb_APcheckFile THEN 
            DO:
                ASSIGN 
                    cPosPayProc = REPLACE(v-fileFormat," ","").
                RUN VALUE(cPosPayProc).
            END.  
                  
            ASSIGN 
                v-amt-paid = 0
                ctr        = ctr + 1
                gtot0      = gtot0 + tot0
                gtot1      = gtot1 + tot1
                gtot2      = gtot2 + tot2
                tot0       = 0
                tot1       = 0
                tot2       = 0
                v-amt-paid = 0.
        END. /* v-lst-chk */

        ASSIGN
            v-fst-chk = NO
            v-lst-chk = NO.

        ACCUM ap-sel.disc-amt (TOTAL).
        ACCUM ap-sel.amt-paid (TOTAL).
    END. /* each ap-sel */
    
    IF lAPInvoiceLength THEN DISPLAY 
            "*** GRAND TOTALS ***" AT 50
            gtot0                  TO 101
            gtot1                  TO 112
            gtot2                  TO 127 "**" SKIP (1)
            "NUMBER OF CHECKS WRITTEN " ctr
            WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME b WIDTH 142 STREAM-IO.
    ELSE DISPLAY 
            "*** GRAND TOTALS ***" AT 50
            gtot0                  TO 91
            gtot1                  TO 102
            gtot2                  TO 117 "**" SKIP (1)
            "NUMBER OF CHECKS WRITTEN " ctr
            WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME b1 WIDTH 132 STREAM-IO.

    IF tb_APcheckFile THEN 
    DO:
        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&frame-name} EQ "Text" THEN
            OUTPUT STREAM checkFile CLOSE.
        ELSE 
            OUTPUT STREAM ap-excel CLOSE.
    END.

    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            "" + "," +
            "" + "," +
            "" + "," +
            "" + "," +
            "*** GRAND TOTALS ***" + "," +
            "" + "," +
            STRING(gtot0,"->>>>>>>9.99") + "," +
            STRING(gtot1,"->>>>9.99 ") + "," +
            STRING(gtot2,"->>>>>>>9.99") + "," +
            "**"
            SKIP.

    ASSIGN   /* For posting without currency exchange rate */
        gtot1 = (ACCUM TOTAL ap-sel.disc-amt)
        gtot2 = (ACCUM TOTAL ap-sel.amt-paid).

    IF tb_prt-acc THEN 
    DO:
        HIDE FRAME f-top.
        HIDE FRAME f-top3.

        ASSIGN
            str-tit3 = "Period " + STRING(tran-period,"99") + " - " + "Summary by Account"
            x        = (132 - LENGTH(str-tit3)) / 2
            str-tit3 = FILL(" ",x) + str-tit3.

        PAGE.

        FORM HEADER
            "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#    "
            "LINE DESCRIPTION                 QTY    UNIT PRICE     AMT PAID" SKIP
            FILL("_",135) FORMAT "x(135)"
            WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 135 STREAM-IO.
  
        FORM HEADER
            "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#            "
            "LINE DESCRIPTION                 QTY    UNIT PRICE     AMT PAID" SKIP
            FILL("_",143) FORMAT "x(143)"
            WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top4 PAGE-TOP WIDTH 143 STREAM-IO.

        IF lAPInvoiceLength THEN DISPLAY 
                "" WITH FRAME f-top4.
        ELSE DISPLAY 
                "" WITH FRAME f-top2.

        FOR EACH tt-post NO-LOCK,

            FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

            FIRST ap-inv NO-LOCK
            WHERE ap-inv.company EQ ap-sel.company
            AND ap-inv.vend-no EQ ap-sel.vend-no
            AND ap-inv.inv-no  EQ ap-sel.inv-no
            AND ap-inv.freight NE 0,

            FIRST vend NO-LOCK
            WHERE vend.company EQ ap-inv.company
            AND vend.vend-no EQ ap-inv.vend-no
            USE-INDEX vend

            BREAK BY ap-inv.vend-no:

            IF FIRST(ap-inv.vend-no) THEN 
            DO:
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ ap-inv.company
                    AND account.actnum  EQ v-frt-acct
                    NO-ERROR.
                PUT v-frt-acct + " - " +
                    (IF AVAILABLE account THEN account.dscr ELSE "Not on file")
                    FORMAT "x(40)" SKIP.
            END.

            v-frgt-amt = ap-sel.amt-paid *
                (ap-inv.freight / (ap-inv.net + ap-inv.freight)).

            PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
                SPACE(1)
                ap-inv.vend-no
                SPACE(1).
            
            IF lAPInvoiceLength THEN 
                PUT ap-inv.inv-no FORMAT "x(20)".
            ELSE 
                PUT ap-inv.inv-no FORMAT "x(12)".
            
            PUT SPACE(6)
                "Freight"                       FORMAT "x(18)"
                SPACE(7)
                1.0                             FORMAT "9.9"
                SPACE(1)
                ap-inv.freight          TO 118
                v-frgt-amt              TO 131
                SKIP.

            ACCUM v-frgt-amt (TOTAL).

            IF LAST(ap-inv.vend-no) THEN
                PUT "** TOTAL " TO 114
                    (ACCUM TOTAL v-frgt-amt) FORMAT "->>,>>>,>>9.99" TO 128
                    " *" SKIP(1).
        END.

        FOR EACH tt-post NO-LOCK,

            FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

            FIRST ap-inv NO-LOCK
            WHERE ap-inv.company EQ ap-sel.company
            AND ap-inv.vend-no EQ ap-sel.vend-no
            AND ap-inv.inv-no  EQ ap-sel.inv-no,

            FIRST vend NO-LOCK
            WHERE vend.company EQ ap-inv.company
            AND vend.vend-no EQ ap-inv.vend-no
            USE-INDEX vend,

            EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no USE-INDEX i-no NO-LOCK

            BREAK BY ap-invl.actnum
            BY ap-invl.inv-no
            BY ap-invl.line

            WITH WIDTH 132 NO-LABELS:

            IF FIRST-OF(ap-invl.actnum) THEN 
            DO:
                FIND FIRST account
                    WHERE account.company EQ ap-inv.company
                    AND account.actnum  EQ ap-invl.actnum
                    NO-LOCK NO-ERROR.

                PUT ap-invl.actnum + " - " +
                    (IF AVAILABLE account THEN account.dscr ELSE "Not on file")
                    FORMAT "x(40)" SKIP.
            END.

            v-line-amt = ap-sel.amt-paid * (ap-invl.amt / (ap-inv.net + ap-inv.freight)).

            PUT ap-invl.po-no         AT 34
                SPACE(1)
                ap-inv.inv-date       FORMAT "99/99/99"
                SPACE(1)
                ap-inv.vend-no
                SPACE(1).
            IF lAPInvoiceLength THEN PUT ap-inv.inv-no FORMAT "x(20)".
            ELSE PUT ap-inv.inv-no FORMAT "x(12)".
            PUT SPACE(1)
                {ap/invlline.i -1}    FORMAT ">>>9"
                SPACE(1)
                ap-invl.dscr          FORMAT "x(18)"
                SPACE(1)
                ap-invl.qty           FORMAT "->,>>>,>>9.9<<"
                SPACE(1)
                ap-invl.unit-pr
                SPACE(1)
                v-line-amt
                SPACE(1)
                SKIP.

            ACCUM v-line-amt (TOTAL BY ap-invl.actnum).
            ACCUM v-line-amt (TOTAL).

            IF LAST-OF(ap-invl.actnum) THEN
                PUT "** TOTAL " TO 114
                    (ACCUM TOTAL BY ap-invl.actnum v-line-amt)
                    FORMAT "->>,>>>,>>9.99" TO 128
                    " *" SKIP(1).
        END.

        FOR EACH tt-post NO-LOCK WHERE tt-post.actnum NE "",

            FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

            FIRST ap-inv NO-LOCK
            WHERE ap-inv.company EQ ap-sel.company
            AND ap-inv.vend-no EQ ap-sel.vend-no
            AND ap-inv.inv-no  EQ ap-sel.inv-no

            BREAK BY tt-post.actnum

            WITH WIDTH 142 NO-LABELS:

            IF FIRST-OF(tt-post.actnum) THEN 
            DO:
                FIND FIRST account
                    WHERE account.company EQ ap-sel.company
                    AND account.actnum  EQ tt-post.actnum
                    NO-LOCK NO-ERROR.

                PUT tt-post.actnum + " - " +
                    (IF AVAILABLE account THEN account.dscr ELSE "Not on file")
                    FORMAT "x(40)" SKIP.
            END.

            PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
                SPACE(1)
                ap-inv.vend-no
                SPACE(1).
            IF lAPInvoiceLength THEN PUT ap-inv.inv-no FORMAT "x(20)".
            ELSE PUT ap-inv.inv-no FORMAT "x(12)".
            PUT SPACE(6)
                "Currency"                      FORMAT "x(18)"
                SPACE(7)
                1.0                             FORMAT "9.9"
                tt-post.curr-paid - ap-sel.amt-paid TO 131
                SKIP.

            ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum).
            ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL).

            IF LAST-OF(tt-post.actnum) THEN
                PUT "** TOTAL " TO 114
                    (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
                    FORMAT "->>,>>>,>>9.99" TO 128
                    " *" SKIP(1).
        END.

        PUT "***** TOTAL for ALL ACCOUNTS " TO 116
            (ACCUM TOTAL v-line-amt) +
            (ACCUM TOTAL v-frgt-amt) +
            (ACCUM TOTAL tt-post.curr-paid - ap-sel.amt-paid)
            FORMAT "->>,>>>,>>9.99" TO 130
            SKIP(2).
    END.
    
    IF tbTransmitFile:CHECKED IN FRAME {&FRAME-NAME} THEN
    DO:   
        ASSIGN 
            cAPIID       = "SendBankCheck"
            cTriggerID   = "PrintCheck"
            cPrimaryID   = "Check " 
            cDescription = cAPIID + " triggered by " + cTriggerID + " from r-chkreg for checks " //+ cPrimaryID
            cDataList    = STRING(post-manual) + ","
                   + STRING(cocode)
            .
               
        RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
            INPUT  cocode,                     /* Company Code (Mandatory) */
            INPUT  locode,                     /* Location Code (Mandatory) */
            INPUT  cAPIID,                     /* API ID (Mandatory) */
            INPUT  cBankCode,                  /* Client ID (Optional) - Pass empty in case to make request for all clients */
            INPUT  cTriggerID,                 /* Trigger ID (Mandatory) */
            INPUT  "PostManual,Company",       /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  cDataList ,                 /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  cPrimaryID,                 /* Primary ID for which API is called for (Mandatory) */   
            INPUT  cDescription,               /* Event's description (Optional) */
            OUTPUT lSuccess,                   /* Success/Failure flag */
            OUTPUT cMessage                    /* Status message */
            ) NO-ERROR.               
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StandardCSV C-Win 
PROCEDURE StandardCSV :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes: gdm - 05210901
        ------------------------------------------------------------------------------*/   
    DEFINE VARIABLE dtCheckDate AS DATE NO-UNDO.
    
    IF tb_APcheckFile THEN 
    DO:
        FIND FIRST ap-chk WHERE
            ap-chk.company  EQ ap-sel.company AND
            ap-chk.check-no EQ ap-sel.check-no
            NO-LOCK NO-ERROR.
        IF ap-chk.check-date NE ? THEN ASSIGN 
                dtCheckDate = ap-chk.check-date .

        IF rd_print-apfile:SCREEN-VALUE IN FRAME {&frame-name} EQ "Text" THEN 
            PUT STREAM checkFile UNFORMATTED
                STRING(INT(ap-sel.check-no),"9999999999") + "," +       /* Check Number     */
                STRING(vend.NAME) + "," +                               /* Vendor Name    */
                STRING(dtCheckDate,"99/99/9999") + "," +                /* Check Date     */
                STRING(ap-sel.amt-paid,"->>>>>>>>9.99")                 /* Check amount paid */
                SKIP .
        ELSE PUT STREAM ap-excel UNFORMATTED
                STRING(INT(ap-sel.check-no),"9999999999") + "," +       /* Check Number     */
                STRING(vend.NAME) + "," +                               /* Vendor Name    */
                STRING(dtCheckDate,"99/99/9999") + "," +                /* Check Date     */
                STRING(ap-sel.amt-paid,"->>>>>>>>9.99")                 /* Check amount paid */
                SKIP .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    /* GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    DEFINE VARIABLE lLocked AS LOG       NO-UNDO.
    DEFINE VARIABLE cUsr    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDevice AS CHARACTER NO-UNDO.


    REPEAT:

        FIND FIRST gl-ctrl NO-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.

        IF AVAILABLE gl-ctrl THEN 
        DO:
            RUN checkLock(INPUT "gl-ctrl",
                INPUT ROWID(gl-ctrl),
                OUTPUT lLocked,
                OUTPUT cUsr,
                OUTPUT cName,
                OUTPUT cDevice).
            IF lLocked THEN 
            DO:
                /* To display standard progress locking message */
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode .
                RELEASE gl-ctrl.
            END.
        /*           MESSAGE "The General Ledger Control Record is locked by " cName SKIP */
        /*                   "Press OK to try again."                                     */
        /*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
        END. /* avail gl-ctrl */
        RELEASE gl-ctrl.
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAILABLE gl-ctrl THEN 
        DO:

            ASSIGN 
                v-trnum       = gl-ctrl.trnum + 1 
                gl-ctrl.trnum = v-trnum.
            FIND CURRENT gl-ctrl NO-LOCK.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */

    END. /* REPEAT */
/* gdm - 11050906 */




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
         Purpose:    
         Parameters:  <none>
         Notes:      
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\APChecksRegister.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

