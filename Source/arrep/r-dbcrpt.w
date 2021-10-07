&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Print Invoices

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

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

/* gdm - 03120909 */
{custom/xprint.i}
{sys/inc/print1.i}

DEFINE            VARIABLE v-program      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-lo-cust      LIKE cust.cust-no INIT "" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-hi-cust      LIKE cust.cust-no INIT "zzzzzzzz" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-lo-memo      LIKE ar-cash.check-no INIT 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-hi-memo      LIKE ar-cash.check-no INIT 99999999 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-reprint      AS LOG       NO-UNDO.

/* gdm - 04210922 */
DEFINE NEW SHARED VARIABLE v-begdt        LIKE ar-cash.check-date NO-UNDO.
DEFINE NEW SHARED VARIABLE v-enddt        LIKE ar-cash.check-date NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tbpst        AS LOG       NO-UNDO.

DEFINE            VARIABLE save_id        AS RECID     NO-UNDO.
DEFINE            VARIABLE time_stamp     AS ch        NO-UNDO.
DEFINE            VARIABLE qfirst         AS l         NO-UNDO.
DEFINE            VARIABLE g1             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE archk          AS DECIMAL   FORMAT ">>>>>>99" NO-UNDO.
DEFINE            VARIABLE t1             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE g2             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE t3             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v1             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v2             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE t2             AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v-on-act-amt   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE big_ul         AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE            VARIABLE big_ul2        AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE            VARIABLE letterhead     AS CHARACTER FORMAT "x(50)" EXTENT 5 NO-UNDO.

DEFINE            VARIABLE v-printlines   AS INTEGER   INIT 0 NO-UNDO.
DEFINE            VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-pdf-file    AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-ftp-done     AS LOG       NO-UNDO.
DEFINE            VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-term-id      AS CHARACTER NO-UNDO.

DEFINE            VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* gdm - 03120909 */
DEFINE VARIABLE v-chrfld AS CHARACTER     NO-UNDO.

/* gdm 03120909 - dont why this is here, its also in MAIN BLOCK - 
redunduncy ? commented out
find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust end_cust begin_memo ~
end_memo begin_date end_date tb_reprint tb_posted tb_export rd-dest ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_memo end_memo ~
begin_date end_date tb_reprint tb_posted tb_export rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win    AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_memo     AS INTEGER   FORMAT ">>>>>>>>>>>>" INITIAL 0 
    LABEL "Beginning Memo#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_memo       AS INTEGER   FORMAT ">>>>>>>>>>>9" INITIAL 99999999 
    LABEL "Ending Memo#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

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
    "To Email", 5,
    "To File", 3
    SIZE 17.4 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 5.52.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 7.76.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_export    AS LOGICAL INITIAL NO 
    LABEL "Export/FTP  Memos?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_posted    AS LOGICAL INITIAL NO 
    LABEL "Reprint Posted Memos?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint   AS LOGICAL INITIAL NO 
    LABEL "Reprint Memos?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust AT ROW 2.24 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 2.24 COL 70 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_memo AT ROW 3.19 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Invoice Number"
    end_memo AT ROW 3.19 COL 70 COLON-ALIGNED HELP
    "Enter Ending Invoice Number"
    begin_date AT ROW 4.14 COL 27 COLON-ALIGNED HELP
    "Enter Beginning AR Memo Date"
    end_date AT ROW 4.14 COL 70 COLON-ALIGNED HELP
    "Enter Ending AR Memo Date"
    tb_reprint AT ROW 5.57 COL 36
    tb_posted AT ROW 6.76 COL 36
    tb_export AT ROW 7.95 COL 36
    lv-font-no AT ROW 10 COL 33 COLON-ALIGNED
    rd-dest AT ROW 10.05 COL 4.6 NO-LABELS
    lv-ornt AT ROW 10.05 COL 43 NO-LABELS
    lines-per-page AT ROW 10.1 COL 87.2 COLON-ALIGNED
    lv-font-name AT ROW 11.19 COL 28.8 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14.1 COL 31
    tbAutoClose AT ROW 15.43 COL 31 WIDGET-ID 64
    btn-ok AT ROW 16.38 COL 30.8
    btn-cancel AT ROW 16.38 COL 52.8
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 9.33 COL 4
    RECT-6 AT ROW 9.76 COL 3
    RECT-7 AT ROW 1.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 17.57
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
        TITLE              = "Print Credit/Debit Memo"
        HEIGHT             = 16.86
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
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_memo:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_memo:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_export:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_export:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_posted:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_posted:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_reprint:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Print Credit/Debit Memo */
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
ON WINDOW-CLOSE OF C-Win /* Print Credit/Debit Memo */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_memo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_memo C-Win
ON LEAVE OF begin_memo IN FRAME FRAME-A /* Beginning Memo# */
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

        ASSIGN {&DISPLAYED-OBJECTS}.

        /* gdm - 03120909 */
        IF IS-xprint-form 
            THEN 
        DO:
            RUN Set-Memo-Form. 
            RUN run-report. 
        END.
        ELSE RUN run-report-old.       

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_cust
                            &end_cust=end_cust
                            &fax-subject="Credit/Debit Memo"
                            &fax-body="Credit/Debit Memo"
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        ASSIGN 
                            lv-pdf-file = list-name.
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust
                             &end_cust=end_cust
                             &mail-subject="Credit/Debit Memo"
                             &mail-body="Credit/Debit Memo"
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust
                                  &end_cust=end_cust
                                  &mail-subject="Credit/Debit Memo"
                                  &mail-body="Credit/Debit Memo"
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
        IF begin_memo = end_memo THEN 
        DO:
            FIND FIRST ar-cash WHERE ar-cash.company = g_company
                AND ar-cash.check-no = begin_memo NO-LOCK NO-ERROR.
            IF AVAILABLE ar-cash THEN ASSIGN begin_cust:SCREEN-VALUE = ar-cash.cust-no
                    end_cust:SCREEN-VALUE   = ar-cash.cust-no.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_memo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_memo C-Win
ON LEAVE OF end_memo IN FRAME FRAME-A /* Ending Memo# */
    DO:
        ASSIGN {&self-name}.
        IF begin_memo = end_memo THEN 
        DO:
            FIND FIRST ar-cash WHERE ar-cash.company = g_company
                AND ar-cash.check-no = begin_memo NO-LOCK NO-ERROR.
            IF AVAILABLE ar-cash THEN ASSIGN begin_cust:SCREEN-VALUE = ar-cash.cust-no
                    end_cust:SCREEN-VALUE   = ar-cash.cust-no.
            /* task 01311304 */
            IF AVAILABLE ar-cash AND ar-cash.posted THEN 
            DO:
                IF ar-cash.printed THEN
                    ASSIGN
                        tb_reprint:SCREEN-VALUE = "YES"
                        tb_posted:SCREEN-VALUE  = "YES".
                ELSE
                    ASSIGN
                        tb_posted:SCREEN-VALUE  = "YES" 
                        tb_reprint:SCREEN-VALUE = "NO".
            END.    
            ELSE 
            DO:
                IF ar-cash.printed THEN
                    tb_reprint:SCREEN-VALUE = "YES".
                ELSE
                    tb_reprint:SCREEN-VALUE = "NO" .
                tb_posted:SCREEN-VALUE  = "NO" . /* task 01311304 */
            END.
        END. 
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Memos? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Memos? */
    DO:
        ASSIGN {&self-name}.
        IF tb_posted THEN ASSIGN tb_export:SENSITIVE     = YES
                tb_reprint:SCREEN-VALUE = "YES".
        ELSE ASSIGN tb_export:SCREEN-VALUE = "NO"
                tb_export:SENSITIVE    = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Memos? */
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.


    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode AND sys-ctrl.name EQ "ARMEMO" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "ARMEMO"
            sys-ctrl.descrip  = "Credit/Debit Memo Form"
            sys-ctrl.char-fld = "".
    END. 

    ASSIGN 
        v-chrfld    = IF AVAILABLE sys-ctrl THEN TRIM(sys-ctrl.char-fld) ELSE "" 
        v-print-fmt = sys-ctrl.char-fld.

    IF AVAILABLE sys-ctrl AND 
        (TRIM(sys-ctrl.char-fld) NE "" AND
        TRIM(sys-ctrl.char-fld) NE "HOP")
        THEN IS-xprint-form = TRUE.  

    DO TRANSACTION:
        {sys/inc/inexport.i}
    END.

    lines-per-page = 58.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AW3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO begin_cust.
        IF v-print-fmt = "frankstn" OR v-print-fmt = "Mirpkg" THEN 
            ASSIGN tb_export:HIDDEN       = NO
                tb_export              = NO
                tb_export:SCREEN-VALUE = "NO"
                tb_export:SENSITIVE    = IF tb_posted:SCREEN-VALUE = "yes" THEN YES ELSE NO .
        ELSE ASSIGN tb_export              = NO
                tb_export:SCREEN-VALUE = "NO"
                tb_export:HIDDEN       = YES.
        DISABLE lines-per-page.
        IF tb_posted:SCREEN-VALUE = 'yes' THEN tb_reprint:SCREEN-VALUE = "YES".
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
    DISPLAY begin_cust end_cust begin_memo end_memo begin_date end_date tb_reprint 
        tb_posted tb_export rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_cust end_cust begin_memo end_memo begin_date 
        end_date tb_reprint tb_posted tb_export rd-dest tbAutoClose btn-ok 
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
                                       /* use-dialog(1) and landscape(2) */
   
     */
    /* gdm - 03120909 */
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE 
        /* gdm - 03120909 end */
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
    /* gdm - 03120909 */
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE 
        /* gdm - 03120909 end */
        RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - O/E MODULE                                                 */
    /* -------------------------------------------------------------------------- */

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    {sa/sa-sls01.i}

    v-term-id = v-term.

    {sys/form/r-top3.f}



    /* gdm - 03120909 */
    IF IS-xprint-form THEN 
    DO:
        CASE rd-dest:
            WHEN 1 THEN 
                PUT  "<PRINTER?></PROGRESS>".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<PREVIEW><MODAL=NO></PROGRESS>".
                    ELSE 
                        PUT "<PREVIEW></PROGRESS>". 
                END.    


            WHEN  4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                    /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
                    PUT UNFORMATTED 
                        "</PROGRESS><PRINTER?><EXPORT=" Ls-fax-file ",BW>".
                END.
            WHEN 5 THEN 
                PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
        END CASE.
    END.


    ASSIGN 
        v-lo-cust = begin_cust
        v-hi-cust = end_cust
        v-lo-memo = begin_memo
        v-hi-memo = end_memo
        v-reprint = tb_reprint
        v-begdt   = begin_date
        v-enddt   = end_date
        v-tbpst   = tb_posted.

    IF v-hi-cust EQ "" THEN v-hi-cust = "zzzzzzzz".

    SESSION:SET-WAIT-STATE("general").

    /* gdm - 03120909 */
    IF IS-xprint-form THEN RUN VALUE(v-program).

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-old C-Win 
PROCEDURE run-report-old :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    DEFINE VARIABLE lv-desc LIKE ar-cashl.dscr NO-UNDO.

    IF td-show-parm THEN RUN show-param.

    {sa/sa-sls01.i}
    v-term-id = v-term.

    {sys/form/r-top3.f}


    FORMAT 
        lv-desc AT 4
        ar-cashl.inv-no AT 55
        ar-cashl.amt-paid TO 78
        WITH FRAME crdb-lines NO-BOX NO-LABELS WIDTH 80 DOWN.


    ASSIGN
        time_stamp = STRING(TIME, "hh:mmam")
        tmpstore   = FILL("_",125)
        big_ul     = FILL("=",80)
        big_ul2    = FILL("=",80).

    ASSIGN 
        v-lo-cust = begin_cust
        v-hi-cust = end_cust
        v-lo-memo = begin_memo
        v-hi-memo = end_memo
        v-reprint = tb_reprint.

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.


    ASSIGN
        letterhead[1] = ""
        letterhead[2] = company.name
        letterhead[3] = company.addr[1]
        letterhead[4] = company.addr[2]
        letterhead[5] = company.city + ", " + company.state + "  " + company.zip.

    DO i = 5 TO 2 BY -1:
        IF letterhead[i - 1] LE " " AND letterhead[i] GE "" THEN
            ASSIGN
                letterhead[i - 1] = letterhead[i]
                letterhead[i]     = "".
    END.
    DO i = 1 TO 5:
        {sys/inc/ctrtext.i letterhead[i] 50}.
    END.


    IF v-hi-cust EQ "" THEN v-hi-cust = "zzzzzzzz".

    IF v-chrfld EQ "HOP" THEN 
    DO:
        RUN ar/rep/memohop.p.
        RETURN.
    END.
    SESSION:SET-WAIT-STATE("general").


    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cocode
        AND ar-cash.posted     EQ tb_posted
        AND ar-cash.memo       EQ YES
        AND ar-cash.cust-no    GE v-lo-cust
        AND ar-cash.cust-no    LE v-hi-cust
        AND ar-cash.check-no   GE v-lo-memo
        AND ar-cash.check-no   LE v-hi-memo
        AND ar-cash.check-date GE begin_date
        AND ar-cash.check-date LE end_date
        AND CAN-FIND(FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no)
        AND ((v-reprint AND
        ar-cash.printed EQ YES) OR
        (NOT v-reprint AND
        ar-cash.printed EQ NO))
        USE-INDEX posted        
        BREAK BY ar-cash.cust-no
        BY ar-cash.check-no WITH FRAME a1:

        IF ar-cash.stat EQ "H" THEN
            NEXT.

        RELEASE reftable NO-ERROR.

        IF CAN-FIND(FIRST ar-cashl WHERE ar-cashl.company = cocode AND
            ar-cashl.c-no = ar-cash.c-no AND
            (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0 )
            THEN 
        DO:
            CREATE report.
            ASSIGN
                report.term-id = v-term-id
                report.key-01  = STRING(ar-cash.check-no,"9999999999")
                report.rec-id  = RECID(ar-cash).
        END.

        IF FIRST-OF(ar-cash.cust-no) THEN 
        DO:
            v-printlines = 1.
            FIND FIRST cust
                {ar/ar-custW.i}
                AND cust.cust-no EQ ar-cash.cust-no
            NO-LOCK NO-ERROR.

            IF FIRST(ar-cash.cust-no) THEN
                FORMAT HEADER
                    SKIP(1)
                    letterhead[1] AT 5 "  Date:" TO 70
                    ar-cash.check-date FORMAT "99/99/99" SKIP
                    letterhead[2] AT 5
                    letterhead[3] AT 5 "Memo #:" TO 70
                    ar-cash.check-no FORMAT ">>>>>>>9" SKIP
                    letterhead[4] AT 5
                    letterhead[5] AT 5
                    SKIP (4)
                    "Customer #:" AT 11 ar-cash.cust-no
                    cust.name              AT 11
                    cust.addr [1]          AT 11
                    cust.addr [2]          AT 11
                    cust.city              AT 11 cust.state cust.zip
                    SKIP(4)
                    big_ul AT 1
                    "********************     CREDIT / DEBIT MEMO     ********************"
                    AT 6
                    big_ul2 AT 1 SKIP(1)
                    "Description" AT 4 "Invoice #" AT 55 "Amount" TO 78
                    "---------------------------------------------" AT 4 "---------" AT 55
                    "--------------" TO 78
                    WITH FRAME crdb-header NO-BOX NO-LABELS WIDTH 80 PAGE-TOP STREAM-IO.

        END.
        VIEW FRAME crdb-header.
        IF FIRST-OF(ar-cash.check-no) THEN PAGE.

        FOR EACH ar-cashl
            WHERE ar-cashl.company EQ cocode
            AND ar-cashl.c-no    EQ ar-cash.c-no
            BREAK BY ar-cashl.line
            WITH FRAME crdb-lines NO-BOX NO-LABELS WIDTH 80 DOWN:

            ASSIGN
                v2      = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc
                lv-desc = ar-cashl.dscr.

            IF tb_posted THEN 
            DO:

                IF lv-desc BEGINS "Credit -" THEN
                    lv-desc = SUBSTR(lv-desc,10).
                ELSE IF lv-desc BEGINS "Debit -" THEN
                        lv-desc = SUBSTR(lv-desc,9).
            END.

            DISPLAY lv-desc ar-cashl.inv-no
                (ar-cashl.amt-paid - ar-cashl.amt-disc) @ ar-cashl.amt-paid
                WITH FRAME crdb-lines STREAM-IO.
            DOWN WITH FRAME crdb-lines.
            v-printlines = v-printlines + 1.
            IF v-printlines GT 26 THEN 
            DO:
                PUT "** CONTINUED **" AT 35.
                PAGE.
                ASSIGN 
                    v-printlines = 0.
            END.      

        END. /* each ar-cashl */

        IF LAST-OF(ar-cash.check-no) THEN 
        DO:
            PUT SKIP(27 - v-printlines) "Memo Amount" TO 78 SKIP v2 TO 78 SKIP.
            ASSIGN
                g1 = g1 + v1
                g2 = g2 + v2
                v1 = 0
                v2 = 0.
        END.

    
        /* gdm 07010903 */
        ASSIGN 
            ar-cash.ret-memo = YES
            ar-cash.printed  = YES.

    END. /* each ar-cash */

    g2 = 0.
    OUTPUT CLOSE.

    v-ftp-done = NO.
    IF tb_export AND inexport-log THEN 
    DO:    
        DEFINE VARIABLE v-exp-file AS CHARACTER NO-UNDO.
        v-exp-file = inexport-desc +  
            "MEMO_" + trim(v-print-fmt) + 
            substr(STRING(YEAR(TODAY),"9999"),3,2) +
            string(MONTH(TODAY),"99") +
            string(DAY(TODAY),"99") +
            substr(STRING(TIME,"HH:MM:SS"),1,2) +
            substr(STRING(TIME,"HH:MM:SS"),4,2) +
            substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".
        OUTPUT TO VALUE(v-exp-file).
        IF inexport-cha EQ "CIT" THEN 
        DO:
            RUN ar/rep/expfmemo.p .
            OUTPUT CLOSE.
            OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
            PUT UNFORMATTED 
                "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
                "ftpa1526" SKIP  /* userid*/
                "none" SKIP  /* password */
                "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
                "quit" .
            OUTPUT CLOSE.
            OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
            v-ftp-done = YES.
        END.
    END.

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Memo-Form C-Win 
PROCEDURE Set-Memo-Form :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE icFormName AS CHARACTER NO-UNDO.

    ASSIGN
        icFormName = v-chrfld.

    CASE icFormName:

        WHEN "AllWest" THEN
            ASSIGN
                v-program      = "ar/rep/AlWstMem.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "PremierPkg" THEN
            ASSIGN
                v-program      = "ar/rep/prpkgmem.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "StdCreditMemo10" THEN
            ASSIGN
                v-program      = "ar/rep/stdcrmemo10.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "SouleMed" THEN
            ASSIGN
                v-program      = "ar/rep/soulmemo.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Soule" THEN
            ASSIGN
                v-program      = "ar/rep/soulpkmem.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Xprint-CAN" THEN
            ASSIGN
                v-program      = "ar/rep/Xprint-CAN.p"
                lines-per-page = 66
                is-xprint-form = YES.
        OTHERWISE
        ASSIGN
            v-program      = "ar/rep/crdbmemo.p"
            is-xprint-form = YES
            lines-per-page = 66.

    END CASE.


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

    PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

