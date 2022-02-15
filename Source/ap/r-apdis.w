&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

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
DEFINE VARIABLE v-invalid    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-postable   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE v-descr      AS CHARACTER FORMAT "x(15)".
DEFINE VARIABLE save_id      AS RECID.
DEFINE VARIABLE time_stamp   AS CHARACTER.
DEFINE VARIABLE qfirst       AS LOGICAL.
DEFINE VARIABLE post         AS LOGICAL   FORMAT "Yes/No"
    LABEL "   Post to G/L & Vendor files?   " INITIAL NO.
DEFINE VARIABLE g1           AS DECIMAL.
DEFINE VARIABLE t1           AS DECIMAL.
DEFINE VARIABLE g2           AS DECIMAL.
DEFINE VARIABLE t3           AS DECIMAL.
DEFINE VARIABLE v1           AS DECIMAL.
DEFINE VARIABLE v2           AS DECIMAL.
DEFINE VARIABLE t2           AS DECIMAL.
DEFINE VARIABLE xtrnum       AS INTEGER.
DEFINE VARIABLE xap-acct     AS CHARACTER.
DEFINE VARIABLE xcs-acct     AS CHARACTER.
DEFINE VARIABLE sort-by-vend AS LOG       INIT YES FORMAT "Vendor/Sequence" NO-UNDO.
DEFINE VARIABLE v-s-date     LIKE ap-pay.check-date FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE v-e-date     LIKE ap-pay.check-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.

DEFINE BUFFER tmp-period FOR period.
DEFINE BUFFER b-bank     FOR bank.
DEFINE VARIABLE v-print-fmt    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 tran-date begin_date ~
end_date rd_sort rd-dest tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_date end_date ~
lbl_sort rd_sort rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win          AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Post Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period    AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5
    SIZE 16 BY 4 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Vendor#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor#", "Vendor#",
    "Sequence", "Sequence"
    SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 4.5.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 8.52.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 67 BY 3.57.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 2.76 COL 42 COLON-ALIGNED
    tran-period AT ROW 3.95 COL 42 COLON-ALIGNED
    begin_date AT ROW 6.76 COL 32 COLON-ALIGNED HELP
    "Enter Beginning AP Date"
    end_date AT ROW 6.76 COL 62 COLON-ALIGNED HELP
    "Enter Ending AP Date"
    lbl_sort AT ROW 8.19 COL 38 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 8.19 COL 51 NO-LABELS
    lv-ornt AT ROW 10.76 COL 30 NO-LABELS
    lines-per-page AT ROW 10.76 COL 83 COLON-ALIGNED
    rd-dest AT ROW 10.76 COL 6 NO-LABELS
    lv-font-no AT ROW 11.71 COL 33 COLON-ALIGNED
    lv-font-name AT ROW 12.67 COL 27 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14.33 COL 31
    tbAutoClose AT ROW 15.1 COL 31 WIDGET-ID 58
    btn-ok AT ROW 16 COL 31
    btn-cancel AT ROW 16 COL 51
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 10.19 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " DATE RANGE AND SORTING ORDER" VIEW-AS TEXT
    SIZE 39 BY .62 AT ROW 5.76 COL 29
    FGCOLOR 9 FONT 6
    RECT-6 AT ROW 10.57 COL 4
    RECT-7 AT ROW 1.52 COL 4
    RECT-8 AT ROW 6.05 COL 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 17
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
        TITLE              = "Cash Disbursements Register"
        HEIGHT             = 17
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Cash Disbursements Register */
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
ON WINDOW-CLOSE OF C-Win /* Cash Disbursements Register */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.

        RUN check-date.

        ASSIGN rd-dest
            tran-period
            tran-date
            v-s-date     = begin_date
            v-e-date     = end_date
            sort-by-vend = rd_sort = "vendor#".

        IF v-invalid THEN RETURN NO-APPLY.

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        xtrnum        = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = xtrnum.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.

        RUN run-report.

        IF v-postable THEN 
        DO:

            DO:

                SESSION:SET-WAIT-STATE ("general").

                IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
                    THEN is-xprint-form = YES.     
                ELSE is-xprint-form = NO.

                RUN run-report. 

                CASE rd-dest:
                    WHEN 1 THEN RUN output-to-printer.
                    WHEN 2 THEN RUN output-to-screen.
                    WHEN 3 THEN RUN output-to-file.
                    WHEN 4 THEN 
                        DO:
           /*run output-to-fax.*/
                            {custom/asifax.i &type= ''
                            &begin_cust= "tran-date"
                            &END_cust= "tran-date" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                        END. 
                    WHEN 5 THEN 
                        DO:
                            IF is-xprint-form THEN 
                            DO:
                                {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
                            END.
                            ELSE 
                            DO:
                                {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                            END.
                        END. 
                    WHEN 6 THEN RUN OUTPUT-to-port.
                END CASE.
                SESSION:SET-WAIT-STATE("").
            END.



            /*  case rd-dest:
                   when 1 then run output-to-printer.
                   when 2 then run output-to-screen.
                   when 3 then run output-to-file.
              end case. */

            lv-post = NO.

            MESSAGE "Post Cash Disbursements ?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

            END.
            ELSE RUN undo-trnum.
        END.

        ELSE 
        DO:
            MESSAGE "No Cash Disbursements  available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
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
    RUN init-proc.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VL2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    RUN check-date.

    {methods/nowait.i}
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
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN 
        DO:
            IF NOT period.pstat THEN 
            DO:
                MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            ELSE IF period.subLedgerAP EQ "C" THEN 
                DO:
                    MESSAGE "Payables sub ledger already closed. " VIEW-AS ALERT-BOX ERROR.
                    v-invalid = YES.
                END.
            tran-period:SCREEN-VALUE = STRING(period.pnum).
        END.

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
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
        targetfile = lv-audit-dir + "\AP\VL2\Run#"
                    + STRING(xtrnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AP"
        dirname3   = lv-audit-dir + "\AP\VL2".

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
    DISPLAY tran-date tran-period begin_date end_date lbl_sort rd_sort rd-dest 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 RECT-8 tran-date begin_date end_date rd_sort rd-dest 
        tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO :
        FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode.
        IF NOT AVAILABLE ap-ctrl THEN RETURN.
        xap-acct = ap-ctrl.payables.
        xcs-acct = ap-ctrl.cash-act.
        RELEASE ap-ctrl.
    END.

    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.   */

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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */

    /* Use Progress Print. Always use Font#9 in Registry (set above) */
    /*
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
    DEFINE VARIABLE lv-chkno-posted LIKE bank.last-chk NO-UNDO.

    postit:
    DO TRANSACTION ON ERROR UNDO:
        FOR EACH ap-dis WHERE ap-dis.company = cocode AND
            (ap-dis.check-date >= v-s-date AND ap-dis.check-date <= v-e-date) AND
            NOT ap-dis.posted
            ON ERROR UNDO postit, LEAVE postit:
            FIND vend WHERE (vend.company = cocode) AND vend.vend-no = ap-dis.vend-no
                NO-LOCK NO-ERROR.
            FIND bank WHERE bank.bank-code = ap-dis.bank-code AND
                bank.company = cocode NO-ERROR.

            FOR EACH ap-disl WHERE ap-disl.d-no = ap-dis.d-no
                BREAK BY ap-disl.check-no:

                IF FIRST-OF(ap-disl.check-no) THEN
                    i = 0.
                lv-chkno-posted = ap-disl.check-no.
                FIND FIRST ap-pay WHERE ap-pay.company  = cocode AND
                    ap-pay.check-act = bank.actnum AND
                    ap-pay.check-no = ap-dis.check-no NO-ERROR.

                IF NOT AVAILABLE ap-pay THEN 
                DO:
                    FIND LAST ap-pay USE-INDEX c-no NO-ERROR.
                    IF AVAILABLE ap-pay THEN 
                    DO:
                        x = ap-pay.c-no.
                    END.

                    DO:
                        CREATE ap-pay.
                        ASSIGN
                            ap-pay.company          = cocode
                            ap-pay.check-act        = bank.actnum
                            ap-pay.check-amt        = ap-dis.check-amt
                            ap-pay.check-no         = ap-dis.check-no
                            ap-pay.period           = tran-period
                            ap-pay.c-no             = x + 1
                            ap-pay.vend-no          = ap-dis.vend-no
                            ap-pay.bank-code        = ap-dis.bank-code
                            ap-pay.d-no             = ap-dis.d-no
                            ap-pay.transactionDate  = tran-date
                            .
                    END.
                END.

                ASSIGN
                    ap-pay.check-date = ap-dis.check-date
                    ap-pay.man-check  = TRUE
                    ap-pay.posted     = TRUE.

                IF ap-pay.check-date = ? THEN
                    ap-pay.check-date = TODAY.

                FIND LAST ap-payl WHERE ap-payl.c-no = ap-pay.c-no USE-INDEX c-no
                    NO-ERROR.
                IF AVAILABLE ap-payl THEN
                    i = ap-payl.line + 1.
                ELSE
                    i = 1.

                CREATE ap-payl.
                ASSIGN 
                    ap-payl.posted    = TRUE
                    ap-payl.c-no      = ap-pay.c-no
                    ap-payl.check-no  = ap-disl.check-no
                    ap-payl.line      = i
                    ap-payl.inv-no    = ""
                    ap-payl.d-no      = ap-disl.d-no
                    ap-payl.amt-disc  = 0
                    ap-payl.amt-paid  = ap-disl.amt
                    ap-payl.vend-no   = ap-dis.vend-no
                    ap-payl.man-check = TRUE
                    ap-payl.actnum    = ap-disl.actnum.


                bank.bal = bank.bal - ap-disl.amt.
                RUN GL_SpCreateGLHist(cocode,
                    ap-disl.actnum,
                    "CDISB",
                    (IF AVAILABLE vend THEN vend.name ELSE ap-dis.payee)
                    + " " + string(ap-dis.check-no),
                    tran-date,
                    ap-disl.amt,
                    xtrnum,
                    tran-period,
                    "A",
                    tran-date,
                    (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Check:" + STRING(ap-dis.check-no,"999999999999"),
                    "AP").
                DO :
                    FIND FIRST b-bank WHERE b-bank.actnum = ap-disl.actnum
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE b-bank THEN b-bank.bal = b-bank.bal + ap-disl.amt.
                END.
                ASSIGN
                    t1             = t1 + ap-disl.amt
                    ap-disl.posted = TRUE.
            END.  /* each line */

            /* Commented out per Julie's Request Task #02230003 
            if avail(vend) then
            do:
              assign
                vend.purch[tran-period]   = vend.purch[tran-period]   + t1
                vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
                vend.purch[13]        = vend.purch[13]   + t1
                vend.n-purch[13]      = vend.n-purch[13] + 1
                vend.acc-bal          = vend.acc-bal     + t1.
              if vend.acc-bal >= vend.hibal then
                assign vend.hibal = vend.acc-bal
                       vend.hibal-date = ap-dis.check-date.
              release vend.
            end.
            Commented out per Julie's Request Task #02230003 */

            CREATE ap-ledger.
            ASSIGN
                ap-ledger.company  = cocode
                ap-ledger.vend-no  = ap-dis.vend-no
                ap-ledger.amt      = ap-dis.check-amt
                ap-ledger.refnum   = "CHK# " + string(ap-dis.check-no) +
                              " CD#" + bank.bank-code
                ap-ledger.ref-date = ap-dis.check-date
                ap-ledger.tr-date  = tran-date
                ap-ledger.trnum    = xtrnum
                t1                 = 0
                ap-dis.posted      = TRUE.

         
            RUN GL_SpCreateGLHist(cocode,
                bank.actnum,
                "CDISB",
                "CASH DISBURSEMENTS",
                tran-date,
                (-(ap-dis.check-amt)),
                xtrnum,
                tran-period,
                "A",
                tran-date,
                (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Check:" + STRING(ap-dis.check-no,"999999999999"),
                "AP").
            ASSIGN
                bank.last-chk = IF lv-chkno-posted >= bank.last-chk THEN lv-chkno-posted
                         ELSE bank.last-chk.
        END.
    END. /* postit: transaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    {sys/form/r-top3w.f}

    FORM HEADER
        "VENDOR#  NAME                                   CHECK #      DATE          "
        "AMOUNT       G/L DISTRIBUTION" SKIP FILL("_",130) FORMAT "x(130)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

    time_stamp = STRING(TIME, "HH:MMam").
    tmpstore   = FILL("_",125).

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "CASH DISBURSEMENTS  -  EDIT REGISTER " + string(xtrnum)
        str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.  

    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.

    ASSIGN
        g1 = 0
        g2 = 0.

    IF sort-by-vend THEN 
    DO:
    {ap/ap-dreg.i "vend-no"}
    END.

    ELSE 
    DO:
    {ap/ap-dreg.i "d-no"}
    END.

    DISPLAY  "** GRAND TOTAL  "  AT 90  g2 FORMAT "-9,999,999.99" TO 128
        WITH NO-LABELS NO-UNDERLINE STREAM-IO WIDTH 132 FRAME gt.

    HIDE FRAME f-top.

    str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
        "Summary by Account".
    x = (132 - length(str-tit3)) / 2.
    str-tit3 = FILL(" ",x) + str-tit3 .
    PAGE.
    FORM HEADER
        "ACCCOUNT                                  DATE   VENDOR#  CHECK#"
        "LINE DESCRIPTION                QTY   UNIT PRICE          AMOUNT" SKIP
        FILL("_",132) FORMAT "x(130)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP STREAM-IO WIDTH 132.

    DISPLAY "" WITH FRAME f-top2.

    FOR EACH ap-disl WHERE NOT ap-disl.posted AND ap-disl.company = cocode
        BREAK BY ap-disl.actnum BY ap-disl.check-no
        WITH STREAM-IO WIDTH 132 NO-LABELS:
        FIND ap-dis WHERE ap-dis.company = cocode AND
            ap-dis.d-no = ap-disl.d-no AND
            (ap-dis.check-date >= v-s-date AND ap-dis.check-date <= v-e-date) AND
            NOT ap-dis.posted NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ap-dis THEN NEXT.
        FIND vend WHERE vend.company = cocode AND vend.vend-no = ap-dis.vend-no NO-LOCK NO-ERROR.
        FIND bank WHERE bank.bank-code = ap-dis.bank-code AND
            bank.company = cocode NO-LOCK NO-ERROR.
        IF FIRST-OF(ap-disl.actnum)
            THEN 
        DO:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = ap-disl.actnum
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN v-descr = account.dscr.

            PUT ap-disl.actnum + " - " + v-descr FORMAT "x(39)" .
        END.
        PUT  ap-dis.check-date AT 41     SPACE(1)
            ap-dis.vend-no              SPACE(1)
            ap-dis.check-no             SPACE(1)
            ap-disl.line FORMAT ">>>9"  SPACE(1)
            ap-disl.dscr FORMAT "x(20)" SPACE(1)
            ap-disl.qty                 SPACE(1)
            ap-disl.unit-pr             SPACE(2)
            ap-disl.amt
            SKIP.
        ACCUMULATE ap-disl.amt (TOTAL BY ap-disl.actnum).
        ACCUMULATE ap-disl.amt (TOTAL).
        IF LAST-OF(ap-disl.actnum) THEN 
        DO:
            PUT "** TOTAL "  TO 116
                ACCUM TOTAL BY ap-disl.actnum ap-disl.amt FORMAT "-9,999,999.99" TO 131
                SKIP(1).
        END.
        v-postable = YES.
    END.
    PUT "***** TOTAL FOR ALL ACCOUNTS " TO 116
        ACCUM TOTAL ap-disl.amt FORMAT "-9,999,999.99" TO 129.


    SESSION:SET-WAIT-STATE ("").

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
    DEFINE VARIABLE lv-label      AS CHARACTER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* gdm - 11050906 */
    REPEAT:
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAILABLE gl-ctrl THEN 
        DO:

            IF xtrnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
            RELEASE gl-ctrl.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

