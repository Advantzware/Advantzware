&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-dash.w

  Description: Management Dashboard

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{salrep/dashtt.i NEW}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report 
    FIELD DATE         AS DATE
    FIELD row-id       AS ROWID
    FIELD qty          AS DECIMAL
    FIELD amt          LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD cash-date    LIKE ar-inv.inv-date
    FIELD misc         AS LOG
    FIELD cost         AS DECIMAL
    FIELD msf          AS DECIMAL
    FIELD is-duplicate AS LOG.

DEFINE TEMP-TABLE tt-ap-report NO-UNDO LIKE report
    FIELD check-date LIKE ap-pay.check-date
    FIELD inv-no     LIKE ap-payl.inv-no
    FIELD amt-paid   LIKE ap-payl.amt-paid.

DEFINE TEMP-TABLE work-tmp NO-UNDO
    FIELD job          LIKE job.job
    FIELD frm          LIKE job-mch.frm
    FIELD blank-no     LIKE job-mch.blank-no
    FIELD sort-field   AS CHARACTER
    FIELD dept         AS CHARACTER FORMAT 'xx'
    FIELD m-code       LIKE mach.m-code
    FIELD pass         LIKE job-mch.pass
    FIELD r-act-hrs    AS DECIMAL   FORMAT '>>>>9.99'
    FIELD m-act-hrs    AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-chg-hrs   AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-nochg-hrs AS DECIMAL   FORMAT '>>>>9.99'
    FIELD qty          AS DECIMAL   FORMAT '>>>>>>>>9'
    FIELD msf          AS DECIMAL   FORMAT '>>>>>.999'
    INDEX work-tmp job frm blank-no dept m-code pass sort-field.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD w-sman-no AS CHARACTER
    FIELD w-sqft    LIKE itemfg.t-sqft FORMAT "->>>9.999" EXTENT 3
    FIELD w-amt     LIKE ar-inv.gross FORMAT "->>>,>>9.99" EXTENT 3
    FIELD w-cost    LIKE ar-inv.t-cost FORMAT "->>,>>9.99" EXTENT 3
    FIELD w-msf     AS DECIMAL   EXTENT 3.

DEFINE TEMP-TABLE w-ord NO-UNDO
    FIELD cost     LIKE oe-ordl.cost
    FIELD price    LIKE oe-ordl.price
    FIELD t-price  LIKE oe-ordl.t-price FORMAT "->>,>>>,>>9"
    FIELD rel-qty  LIKE oe-rel.qty
    FIELD rel-date AS DATE
    FIELD msf      AS DECIMAL FORMAT "->>9.999"
    FIELD tons     AS DECIMAL.

DEFINE TEMP-TABLE w-ord2 NO-UNDO
    FIELD qty-due  AS INTEGER   FORMAT "->>>,>>>,>>9"
    FIELD qty      LIKE oe-ordl.qty
    FIELD cost     LIKE oe-ordl.cost
    FIELD price    LIKE oe-ordl.price FORMAT ">>,>>9.99"
    FIELD uom      LIKE oe-ordl.pr-uom
    FIELD disc     LIKE oe-ordl.disc
    FIELD t-price  LIKE oe-ordl.t-price FORMAT ">>>,>>9.99"
    FIELD rel-date LIKE oe-relh.rel-date
    FIELD rel-stat AS CHARACTER
    FIELD inv-qty  LIKE oe-ordl.qty .

DEFINE BUFFER b-mach      FOR mach.
DEFINE BUFFER xreport     FOR tt-report.
DEFINE BUFFER b-tt-report FOR tt-report.
DEFINE BUFFER b-ar-invl   FOR ar-invl.
DEFINE BUFFER b-oe-ordl   FOR oe-ordl.
DEFINE BUFFER b-ar-cashl  FOR ar-cashl.
DEFINE BUFFER xoe-ord     FOR oe-ord.
DEFINE BUFFER b-itemfg    FOR itemfg.
DEFINE BUFFER b-account   FOR account.
DEFINE BUFFER b-company   FOR company.

DEFINE VARIABLE counter    AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-sman-no  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-prodc    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cp-part-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE cp-rowid   AS ROWID     NO-UNDO.
DEFINE VARIABLE fsman      AS CHARACTER NO-UNDO.
DEFINE VARIABLE tsman      AS CHARACTER INIT "zzz" NO-UNDO.
DEFINE VARIABLE v-dso      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iMach      AS INTEGER   INIT 100 NO-UNDO .
DEFINE VARIABLE iSales     AS INTEGER   INIT 4 NO-UNDO .
DEFINE BUFFER bf-user-print FOR user-print .
ASSIGN
    cocode = gcompany
    locode = gloc.

/* gdm - 03090905 */
DEFINE VARIABLE v-runflg AS LOG           INIT NO NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME browse-ar

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES account mach company

/* Definitions for BROWSE browse-ar                                     */
&Scoped-define FIELDS-IN-QUERY-browse-ar account.actnum account.dscr account.TYPE   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-ar   
&Scoped-define SELF-NAME browse-ar
&Scoped-define QUERY-STRING-browse-ar FOR EACH account WHERE      account.company = fi_company:SCREEN-VALUE NO-LOCK
&Scoped-define OPEN-QUERY-browse-ar OPEN QUERY {&SELF-NAME} FOR EACH account WHERE      account.company = fi_company:SCREEN-VALUE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browse-ar account
&Scoped-define FIRST-TABLE-IN-QUERY-browse-ar account


/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine mach.m-code mach.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine   
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code.
&Scoped-define TABLES-IN-QUERY-browse-machine mach
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine mach


/* Definitions for BROWSE browse-sales-forecast                         */
&Scoped-define FIELDS-IN-QUERY-browse-sales-forecast company.company company.NAME   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-sales-forecast   
&Scoped-define SELF-NAME browse-sales-forecast
&Scoped-define QUERY-STRING-browse-sales-forecast FOR EACH company NO-LOCK
&Scoped-define OPEN-QUERY-browse-sales-forecast OPEN QUERY {&SELF-NAME} FOR EACH company NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browse-sales-forecast company
&Scoped-define FIRST-TABLE-IN-QUERY-browse-sales-forecast company


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-browse-ar}~
    ~{&OPEN-QUERY-browse-machine}~
    ~{&OPEN-QUERY-browse-sales-forecast}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company ~
browse-machine tg_round tg_set-comp browse-sales-forecast browse-ar btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company tg_round ~
tg_set-comp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win    AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE fi_as-of-date AS DATE      FORMAT "99/99/9999":U 
    LABEL "As Of Date" 
    VIEW-AS FILL-IN 
    SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_company    AS CHARACTER FORMAT "X(3)":U 
    LABEL "Company" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 19.52
    BGCOLOR 15 .

DEFINE VARIABLE tg_round    AS LOGICAL INITIAL NO 
    LABEL "Round dollar amounts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tg_set-comp AS LOGICAL INITIAL NO 
    LABEL "Set Components Use Header Price?" 
    VIEW-AS TOGGLE-BOX
    SIZE 39 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-ar FOR 
    account SCROLLING.

DEFINE QUERY browse-machine FOR 
    mach SCROLLING.

DEFINE QUERY browse-sales-forecast FOR 
    company SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-ar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-ar C-Win _FREEFORM
    QUERY browse-ar NO-LOCK DISPLAY
    account.actnum FORMAT "X(25)" COLUMN-LABEL "Account Number"
    account.dscr FORMAT "X(45)" COLUMN-LABEL "Description"
    account.TYPE FORMAT "X(1)" COLUMN-LABEL "Type"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 46 BY 5.62 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.

DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
    QUERY browse-machine NO-LOCK DISPLAY
    mach.m-code FORMAT "X(6)" WIDTH 10
    mach.m-dscr FORMAT "X(20)" COLUMN-LABEL "Description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 44.4 BY 7.19 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.

DEFINE BROWSE browse-sales-forecast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-sales-forecast C-Win _FREEFORM
    QUERY browse-sales-forecast NO-LOCK DISPLAY
    company.company FORMAT "X(3)" COLUMN-LABEL "Code"
    company.NAME FORMAT "X(30)" COLUMN-LABEL "Description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 41.4 BY 5.62 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fi_as-of-date AT ROW 2.62 COL 14.6 COLON-ALIGNED
    fi_company AT ROW 2.62 COL 45 COLON-ALIGNED
    browse-machine AT ROW 5.43 COL 5
    tg_round AT ROW 5.57 COL 51.4
    tg_set-comp AT ROW 7.24 COL 51.4 WIDGET-ID 2
    browse-sales-forecast AT ROW 14.91 COL 4
    browse-ar AT ROW 14.91 COL 46.6
    btn-ok AT ROW 21.76 COL 27.6
    btn-cancel AT ROW 21.76 COL 51.2
    "Select A/R Account(s) for Avg DSO:" VIEW-AS TEXT
    SIZE 42.4 BY .62 AT ROW 13.19 COL 46.2
    FONT 6
    "Release Sales Forecast:" VIEW-AS TEXT
    SIZE 28 BY .62 AT ROW 6.57 COL 51.4 WIDGET-ID 4
    FONT 6
    "Select Up to 4 Companies" VIEW-AS TEXT
    SIZE 30.4 BY .62 AT ROW 13.19 COL 5
    FONT 6
    "Select Up To 99 Machines for Production:" VIEW-AS TEXT
    SIZE 48 BY .62 AT ROW 4.19 COL 4.8
    FONT 6
    "for Sales Forecast:" VIEW-AS TEXT
    SIZE 24.4 BY .62 AT ROW 13.95 COL 5
    FONT 6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    BGCOLOR 15 
    RECT-9 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95 BY 22.62
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
        TITLE              = "Management Highlights"
        HEIGHT             = 22.62
        WIDTH              = 95
        MAX-HEIGHT         = 32.52
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 32.52
        VIRTUAL-WIDTH      = 273.2
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
/* BROWSE-TAB browse-machine fi_company FRAME-A */
/* BROWSE-TAB browse-sales-forecast tg_set-comp FRAME-A */
/* BROWSE-TAB browse-ar browse-sales-forecast FRAME-A */
ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    fi_company:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-ar
/* Query rebuild information for BROWSE browse-ar
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH account WHERE
     account.company = fi_company:SCREEN-VALUE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-ar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE
     mach.company = fi_company:SCREEN-VALUE AND
     mach.loc = locode
     NO-LOCK BY mach.m-code.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-sales-forecast
/* Query rebuild information for BROWSE browse-sales-forecast
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH company NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-sales-forecast */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Management Highlights */
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
ON WINDOW-CLOSE OF C-Win /* Management Highlights */
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
        DO WITH FRAME {&FRAME-NAME}:

            IF v-runflg THEN 
            DO:

                SESSION:SET-WAIT-STATE("general").

                ASSIGN {&displayed-objects} fi_company tg_round.

                EMPTY TEMP-TABLE tt-raw-op.
                EMPTY TEMP-TABLE tt-raw-prod.
                EMPTY TEMP-TABLE work-tmp.
                EMPTY TEMP-TABLE tt-report.
                EMPTY TEMP-TABLE w-data.
                EMPTY TEMP-TABLE tt-sales-prod-cat.
                EMPTY TEMP-TABLE tt-raw-salesmen.
                EMPTY TEMP-TABLE tt-raw-sales.
                EMPTY TEMP-TABLE tt-sales-forecast.
                EMPTY TEMP-TABLE tt-ar-dso.

                IF NOT CAN-FIND(FIRST company WHERE
                    company.company EQ fi_company) THEN
                DO:
                    MESSAGE "Invalid Company."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
                    LEAVE.
                END.

                IF BROWSE browse-machine:NUM-SELECTED-ROWS GT 99 THEN
                DO:
                    MESSAGE "More than 99 Machines are Selected."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY "ENTRY:U" TO BROWSE browse-machine.
                    LEAVE.
                END.

                IF BROWSE browse-sales-forecast:NUM-SELECTED-ROWS GT 4 THEN
                DO:
                    MESSAGE "More than 4 Companines are Selected."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY "ENTRY:U" TO BROWSE browse-sales-forecast.
                    LEAVE.
                END.

                RUN reftable-proc.

                /*blank prod cat*/
                CREATE tt-sales-prod-cat.
                tt-sales-prod-cat.DATE = fi_as-of-date.
                RELEASE tt-sales-prod-cat.

                FOR EACH fgcat FIELDS(procat) WHERE
                    fgcat.company EQ fi_company
                    NO-LOCK:

                    CREATE tt-sales-prod-cat.
                    ASSIGN 
                        tt-sales-prod-cat.prod-cat = fgcat.procat
                        tt-sales-prod-cat.DATE     = fi_as-of-date.
                    RELEASE tt-sales-prod-cat.
                END.

                IF NOT CAN-FIND(FIRST tt-sales-prod-cat WHERE
                    tt-sales-prod-cat.prod-cat EQ "MEMO") THEN
                DO:
                    CREATE tt-sales-prod-cat.
                    ASSIGN 
                        tt-sales-prod-cat.prod-cat = "MEMO"
                        tt-sales-prod-cat.DATE     = fi_as-of-date.
                    RELEASE tt-sales-prod-cat.
                END.

                IF NOT CAN-FIND(FIRST tt-sales-prod-cat WHERE
                    tt-sales-prod-cat.prod-cat EQ "MISC") THEN
                DO:
                    CREATE tt-sales-prod-cat.
                    ASSIGN 
                        tt-sales-prod-cat.prod-cat = "MISC"
                        tt-sales-prod-cat.DATE     = fi_as-of-date.
                    RELEASE tt-sales-prod-cat.
                END.

                FOR EACH sman WHERE
                    sman.company EQ fi_company
                    NO-LOCK:

                    CREATE tt-raw-salesmen.
                    ASSIGN 
                        tt-raw-salesmen.sman  = sman.sman
                        tt-raw-salesmen.sname = sman.sname
                        tt-raw-salesmen.DATE  = fi_as-of-date.
                    RELEASE tt-raw-salesmen.
                END.

                RUN run-report.

                SESSION:SET-WAIT-STATE("").
            END.
            ELSE
                IF NOT v-runflg THEN 
                DO:

                    MESSAGE 
                        "Management Reports are available for purchase, please call ASI."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

                    APPLY "close" TO THIS-PROCEDURE.

                END.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON ENTRY OF fi_company IN FRAME FRAME-A /* Company */
    DO:
        SELF:MODIFIED = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON LEAVE OF fi_company IN FRAME FRAME-A /* Company */
    DO:
        DO WITH FRAME {&FRAME-NAME}:

            IF SELF:MODIFIED THEN
            DO:
                IF BROWSE browse-sales-forecast:NUM-SELECTED-ROWS GT 0 THEN
                    IF browse-sales-forecast:DESELECT-ROWS() THEN.

                FOR EACH reftable WHERE
                    reftable.reftable EQ "HM1SF" AND
                    reftable.company EQ USERID("NOSWEAT")
                    NO-LOCK,
                    FIRST company WHERE
                    company.company = reftable.loc
                    NO-LOCK:

                    REPOSITION browse-sales-forecast TO ROWID ROWID(company).
                    IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.
                END.

                FIND FIRST company WHERE
                    company.company EQ fi_company:SCREEN-VALUE
                    NO-LOCK NO-ERROR.

                IF AVAILABLE company THEN
                DO:
                    REPOSITION browse-sales-forecast TO ROWID ROWID(company).
                    IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.

                    RELEASE company.
                END.

                FIND FIRST company NO-LOCK NO-ERROR.

                IF AVAILABLE company THEN
                    REPOSITION browse-sales-forecast TO ROWID ROWID(company).

                ASSIGN fi_company.

                CLOSE QUERY browse-machine.

                OPEN QUERY browse-machine FOR EACH mach WHERE
                    mach.company = fi_company AND
                    mach.loc = locode
                    NO-LOCK BY mach.m-code.

                FOR EACH reftable WHERE
                    reftable.reftable EQ "HM1" AND
                    reftable.company EQ USERID("NOSWEAT") AND
                    reftable.loc = fi_company
                    NO-LOCK,
                    FIRST mach WHERE
                    mach.company = fi_company AND
                    mach.loc = locode AND
                    mach.m-code = reftable.CODE
                    NO-LOCK:

                    REPOSITION browse-machine TO ROWID ROWID(mach).
                    IF browse-machine:SELECT-FOCUSED-ROW() THEN.
                END.

                FOR EACH b-mach WHERE
                    b-mach.company EQ fi_company:SCREEN-VALUE AND
                    b-mach.loc EQ locode
                    NO-LOCK
                    BY b-mach.m-code:

                    LEAVE.
                END.

                IF AVAILABLE b-mach THEN
                    REPOSITION browse-machine TO ROWID ROWID(b-mach).

                CLOSE QUERY browse-ar.

                OPEN QUERY browse-ar FOR EACH account WHERE
                    account.company = fi_company NO-LOCK.
            END.

            FOR EACH reftable WHERE
                reftable.reftable EQ "HM1Acct" AND
                reftable.company EQ USERID("NOSWEAT") AND
                reftable.loc = fi_company:SCREEN-VALUE
                NO-LOCK,
                FIRST account WHERE
                account.company = fi_company:SCREEN-VALUE AND
                account.actnum = reftable.CODE
                NO-LOCK:

                REPOSITION browse-ar TO ROWID ROWID(account).
                IF browse-ar:SELECT-FOCUSED-ROW() THEN.
            END.

            FIND FIRST account WHERE
                account.company = fi_company:SCREEN-VALUE
                NO-LOCK NO-ERROR.

            IF AVAILABLE account THEN
                REPOSITION browse-ar TO ROWID ROWID(account).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-ar
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

&Scoped-define sdBrowseName browse-machine
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName browse-sales-forecast
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName browse-ar
{methods/template/brwcustom2.i 3}

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

    fi_company = cocode.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}
    {custom/usrprint.i}

    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST company WHERE
            company.company EQ fi_company:SCREEN-VALUE
            NO-LOCK NO-ERROR.

        IF AVAILABLE company THEN
        DO:
            REPOSITION browse-sales-forecast TO ROWID ROWID(company).
            IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.

            RELEASE company.
        END.


        FIND FIRST bf-user-print NO-LOCK
            WHERE (bf-user-print.company  EQ cocode  OR bf-user-print.company EQ "")       
            AND bf-user-print.program-id EQ "HM1SF" 
            AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

        IF AVAILABLE bf-user-print THEN 
        DO:
            DO i = 1 TO iSales:
                FIND FIRST company NO-LOCK
                    WHERE company.company = bf-user-print.field-value[i]  NO-ERROR.

                IF AVAILABLE company AND bf-user-print.field-value[i] NE ""  THEN
                    REPOSITION browse-sales-forecast TO ROWID ROWID(company).
                IF browse-sales-forecast:SELECT-FOCUSED-ROW() THEN.
            END.
        END.    /*avail user-print */ 
        RELEASE bf-user-print .

        FIND FIRST company NO-LOCK NO-ERROR.

        IF AVAILABLE company THEN
            REPOSITION browse-sales-forecast TO ROWID ROWID(company).

        CLOSE QUERY browse-machine.

        OPEN QUERY browse-machine FOR EACH mach WHERE
            mach.company = fi_company:SCREEN-VALUE AND
            mach.loc = locode
            NO-LOCK BY mach.m-code.
     
        FIND FIRST bf-user-print NO-LOCK
            WHERE bf-user-print.company    EQ cocode        
            AND bf-user-print.program-id EQ "HM1" 
            AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

        IF AVAILABLE bf-user-print THEN 
        DO:
            DO i = 1 TO iMach:
                IF bf-user-print.field-value[i] EQ "" THEN LEAVE .
                FIND FIRST mach NO-LOCK
                    WHERE mach.company = fi_company:SCREEN-VALUE AND
                    mach.loc = locode AND
                    mach.m-code = bf-user-print.field-value[i] NO-ERROR.
                IF AVAILABLE mach AND bf-user-print.field-value[i] NE ""  THEN
                    REPOSITION browse-machine TO ROWID ROWID(mach).
                IF browse-machine:SELECT-FOCUSED-ROW() THEN.
            END.
        END.    /*avail user-print */ 

        FOR EACH b-mach WHERE
            b-mach.company EQ fi_company:SCREEN-VALUE AND
            b-mach.loc = locode
            NO-LOCK
            BY b-mach.m-code:

            LEAVE.
        END.

        IF AVAILABLE b-mach THEN
            REPOSITION browse-machine TO ROWID ROWID(b-mach).

        CLOSE QUERY browse-ar.

        OPEN QUERY browse-ar FOR EACH account WHERE
            account.company = fi_company:SCREEN-VALUE NO-LOCK.
     
        FIND FIRST bf-user-print NO-LOCK
            WHERE bf-user-print.company    EQ cocode        
            AND bf-user-print.program-id EQ "HM1Acct" 
            AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

        IF AVAILABLE bf-user-print THEN 
        DO:
            DO i = 1 TO iMach:
                IF bf-user-print.field-value[i] EQ "" THEN LEAVE .
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ fi_company:SCREEN-VALUE 
                    AND account.actnum EQ bf-user-print.field-value[i] NO-ERROR.

                IF AVAILABLE account AND bf-user-print.field-value[i] NE ""  THEN
                    REPOSITION browse-ar TO ROWID ROWID(account).
                IF browse-ar:SELECT-FOCUSED-ROW() THEN.
            END.
        END.    /*avail user-print */ 

        FIND FIRST account WHERE
            account.company = fi_company:SCREEN-VALUE
            NO-LOCK NO-ERROR.

        IF AVAILABLE account THEN
            REPOSITION browse-ar TO ROWID ROWID(account).

        fi_as-of-date:SCREEN-VALUE = STRING(TODAY).
        APPLY "entry" TO fi_as-of-date.
    END.

    /* gdm - 03090905 */
    {salrep/SlsMgmt.i}

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ap-ar-proc C-Win 
PROCEDURE ap-ar-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*from arrep\r-cashsm.w  include terms discount is yes
      Show only invoices ... 0 days*/

    DEFINE VARIABLE v-sman           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-amt            LIKE ar-cashl.amt-paid EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-dsc            LIKE v-amt NO-UNDO.
    DEFINE VARIABLE v-pct            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-start-of-month AS DATE      NO-UNDO.

    EMPTY TEMP-TABLE tt-ar-ap.
    EMPTY TEMP-TABLE tt-report.

    CREATE tt-ar-ap.

    v-start-of-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

    FOR EACH cust FIELDS(company cust-no sman) WHERE
        cust.company EQ fi_company NO-LOCK:

        FOR EACH ar-inv FIELDS(x-no inv-date inv-date) 
            WHERE ar-inv.company  EQ fi_company
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE v-start-of-month
            AND ar-inv.inv-date LE fi_as-of-date
            AND ar-inv.terms    EQ "CASH"
            NO-LOCK,
            EACH ar-invl FIELDS(sman s-pct inv-no )
            WHERE ar-invl.x-no EQ ar-inv.x-no
            NO-LOCK

            /*transaction*/ :

            DO i = 1 TO 3:
                v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                ELSE ar-invl.sman[i].

                IF i NE 1 AND
                    (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

                FIND FIRST tt-report
                    WHERE tt-report.key-01  EQ v-sman
                    AND tt-report.key-02  EQ string(ar-invl.inv-no,"9999999999")
                    AND tt-report.rec-id  EQ recid(ar-invl)
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE tt-report THEN 
                DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.key-01 = v-sman
                        tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                        tt-report.key-04 = STRING(ar-inv.inv-date,"99/99/9999")
                        tt-report.rec-id = RECID(ar-invl).
                END.
            END.
        END.      

        FOR EACH ar-cash FIELDS(c-no cust-no check-date)
            WHERE ar-cash.company    EQ fi_company
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE v-start-of-month
            AND ar-cash.check-date LE fi_as-of-date
            AND ar-cash.posted     EQ YES
            AND ar-cash.check-no   NE 0
            NO-LOCK,
            EACH ar-cashl FIELDS(inv-no)
            WHERE ar-cashl.c-no   EQ ar-cash.c-no
            AND ar-cashl.posted EQ YES
            AND ar-cashl.memo   EQ NO
            /* and (v-days         eq 0 or
                  (ar-cash.check-date - ar-cashl.inv-date gt v-days and
                   ar-cashl.inv-no ne 0)) */
            NO-LOCK

            /*transaction*/ :

            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl FIELDS(sman s-pct inv-no)
                    WHERE ar-invl.company EQ fi_company
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK:

                    DO i = 1 TO 3:
                        v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                        ELSE ar-invl.sman[i].

                        IF i NE 1 AND
                            (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

                        FIND FIRST tt-report
                            WHERE tt-report.key-01  EQ v-sman
                            AND tt-report.key-02  EQ string(ar-invl.inv-no,"9999999999")
                            AND tt-report.rec-id  EQ recid(ar-cashl)
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE tt-report THEN 
                        DO:
                            CREATE tt-report.
                            ASSIGN
                                tt-report.key-01 = v-sman
                                tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                                tt-report.key-04 = STRING(ar-cash.check-date,"99/99/9999")    
                                tt-report.rec-id = RECID(ar-cashl).
                        END.
                    END.
                END.

            ELSE 
            DO:
                v-sman = cust.sman.

                FIND FIRST tt-report
                    WHERE tt-report.key-01  EQ v-sman
                    AND tt-report.key-02  EQ string(ar-cashl.inv-no,"9999999999")
                    AND tt-report.rec-id  EQ recid(ar-cashl)
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE tt-report THEN 
                DO:
                    CREATE tt-report.

                    ASSIGN
                        tt-report.key-01 = v-sman
                        tt-report.key-02 = STRING(ar-cashl.inv-no,"9999999999")
                        tt-report.key-04 = STRING(ar-cash.check-date,"99/99/9999")
                        tt-report.rec-id = RECID(ar-cashl).
                END.
            END.
        END.
    END.

    FOR EACH tt-report
    /*transaction*/ :

        RELEASE ar-inv.
        RELEASE ar-cash.

        FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.    

        IF AVAILABLE ar-cashl THEN 
        DO:
            FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

            ASSIGN
                v-dsc[1] = ar-cashl.amt-disc
                v-amt[1] = ar-cashl.amt-paid + v-dsc[1]
                v-amt[2] = v-amt[1].

            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl FIELDS(x-no inv-no amt sman s-pct)
                    WHERE ar-invl.company EQ fi_company
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK,

                    FIRST ar-inv FIELDS(tax-amt f-bill freight) WHERE
                    ar-inv.x-no EQ ar-invl.x-no NO-LOCK

                    BREAK BY ar-invl.inv-no:

                    IF FIRST(ar-invl.inv-no) THEN
                        ASSIGN
                            v-amt    = 0
                            v-amt[1] = ar-inv.tax-amt +
                      (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0).

                    v-amt[1] = v-amt[1] + ar-invl.amt.

                    IF ar-invl.sman[1] NE "" THEN
                    DO i = 1 TO 3:
                        IF tt-report.key-01 EQ ar-invl.sman[i] THEN 
                        DO:
                            ASSIGN
                                v-amt[2] = v-amt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100).
                            LEAVE.
                        END.
                    END.
                    ELSE
                        ASSIGN
                            v-amt[2] = v-amt[2] + ar-invl.amt.
                END.

            ASSIGN
                v-pct    = v-amt[2] / v-amt[1]
                v-amt[1] = (ar-cashl.amt-paid + v-dsc[1]) * v-pct
                v-pct    = v-amt[1] / v-amt[2].

            RELEASE ar-inv.
        END.

        ELSE 
        DO:
            FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK.

            ASSIGN
                v-amt[1] = ar-invl.amt.
        END.

        IF v-amt[1] EQ ? THEN v-amt[1] = 0.

        IF DATE(tt-report.key-04) EQ fi_as-of-date THEN
            tt-ar-ap.date-ar-rec-amt = tt-ar-ap.date-ar-rec-amt + v-amt[1].

        tt-ar-ap.mtd-ar-rec-amt = tt-ar-ap.mtd-ar-rec-amt + v-amt[1].

    END.

    RUN ap-proc.

    RELEASE tt-ar-ap.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ap-proc C-Win 
PROCEDURE ap-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*from VR13*/
    DEFINE VARIABLE v-gross-amt      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-start-of-month AS DATE    NO-UNDO.

    v-start-of-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

    EMPTY TEMP-TABLE tt-ap-report.

    FOR EACH vend FIELDS(vend-no) WHERE
        vend.company EQ fi_company
        NO-LOCK,
        EACH ap-pay FIELDS(c-no check-no vend-no check-act check-date check-amt)
        WHERE ap-pay.company    EQ fi_company
        AND ap-pay.vend-no    EQ vend.vend-no
        AND ap-pay.check-date GE v-start-of-month
        AND ap-pay.check-date LE fi_as-of-date
        AND ap-pay.posted     EQ YES
        AND ap-pay.memo       EQ NO
        NO-LOCK USE-INDEX vend-no,
        EACH ap-payl FIELDS(amt-paid amt-disc inv-no amt-paid amt-disc LINE) 
        WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK

        BREAK BY ap-pay.check-act
        BY ap-pay.check-no
        BY ap-payl.inv-no
        BY ap-payl.line
        BY ap-payl.amt-paid:

        v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).

        IF FIRST-OF(ap-payl.inv-no) THEN 
        DO:
            CREATE tt-ap-report.

            ASSIGN
                tt-ap-report.key-03     = ap-payl.inv-no
                tt-ap-report.check-date = ap-pay.check-date
                tt-ap-report.inv-no     = ap-payl.inv-no
                tt-ap-report.amt-paid   = ap-payl.amt-paid.
        END.

        IF LAST-OF(ap-pay.check-no) THEN 
        DO:
            IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN 
            DO:
                CREATE tt-ap-report.

                ASSIGN
                    tt-ap-report.key-03     = FILL("z",100) + "TOTAL"
                    tt-ap-report.check-date = ap-pay.check-date
                    tt-ap-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
                    tt-ap-report.amt-paid   = ap-pay.check-amt.

                IF tt-ap-report.inv-no EQ "Void" THEN
                    tt-ap-report.amt-paid  = tt-ap-report.amt-paid * -1.
            END.

            ASSIGN
                v-gross-amt = 0. 
        END.
    END.

    FOR EACH tt-ap-report:

        IF tt-ap-report.key-03 NE FILL("z",100) + "TOTAL" OR
            tt-ap-report.inv-no EQ "Void" THEN
        DO:
            tt-ar-ap.mtd-ap-paid-amt = tt-ar-ap.mtd-ap-paid-amt + tt-ap-report.amt-paid.

            IF tt-ap-report.check-date EQ fi_as-of-date THEN
                tt-ar-ap.date-ap-paid-amt = tt-ar-ap.date-ap-paid-amt + tt-ap-report.amt-paid. 
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE backlog-sales-forecast-proc C-Win 
PROCEDURE backlog-sales-forecast-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*OR 13 Range this month, Include jobs, Exclude set components = no*/
    DEFINE INPUT PARAMETER ip-start-month AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

    DEFINE VARIABLE li-qty AS INTEGER EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-qty  AS INTEGER EXTENT 2.

    ip-start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

    IF MONTH(fi_as-of-date) NE 12 THEN
        ip-start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
    ELSE
        ip-start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

    FOR EACH tt-sales-forecast:

        EMPTY TEMP-TABLE tt-report.
        EMPTY TEMP-TABLE w-ord2.

        FOR EACH xoe-ord FIELDS(cust-no ord-date TYPE company opened ord-no
            last-date) WHERE
            xoe-ord.company  EQ tt-sales-forecast.company AND
            xoe-ord.opened   EQ YES
            USE-INDEX opened NO-LOCK,
            EACH oe-ordl FIELDS(ship-qty t-ship-qty qty part-no i-no
            job-no job-no2) WHERE
            oe-ordl.company EQ xoe-ord.company AND
            oe-ordl.ord-no  EQ xoe-ord.ord-no AND
            oe-ordl.stat    NE "C" AND
            oe-ordl.req-date GE ip-start-month AND
            oe-ordl.req-date LT ip-start-next-month
            NO-LOCK:

            IF oe-ordl.ship-qty EQ oe-ordl.t-ship-qty THEN
                li-qty[2] = oe-ordl.ship-qty.
            ELSE
                RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT li-qty[1], OUTPUT li-qty[2]).

            IF li-qty[2] LT oe-ordl.qty THEN 
            DO:  
                CREATE tt-report.
                ASSIGN
                    tt-report.key-03 = oe-ordl.i-no
                    tt-report.key-04 = STRING(xoe-ord.ord-no,"9999999999")
                    tt-report.rec-id = RECID(oe-ordl).
            END.
        END. /*for each xoe-ord */

        FOR EACH job-hdr FIELDS(job job-no job-no2 i-no qty cust-no)
            WHERE job-hdr.company EQ tt-sales-forecast.company
            AND job-hdr.ord-no  EQ 0
            NO-LOCK,
            FIRST job FIELDS(job-no job-no2)
            WHERE job.company    EQ tt-sales-forecast.company
            AND job.job        EQ job-hdr.job
            AND job.job-no     EQ job-hdr.job-no
            AND job.job-no2    EQ job-hdr.job-no2
            AND job.start-date GE ip-start-month
            AND job.start-date LT ip-start-next-month
            AND job.opened     EQ YES
            NO-LOCK,
            FIRST itemfg FIELDS(part-no)
            WHERE itemfg.company EQ tt-sales-forecast.company
            AND itemfg.i-no    EQ job-hdr.i-no
            NO-LOCK  

            BREAK BY job-hdr.job
            BY job-hdr.job-no
            BY job-hdr.job-no2
            BY job-hdr.i-no:

            v-qty[1] = v-qty[1] + job-hdr.qty.

            IF LAST-OF(job-hdr.i-no) THEN 
            DO:
                RUN fg/GetProductionQty.p (INPUT cocode,
                    INPUT job-hdr.job-no,
                    INPUT job-hdr.job-no2,
                    INPUT job-hdr.i-no,
                    INPUT NO,
                    OUTPUT v-qty[2]).

                IF v-qty[2] LT v-qty[1] THEN 
                DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.key-03 = job-hdr.i-no
                        tt-report.key-04 = STRING(v-qty[1] - v-qty[2],"9999999999")
                        tt-report.key-05 = job-hdr.cust-no
                        tt-report.rec-id = RECID(job).
                END.

                v-qty = 0.
            END. /*last-of (job-hdr.i-no) */
        END. /* each job-hdr */

        FOR EACH tt-report:

            {oe/rep/backlog3.i}

            FIND FIRST itemfg WHERE
                itemfg.company = tt-sales-forecast.company AND
                itemfg.i-no   = tt-report.key-03
                NO-LOCK NO-ERROR.

            ASSIGN
                tt-sales-forecast.backlog-amt  = tt-sales-forecast.backlog-amt + w-ord2.t-price
                tt-sales-forecast.backlog-qty  = tt-sales-forecast.backlog-qty + w-ord2.qty-due
                tt-sales-forecast.backlog-cost = tt-sales-forecast.backlog-cost + w-ord2.cost.

            IF AVAILABLE itemfg THEN
                tt-sales-forecast.backlog-msf = tt-sales-forecast.backlog-msf + (w-ord2.qty-due * itemfg.t-sqft / 1000).

        END. /* each tt-report */

    END. /* for each tt-sales-forecast*/

    FOR EACH tt-sales-forecast:
        tt-sales-forecast.backlog-profit = ROUND(tt-sales-forecast.backlog-amt -
            tt-sales-forecast.backlog-cost,2).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dso-proc C-Win 
PROCEDURE dso-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-total-period-bal AS DECIMAL   DECIMALS 2 NO-UNDO.
    DEFINE VARIABLE v-ninety-days-date AS DATE      NO-UNDO.
    DEFINE VARIABLE v-raw-sales        AS DECIMAL   DECIMALS 2 NO-UNDO.    
    DEFINE VARIABLE cAccountList       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dOpenBal           AS DECIMAL   NO-UNDO.

    FIND FIRST period WHERE
        period.company EQ fi_company AND
        period.pst LE fi_as-of-date AND
        period.pend GE fi_as-of-date
        NO-LOCK NO-ERROR.

    v-dso = 0.

    IF AVAILABLE period THEN
    DO:
        FOR EACH tt-ar-dso,
            FIRST account FIELDS(cyr company actnum) WHERE
            account.company EQ fi_company AND
            account.actnum EQ tt-ar-dso.actnum
            NO-LOCK:
            cAccountList = cAccountList + tt-ar-dso.actnum + ",".
            dOpenBal = 0.
            RUN gl/gl-opend.p (ROWID(account), period.pst, OUTPUT dOpenBal).
            /*           v-total-period-bal = v-total-period-bal + account.cyr[period.pnum]. */
            v-total-period-bal = v-total-period-bal + dOpenBal.          
            /*           IF period.pstat eq YES THEN */
            FOR EACH glhist FIELDS(tr-amt) WHERE
                glhist.company EQ account.company AND
                glhist.actnum  EQ account.actnum AND
                glhist.period  EQ period.pnum AND
                glhist.tr-date GE period.pst AND
                glhist.tr-date LE period.pend
                NO-LOCK:

                v-total-period-bal = v-total-period-bal + glhist.tr-amt.
            END.
        END.

        v-ninety-days-date = fi_as-of-date - 89.

        FOR EACH tt-raw-sales 
            WHERE tt-raw-sales.DATE GE v-ninety-days-date
            AND tt-raw-sales.DATE LE fi_as-of-date :

            v-raw-sales = v-raw-sales + ROUND(tt-raw-sales.date-amt,2).
        END.

        ASSIGN
            v-raw-sales = v-raw-sales / 90.0
            v-dso       = IF v-raw-sales NE 0 THEN ROUND(v-total-period-bal / v-raw-sales,1)
                 ELSE 0.
    END.

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
    DISPLAY fi_as-of-date fi_company tg_round tg_set-comp 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-9 fi_as-of-date fi_company browse-machine tg_round tg_set-comp 
        browse-sales-forecast browse-ar btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE invoiced-sales-forecast-proc C-Win 
PROCEDURE invoiced-sales-forecast-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*OR6 Order Cost*/
    DEFINE INPUT PARAMETER ip-start-month AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

    DEFINE VARIABLE ld-date    AS DATE      NO-UNDO.
    DEFINE VARIABLE ld-inv-pct AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ll-comp    AS LOG       NO-UNDO.
    DEFINE VARIABLE v-slsm     LIKE ar-invl.sman EXTENT 1 NO-UNDO.
    DEFINE VARIABLE v-slsp     LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.
    DEFINE VARIABLE v-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-amt      AS DECIMAL   DECIMALS 2 NO-UNDO.
    DEFINE VARIABLE ld-csh-pct AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL   DECIMALS 2 NO-UNDO.
    DEFINE VARIABLE v-i-no     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ord-no   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-msf      AS DECIMAL   NO-UNDO.

    ip-start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

    IF MONTH(fi_as-of-date) NE 12 THEN
        ip-start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
    ELSE
        ip-start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

    FOR EACH tt-sales-forecast:

        EMPTY TEMP-TABLE tt-report.

        FOR EACH cust FIELDS(cust-no sman) WHERE
            cust.company EQ tt-sales-forecast.company
            NO-LOCK:

            FOR EACH ar-inv FIELDS(x-no inv-no inv-date) WHERE
                ar-inv.company  EQ tt-sales-forecast.company AND
                ar-inv.posted   EQ YES AND
                ar-inv.cust-no  EQ cust.cust-no AND
                ar-inv.inv-date GE ip-start-month AND
                ar-inv.inv-date LT ip-start-next-month
                NO-LOCK,
                EACH ar-invl FIELDS(sman s-pct billable misc) WHERE
                ar-invl.x-no EQ ar-inv.x-no AND
                (ar-invl.billable OR NOT ar-invl.misc)
                NO-LOCK:

                RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT ll-comp).
                IF ll-comp THEN NEXT.

                DO i = 1 TO 3:
                    v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                        cust.sman ELSE ar-invl.sman[i].

                    IF (i NE 1 AND
                        (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                    CREATE tt-report.
                    ASSIGN
                        tt-report.key-01 = v-slsm[1]
                        tt-report.key-02 = cust.cust-no
                        tt-report.key-10 = "ar-invl"
                        tt-report.rec-id = RECID(ar-invl)
                        tt-report.row-id = ROWID(ar-invl).
                    IF i GT 1 THEN 
                    DO:
                        tt-report.is-duplicate = YES.
                    END.

                END.
            END. /*each ar-inv*/

            FOR EACH ar-cash FIELDS(c-no)
                WHERE ar-cash.company    EQ tt-sales-forecast.company
                AND ar-cash.cust-no    EQ cust.cust-no
                AND ar-cash.check-date GE ip-start-month
                AND ar-cash.check-date LT ip-start-next-month
                AND ar-cash.posted     EQ YES
                NO-LOCK,
                EACH ar-cashl FIELDS(company actnum inv-no dscr)
                WHERE ar-cashl.c-no    EQ ar-cash.c-no
                AND ar-cashl.posted  EQ YES
                AND ar-cashl.memo    EQ YES
                AND CAN-FIND(FIRST account
                WHERE account.company EQ ar-cashl.company
                AND account.actnum  EQ ar-cashl.actnum
                AND account.type    EQ "R")
                NO-LOCK:

                RELEASE tt-report.
                RELEASE ar-invl.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ ar-cashl.company
                        AND ar-invl.cust-no EQ cust.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        AND (ar-invl.billable OR NOT ar-invl.misc)
                        NO-LOCK NO-ERROR.

                IF ar-cashl.inv-no NE 0                                                       AND
                    (AVAILABLE ar-invl                             OR
                    (NOT AVAILABLE reftable AND
                    NOT ar-cashl.dscr MATCHES "*oe return*") OR
                    SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
                    FOR EACH b-ar-invl FIELDS(billable misc sman s-pct inv-no)
                        WHERE b-ar-invl.company EQ ar-cashl.company
                        AND b-ar-invl.cust-no EQ cust.cust-no
                        AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
                        NO-LOCK:

                        IF NOT ((b-ar-invl.billable OR NOT b-ar-invl.misc)
                            AND (NOT AVAILABLE ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))) THEN
                            NEXT.

                        IF AVAILABLE ar-invl THEN 
                        DO:
                            RUN oe/invlcomp.p (ROWID(b-ar-invl), OUTPUT ll-comp).
                            IF ll-comp THEN NEXT.
                        END.

                        DO i = 1 TO 3:
                            v-slsm[1] = IF b-ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                                cust.sman ELSE b-ar-invl.sman[i].

                            IF (i NE 1 AND
                                (v-slsm[1] EQ "" OR b-ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                            CREATE tt-report.
                            ASSIGN
                                tt-report.key-01 = v-slsm[1]
                                tt-report.row-id = ROWID(b-ar-invl)
                                tt-report.key-02 = cust.cust-no
                                tt-report.key-10 = "ar-cashl"
                                tt-report.rec-id = RECID(ar-cashl).
                            IF i GT 1 THEN  
                            DO:
                                tt-report.is-duplicate = YES.
                            END.

                        END.
                    END.

                ELSE 
                DO:

                    CREATE tt-report.
                    ASSIGN
                        tt-report.key-01 = cust.sman.

                    IF AVAILABLE tt-report THEN
                        ASSIGN
                            tt-report.key-02 = cust.cust-no
                            tt-report.key-10 = "ar-cashl"
                            tt-report.rec-id = RECID(ar-cashl).
                END.
            END. /*each ar-cash*/
        END. /*each cust*/ 

        input-work:
        FOR EACH tt-report WHERE ,
            FIRST cust FIELDS(cust-no) WHERE
            cust.company EQ tt-sales-forecast.company AND
            cust.cust-no EQ tt-report.key-02
            NO-LOCK:

            RELEASE ar-invl.
            RELEASE ar-cashl.

            ASSIGN
                v-cost   = 0
                v-amt    = 0
                v-qty    = 0
                v-msf    = 0
                v-i-no   = ""
                v-ord-no = 0.

            IF tt-report.key-10 EQ "ar-invl" THEN
                FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

            IF AVAILABLE ar-invl THEN 
            DO:

                RELEASE itemfg.

                FIND FIRST itemfg WHERE
                    itemfg.company EQ tt-sales-forecast.company AND
                    itemfg.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.

                DO i = 1 TO 3:
                    IF ar-invl.sman[i] EQ tt-report.key-01 OR
                        ar-invl.sman[1] EQ "" THEN LEAVE.
                    IF i EQ 3 THEN NEXT input-work.
                END.

                ASSIGN
                    v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                            (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                         ELSE ar-invl.s-pct[i]
                    v-i-no    = ar-invl.i-no
                    v-ord-no  = ar-invl.ord-no
                    v-qty     = (IF ar-invl.inv-qty NE 0 THEN ar-invl.inv-qty
                      ELSE ar-invl.qty) * v-slsp[1] / 100
                    v-msf     = IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                     ELSE
                       IF AVAILABLE itemfg THEN
                          (itemfg.t-sqft * ar-invl.qty / 1000) ELSE 0
                    v-amt     = ar-invl.amt * v-slsp[1] / 100
                    v-cost    = ar-invl.t-cost * v-slsp[1] / 100.

            END. /*if avail ar-invl*/
            ELSE
                IF tt-report.key-10 EQ "ar-cashl" THEN
                    FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                RELEASE oe-retl.
                RELEASE ar-invl.

                FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl AND NOT AVAILABLE ar-invl THEN 
                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ tt-sales-forecast.company
                        AND ar-invl.cust-no EQ cust.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                IF AVAILABLE ar-invl THEN 
                DO:
                    DO i = 1 TO 3:
                        IF ar-invl.sman[i] EQ tt-report.key-01 OR
                            ar-invl.sman[1] EQ ""               THEN LEAVE.
                        IF i EQ 3 THEN NEXT input-work.
                    END.

                    RELEASE itemfg.

                    FIND FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ tt-sales-forecast.company
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-ERROR.

                    ASSIGN
                        v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                             (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                            ELSE ar-invl.s-pct[i]
                        v-amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                      (v-slsp[1] / 100)
                        v-ord-no  = ar-invl.ord-no
                        v-i-no    = ar-invl.i-no.

                    IF AVAILABLE oe-retl THEN
                        ASSIGN
                            v-qty  = oe-retl.tot-qty-return * -1
                            v-msf  = IF AVAILABLE itemfg THEN
                          oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                        ELSE 0
                            v-cost = (oe-retl.cost * (oe-retl.tot-qty-return / 1000) *
                                         (v-slsp[1] / 100) * -1).

                    ELSE 
                    DO:
                        ld-inv-pct = 0.
                        FOR EACH b-ar-invl FIELDS(amt) WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                            ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                            ACCUMULATE 1 (TOTAL).
                        END.
                        ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                               (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                            ELSE (ACCUM TOTAL 1))
                            ELSE (ar-invl.amt / ld-inv-pct).

                        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

                        ld-csh-pct = 0.
                        FOR EACH b-ar-cashl FIELDS(amt-paid amt-disc)
                            WHERE b-ar-cashl.c-no   EQ ar-cashl.c-no
                            AND b-ar-cashl.inv-no EQ ar-cashl.inv-no
                            NO-LOCK:
                            ld-csh-pct = ld-csh-pct + (b-ar-cashl.amt-paid - b-ar-cashl.amt-disc).
                        END.
                        ld-csh-pct = (ar-cashl.amt-paid - ar-cashl.amt-disc) / ld-csh-pct.

                        IF ld-csh-pct EQ ? THEN ld-csh-pct = 0.

                        ASSIGN
                            v-amt  = v-amt * ld-inv-pct
                            v-cost = ar-invl.t-cost * (v-slsp[1] / 100) * -1 * ld-csh-pct.
                    END.
                END.
                ELSE
                    ASSIGN
                        v-qty  = 0
                        v-msf  = 0
                        v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
                        v-cost = 0.

            END. /*avail ar-cashl*/

            IF v-i-no NE "" AND v-ord-no NE 0 THEN
            DO:
                FIND FIRST oe-ordl WHERE
                    oe-ordl.company EQ tt-sales-forecast.company AND
                    oe-ordl.ord-no  EQ v-ord-no AND
                    oe-ordl.i-no    EQ v-i-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                    v-cost = oe-ordl.cost * v-qty / 1000.
            END.

            IF v-cost EQ ? THEN v-cost = 0.

            {sys/inc/roundup.i v-qty}

            ASSIGN
                tt-sales-forecast.invoiced-amt  = tt-sales-forecast.invoiced-amt + v-amt
                tt-sales-forecast.invoiced-qty  = tt-sales-forecast.invoiced-qty + v-qty
                tt-sales-forecast.invoiced-msf  = tt-sales-forecast.invoiced-msf + v-msf
                tt-sales-forecast.invoiced-cost = tt-sales-forecast.invoiced-cost + v-cost.

        END. /*each tt-report */
    END. /* each tt-sales-forecast */

    FOR EACH tt-sales-forecast:
        tt-sales-forecast.invoiced-profit = ROUND(tt-sales-forecast.invoiced-amt -
            tt-sales-forecast.invoiced-cost,2).

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-op-proc C-Win 
PROCEDURE raw-op-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* OR10 for orders booked, except using order date instead of due date*/
    DEFINE VARIABLE v-date            AS DATE    NO-UNDO.
    DEFINE VARIABLE v-price           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-oe-gp           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-start-last-year AS DATE    NO-UNDO.
    DEFINE VARIABLE v-end-this-year   AS DATE    NO-UNDO.

    ASSIGN
        v-start-last-year = DATE(1,1,YEAR(fi_as-of-date) - 1)
        v-end-this-year   = DATE(12,31,YEAR(fi_as-of-date)).

    EMPTY TEMP-TABLE tt-raw-op.

    DO v-date = DATE(1,1,YEAR(fi_as-of-date) - 1) TO
        DATE(12,31,YEAR(fi_as-of-date)):

        CREATE tt-raw-op.
        tt-raw-op.DATE = v-date.
        RELEASE tt-raw-op.
    END.

    FOR EACH oe-ord FIELDS(company ord-no ord-date TYPE) WHERE
        oe-ord.company  EQ fi_company AND
        oe-ord.ord-date GE v-start-last-year AND
        oe-ord.ord-date LE v-end-this-year AND
        oe-ord.type     NE "T"
        NO-LOCK
        USE-INDEX ordate,
        EACH oe-ordl FIELDS(t-price qty company ord-no cost) WHERE
        oe-ordl.company EQ oe-ord.company AND
        oe-ordl.ord-no  EQ oe-ord.ord-no
        NO-LOCK,
        FIRST itemfg FIELDS(company i-no t-sqft weight-100) WHERE
        itemfg.company EQ oe-ord.company AND
        itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK,
        FIRST tt-raw-op WHERE
        tt-raw-op.DATE EQ oe-ord.ord-date:

        ASSIGN
            tt-raw-op.oe-dollars  = tt-raw-op.oe-dollars + oe-ordl.t-price
            tt-raw-op.oe-qty      = tt-raw-op.oe-qty 
                         + oe-ordl.qty
            tt-raw-op.oe-qty-msf  = tt-raw-op.oe-qty-msf 
                             + (itemfg.t-sqft * oe-ordl.qty / 1000)
            tt-raw-op.oe-qty-tons = tt-raw-op.oe-qty-tons
                              +( itemfg.weight-100 * oe-ordl.qty / 100 / 2000)
            v-oe-gp               = (IF oe-ordl.t-price NE 0 THEN
                   ((oe-ordl.t-price - (oe-ordl.cost * (oe-ordl.qty / 1000) ) )  
                     / oe-ordl.t-price * 100)
                   ELSE 0)
            v-oe-gp               = IF v-oe-gp EQ ? THEN 0 ELSE v-oe-gp
            tt-raw-op.oe-gp       = tt-raw-op.oe-gp 
                        + v-oe-gp.

    END.

    RUN raw-op-rel-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-op-rel-proc C-Win 
PROCEDURE raw-op-rel-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*OR2*/
    DEFINE VARIABLE v-types      AS CHARACTER INIT "PALSBICZ" NO-UNDO.
    DEFINE VARIABLE v-type       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-start-date AS DATE      NO-UNDO.
    DEFINE VARIABLE v-end-date   AS DATE      NO-UNDO.
    DEFINE VARIABLE lv-qty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-date       AS DATE      NO-UNDO.
    DEFINE VARIABLE v-rel-gp     AS DECIMAL   NO-UNDO.

    ASSIGN
        v-start-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
        v-end-date   = DATE(12,31,YEAR(fi_as-of-date)). 

    EMPTY TEMP-TABLE w-ord.
    EMPTY TEMP-TABLE tt-report.

   FOR EACH oe-ordl FIELDS(company opened ord-no LINE i-no)
      WHERE oe-ordl.company EQ fi_company
        AND oe-ordl.opened  EQ YES
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,
      FIRST oe-ord FIELDS(company ord-no)
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
      NO-LOCK:

/* RUN oe/cleanrel.p (ROWID(oe-ordl)). */

FOR EACH oe-rel FIELDS(company cust-no ord-no i-no LINE rel-date) NO-LOCK
    WHERE oe-rel.company   EQ oe-ordl.company
    AND oe-rel.ord-no    EQ oe-ordl.ord-no
    AND oe-rel.i-no      EQ oe-ordl.i-no
    AND oe-rel.line      EQ oe-ordl.line
    AND oe-rel.rel-date  GE v-start-date
    AND oe-rel.rel-date  LE v-end-date
    USE-INDEX ord-item:

    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

    IF INDEX("AB",v-type) GT 0 THEN NEXT.

    IF INDEX(v-types,v-type) GT 0 THEN 
    DO:
        CREATE tt-report.
        ASSIGN
            tt-report.key-06 = v-type
            tt-report.rec-id = RECID(oe-rel).
    END.
END.

FOR EACH oe-rell FIELDS(r-no company ord-no i-no LINE b-ord-no po-no
    qty rel-no) NO-LOCK
    WHERE oe-rell.company EQ oe-ordl.company
    AND oe-rell.ord-no  EQ oe-ordl.ord-no
    AND oe-rell.i-no    EQ oe-ordl.i-no
    AND oe-rell.line    EQ oe-ordl.line
    AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
    (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
    USE-INDEX ord-no,

    FIRST oe-relh FIELDS(cust-no r-no posted deleted rel-date) NO-LOCK
    WHERE oe-relh.r-no     EQ oe-rell.r-no
    AND oe-relh.posted   EQ NO
    AND oe-relh.deleted  EQ NO
    AND oe-relh.rel-date GE v-start-date
    AND oe-relh.rel-date LE v-end-date

    USE-INDEX r-no

    BREAK BY oe-rell.r-no
    BY oe-rell.ord-no
    BY oe-rell.i-no
    BY oe-rell.line
    BY oe-rell.rel-no
    BY oe-rell.b-ord-no
    BY oe-rell.po-no:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) THEN 
    DO:
        CREATE tt-report.
        ASSIGN
            tt-report.key-06 = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
            tt-report.qty    = lv-qty
            tt-report.rec-id = RECID(oe-rell).
    END.
END.
END.

IF NOT CAN-FIND(FIRST tt-report) THEN 
DO:
    CREATE tt-report.
END.

RELEASE tt-report.

FOR EACH tt-report:

    RELEASE oe-rel.
    RELEASE oe-rell.
    RELEASE oe-relh.
    RELEASE oe-ord.
    RELEASE oe-ordl.

    FIND FIRST oe-rel 
        WHERE RECID(oe-rel) EQ tt-report.rec-id 
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-rel THEN 
    DO:
        FOR EACH oe-rell FIELDS(company ord-no rel-no b-ord-no i-no LINE
            r-no) NO-LOCK
            WHERE oe-rell.company  EQ fi_company
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            USE-INDEX ord-no,
            FIRST oe-relh FIELDS(cust-no r-no posted deleted) WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:

            IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
                tt-report.rec-id = RECID(oe-rell).
            ELSE RELEASE oe-relh.

            LEAVE.
        END.

        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ fi_company
            AND oe-ordl.ord-no  EQ oe-rel.ord-no
            AND oe-ordl.i-no    EQ oe-rel.i-no
            AND oe-ordl.line    EQ oe-rel.line
            NO-LOCK.
    END.

    FIND oe-rell WHERE RECID(oe-rell) EQ tt-report.rec-id NO-LOCK NO-ERROR.
    IF AVAILABLE oe-rell THEN 
    DO:    
        IF INDEX("SLI",tt-report.key-06) GT 0 THEN
            tt-report.key-06 = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B" .

        FIND FIRST oe-relh
            WHERE oe-relh.company EQ fi_company
            AND oe-relh.r-no    EQ oe-rell.r-no
            USE-INDEX r-no NO-LOCK.

        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ fi_company
            AND oe-ordl.ord-no  EQ oe-rell.ord-no
            AND oe-ordl.i-no    EQ oe-rell.i-no
            AND oe-ordl.line    EQ oe-rell.line
            NO-LOCK.
    END.

    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN
        FIND FIRST cust
            WHERE cust.company EQ fi_company
            AND cust.cust-no EQ oe-ord.cust-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE oe-relh THEN
        ASSIGN
            v-qty  = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty
            v-date = oe-relh.rel-date.
    ELSE
        IF AVAILABLE oe-rel THEN
            ASSIGN
                v-qty  = oe-rel.qty 
                v-date = oe-rel.rel-date.

    IF AVAILABLE oe-ordl THEN 
    DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ fi_company
            AND itemfg.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE itemfg THEN
        DO:
            CREATE w-ord.
            ASSIGN
                w-ord.cost     = oe-ordl.cost
                w-ord.price    = oe-ordl.t-price / oe-ordl.qty
                w-ord.rel-qty  = v-qty
                w-ord.t-price  = w-ord.price * w-ord.rel-qty
                w-ord.rel-date = v-date
                w-ord.msf      = w-ord.rel-qty * itemfg.t-sqft / 1000
                w-ord.tons     = itemfg.weight-100 * oe-ordl.qty / 100 / 2000.
            RELEASE w-ord.
        END.
    END.
END. /*each tt-report*/

FOR EACH w-ord,
    FIRST tt-raw-op WHERE
    tt-raw-op.DATE EQ w-ord.rel-date:

    ASSIGN
        tt-raw-op.rel-dollars  = tt-raw-op.rel-dollars + w-ord.t-price
        tt-raw-op.rel-qty      = tt-raw-op.rel-qty 
                          + w-ord.rel-qty
        tt-raw-op.rel-qty-msf  = tt-raw-op.rel-qty-msf 
                              + w-ord.msf
        tt-raw-op.rel-qty-tons = tt-raw-op.rel-qty-tons
                               + w-ord.tons
        v-rel-gp               = (IF w-ord.t-price NE 0 THEN
                   ((w-ord.t-price - (w-ord.cost * (w-ord.rel-qty / 1000) ) )  
                     / w-ord.t-price * 100)
                   ELSE 0)
        v-rel-gp               = IF v-rel-gp EQ ? THEN 0 ELSE v-rel-gp
        tt-raw-op.rel-gp       = tt-raw-op.rel-gp 
                         + v-rel-gp.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-cat-proc C-Win 
PROCEDURE raw-prod-cat-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-amt           LIKE ar-inv.gross FORMAT "->,>>>,>>9.99" NO-UNDO.    
    DEFINE VARIABLE v-cost          LIKE itemfg.t-sqft FORMAT "->,>>9.999" NO-UNDO.
    DEFINE VARIABLE v-sqft          LIKE ar-inv.t-cost FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-msf           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-start-of-year AS DATE    NO-UNDO.

    /*similar to HF report*/

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-data.

    v-start-of-year = DATE(1,1,YEAR(fi_as-of-date)).

    FOR EACH cust FIELDS(company cust-no) WHERE cust.company EQ fi_company NO-LOCK:
        FOR EACH ar-inv FIELDS(company posted cust-no inv-date x-no)
            WHERE ar-inv.company  EQ fi_company
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE v-start-of-year
            AND ar-inv.inv-date LE fi_as-of-date
            AND ar-inv.type    NE "FC"
            NO-LOCK,
            EACH ar-invl FIELDS(x-no billable misc i-no actnum)
            WHERE ar-invl.x-no EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK:

            CREATE tt-report.

            ASSIGN
                tt-report.rec-id = RECID(ar-invl)
                tt-report.key-01 = "MISC".

            IF NOT ar-invl.misc THEN 
            DO:

                RELEASE itemfg.

                IF ar-invl.i-no NE "" THEN
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ fi_company
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.

                IF AVAILABLE itemfg THEN tt-report.key-01 = itemfg.procat.

                ELSE 
                DO:
                    FIND FIRST fgcat
                        WHERE fgcat.company EQ fi_company
                        AND fgcat.glacc   EQ ar-invl.actnum
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE fgcat THEN tt-report.key-01 = fgcat.procat.
                END.
            END.

            tt-report.key-02 = /*if v-misc and tt-report.key-01 eq "MISC"
                           then ar-invl.actnum else*/ tt-report.key-01.
        END. /*each ar-inv*/

        FOR EACH ar-cash FIELDS(company cust-no check-date posted c-no)
            WHERE ar-cash.company    EQ fi_company
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE v-start-of-year
            AND ar-cash.check-date LE fi_as-of-date
            AND ar-cash.posted     EQ YES
            NO-LOCK,

            EACH ar-cashl FIELDS(c-no posted memo company actnum dscr)
            WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
            WHERE account.company EQ ar-cashl.company
            AND account.actnum  EQ ar-cashl.actnum
            AND account.type    EQ "R")
            NO-LOCK:

            CREATE tt-report.

            ASSIGN
                tt-report.key-01 = "MEMO"
                tt-report.key-02 = ar-cashl.actnum
                tt-report.rec-id = RECID(ar-cashl).

            RELEASE itemfg.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE reftable                      OR
                ar-cashl.dscr MATCHES "*OE RETURN*" THEN 
            DO:

                RELEASE itemfg.

                IF AVAILABLE oe-retl AND oe-retl.i-no NE "" THEN
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ fi_company
                        AND itemfg.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                tt-report.key-01 = IF AVAILABLE itemfg THEN itemfg.procat ELSE "MISC".
            END.

            tt-report.key-02 = /*if v-misc and tt-report.key-01 eq "MISC"
                       then ar-cashl.actnum else*/ tt-report.key-01.
        END.

    END. /*each cust*/

    FOR EACH tt-report,
        FIRST tt-sales-prod-cat WHERE
        tt-sales-prod-cat.prod-cat EQ tt-report.key-02
        BREAK BY tt-report.key-01
        BY tt-report.key-02:

        FIND FIRST w-data NO-ERROR.

        IF FIRST-OF(tt-report.key-02) THEN CREATE w-data.

        FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

            RELEASE itemfg.

            IF ar-invl.i-no NE "" THEN
                FIND FIRST itemfg
                    WHERE itemfg.company EQ fi_company
                    AND itemfg.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.

            ASSIGN
                v-amt  = ar-invl.amt
                v-cost = ar-invl.t-cost
                v-msf  = IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                    ELSE
                    IF AVAILABLE itemfg THEN
                      (itemfg.t-sqft * ar-invl.ship-qty / 1000) ELSE 0
                v-sqft = v-msf * 1000.

            IF v-amt  EQ ? THEN v-amt  = 0.
            IF v-cost EQ ? THEN v-cost = 0.
            IF v-sqft EQ ? THEN v-sqft = 0.
            IF v-msf EQ ? THEN v-msf = 0.

            IF ar-inv.inv-date EQ tt-sales-prod-cat.DATE THEN
                ASSIGN
                    w-data.w-sqft[1] = w-data.w-sqft[1] + v-sqft
                    w-data.w-amt[1]  = w-data.w-amt[1]  + v-amt  
                    w-data.w-cost[1] = w-data.w-cost[1] + v-cost 
                    w-data.w-msf[1]  = w-data.w-msf[1]  + v-msf.

            IF ar-inv.inv-date LE fi_as-of-date THEN
            DO:
                IF MONTH(ar-inv.inv-date) EQ MONTH(fi_as-of-date) THEN
                    ASSIGN
                        w-data.w-sqft[2] = w-data.w-sqft[2] + v-sqft
                        w-data.w-amt[2]  = w-data.w-amt[2]  + v-amt 
                        w-data.w-cost[2] = w-data.w-cost[2] + v-cost
                        w-data.w-msf[2]  = w-data.w-msf[2]  + v-msf.

                ASSIGN
                    w-data.w-sqft[3] = w-data.w-sqft[3] + v-sqft
                    w-data.w-amt[3]  = w-data.w-amt[3]  + v-amt 
                    w-data.w-cost[3] = w-data.w-cost[3] + v-cost
                    w-data.w-msf[3]  = w-data.w-msf[3]  + v-msf.
            END.
        END.
        ELSE 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
                    v-msf  = 0
                    v-cost = 0
                    v-sqft = 0.

                RELEASE itemfg.
                RELEASE ar-invl.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:

                    IF oe-retl.i-no NE "" THEN
                        FIND FIRST itemfg
                            WHERE itemfg.company EQ fi_company
                            AND itemfg.i-no    EQ oe-retl.i-no
                            NO-LOCK NO-ERROR.

                    ASSIGN
                        v-msf  = IF AVAILABLE itemfg THEN
                          oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                        ELSE 0
                        v-sqft = v-msf * 1000.

                    IF v-sqft EQ ? THEN v-sqft = 0.
                    IF v-msf EQ ? THEN v-msf = 0.

                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ fi_company
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN
                        RUN salrep/salecost.p (3,
                            ROWID(ar-invl),
                            oe-retl.job-no,
                            oe-retl.job-no,
                            oe-retl.tot-qty-return,
                            OUTPUT v-cost).
                END.

                IF ar-cash.check-date EQ tt-sales-prod-cat.DATE THEN
                    ASSIGN
                        w-data.w-sqft[1] = w-data.w-sqft[1] - v-sqft
                        w-data.w-amt[1]  = w-data.w-amt[1]  + v-amt 
                        w-data.w-cost[1] = w-data.w-cost[1] - v-cost
                        w-data.w-msf[1]  = w-data.w-msf[1]  - v-msf.

                IF ar-cash.check-date LE fi_as-of-date THEN
                DO:
                    IF MONTH(ar-cash.check-date) EQ MONTH(fi_as-of-date) THEN
                        ASSIGN
                            w-data.w-sqft[2] = w-data.w-sqft[2] - v-sqft
                            w-data.w-amt[2]  = w-data.w-amt[2]  + v-amt 
                            w-data.w-cost[2] = w-data.w-cost[2] - v-cost
                            w-data.w-msf[2]  = w-data.w-msf[2]  - v-msf.

                    ASSIGN
                        w-data.w-sqft[3] = w-data.w-sqft[3] - v-sqft 
                        w-data.w-amt[3]  = w-data.w-amt[3]  + v-amt
                        w-data.w-cost[3] = w-data.w-cost[3] - v-cost
                        w-data.w-msf[3]  = w-data.w-msf[3]  - v-msf.
                END.
            END.
        END.

        IF LAST-OF(tt-report.key-02) THEN
        DO:
            ASSIGN
                tt-sales-prod-cat.date-sf   = tt-sales-prod-cat.date-sf + w-data.w-sqft[1]
                tt-sales-prod-cat.date-amt  = tt-sales-prod-cat.date-amt + w-data.w-amt[1]
                tt-sales-prod-cat.date-msf  = tt-sales-prod-cat.date-msf + w-data.w-msf[1]
                tt-sales-prod-cat.date-cost = tt-sales-prod-cat.date-cost + w-data.w-cost[1]
                tt-sales-prod-cat.mtd-sf    = tt-sales-prod-cat.mtd-sf + w-data.w-sqft[2] 
                tt-sales-prod-cat.mtd-amt   = tt-sales-prod-cat.mtd-amt + w-data.w-amt[2]
                tt-sales-prod-cat.mtd-msf   = tt-sales-prod-cat.mtd-msf + w-data.w-msf[2]
                tt-sales-prod-cat.mtd-cost  = tt-sales-prod-cat.mtd-cost + w-data.w-cost[2]
                tt-sales-prod-cat.ytd-sf    = tt-sales-prod-cat.ytd-sf + w-data.w-sqft[3]
                tt-sales-prod-cat.ytd-amt   = tt-sales-prod-cat.ytd-amt + w-data.w-amt[3]
                tt-sales-prod-cat.ytd-msf   = tt-sales-prod-cat.ytd-msf +  w-data.w-msf[3]
                tt-sales-prod-cat.ytd-cost  = tt-sales-prod-cat.ytd-cost + w-data.w-cost[3].

            DELETE w-data.
        END.
    END.

    FOR EACH tt-sales-prod-cat:
        ASSIGN
            tt-sales-prod-cat.date-profit = IF tt-sales-prod-cat.date-amt NE 0 THEN
                                           (tt-sales-prod-cat.date-amt - tt-sales-prod-cat.date-cost) /
                                           tt-sales-prod-cat.date-amt * 100
                                        ELSE 0
            tt-sales-prod-cat.mtd-profit  = IF tt-sales-prod-cat.mtd-amt NE 0 THEN
                                          (tt-sales-prod-cat.mtd-amt - tt-sales-prod-cat.mtd-cost) /
                                          tt-sales-prod-cat.mtd-amt * 100
                                       ELSE 0
            tt-sales-prod-cat.ytd-profit  = IF tt-sales-prod-cat.ytd-amt NE 0 THEN
                                          (tt-sales-prod-cat.ytd-amt - tt-sales-prod-cat.ytd-cost) /
                                          tt-sales-prod-cat.ytd-amt * 100
                                       ELSE 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-proc C-Win 
PROCEDURE raw-prod-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*DR1*/
    DEFINE VARIABLE v-on               AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-out              AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-up               AS INTEGER NO-UNDO.
    DEFINE VARIABLE std-hrs-var        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE tot-hrs-var        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-start-of-year    AS DATE    NO-UNDO.
    DEFINE VARIABLE v-month-as-of-date AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE tt-report.

    ASSIGN
        v-start-of-year    = DATE(1,1,YEAR(fi_as-of-date))
        v-month-as-of-date = MONTH(fi_as-of-date).

    FOR EACH tt-raw-prod,
        EACH mch-act FIELDS(company m-code op-date job frm blank-no dept pass
        CODE qty op-date hours) WHERE
        mch-act.company EQ fi_company AND
        mch-act.m-code  EQ tt-raw-prod.m-code AND
        mch-act.op-date GE v-start-of-year AND
        mch-act.op-date LE fi_as-of-date
        NO-LOCK:

        FIND FIRST work-tmp WHERE
            work-tmp.job = mch-act.job AND
            work-tmp.frm = mch-act.frm AND
            work-tmp.blank-no = mch-act.blank-no AND
            work-tmp.dept = mch-act.dept AND
            work-tmp.m-code = mch-act.m-code AND
            work-tmp.pass = mch-act.pass
            NO-ERROR.

        FIND FIRST b-mach WHERE
            b-mach.company EQ fi_company AND
            b-mach.loc EQ locode AND
            b-mach.m-code  EQ mch-act.m-code
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE work-tmp THEN
        DO:
            CREATE work-tmp.
            ASSIGN
                work-tmp.job      = mch-act.job
                work-tmp.frm      = mch-act.frm
                work-tmp.blank-no = mch-act.blank-no
                work-tmp.dept     = IF mch-act.dept NE "" THEN mch-act.dept
                                 ELSE IF AVAILABLE b-mach THEN b-mach.dept[1] ELSE ""
                work-tmp.m-code   = mch-act.m-code
                work-tmp.pass     = mch-act.pass.
        END.

        FIND job-code WHERE
            job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.

        IF AVAILABLE job-code THEN
        DO:
            IF job-code.cat EQ "RUN" THEN 
            DO:

                work-tmp.qty = work-tmp.qty
                    + IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.

                IF work-tmp.qty EQ ? THEN work-tmp.qty = 0.

                IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    ASSIGN
                        tt-raw-prod.date-qty     = tt-raw-prod.date-qty
                                         + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                        tt-raw-prod.date-run-hrs = tt-raw-prod.date-run-hrs
                                             + mch-act.hours.

                IF mch-act.op-date LE fi_as-of-date THEN
                DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                        ASSIGN
                            tt-raw-prod.mtd-qty     = tt-raw-prod.mtd-qty
                                           + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                            tt-raw-prod.mtd-run-hrs = tt-raw-prod.mtd-run-hrs
                                               + mch-act.hours.

                    ASSIGN
                        tt-raw-prod.ytd-qty     = tt-raw-prod.ytd-qty
                                        + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                        tt-raw-prod.ytd-run-hrs = tt-raw-prod.ytd-run-hrs
                                            + mch-act.hours.
                END.

                FOR EACH job-hdr FIELDS(company job frm i-no est-no frm blank-no n-on) WHERE
                    job-hdr.company   EQ fi_company AND
                    job-hdr.job       EQ work-tmp.job AND
                    job-hdr.frm       EQ work-tmp.frm
                    NO-LOCK,
                    FIRST itemfg FIELDS(company i-no t-sqft) WHERE
                    itemfg.company EQ fi_company AND
                    itemfg.i-no    EQ job-hdr.i-no
                    NO-LOCK:

                    IF NOT (job-hdr.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0) THEN
                        NEXT.

                    ASSIGN
                        v-on  = 1
                        v-out = 1.

                    IF AVAILABLE b-mach AND index("APB",b-mach.p-type) LE 0 THEN 
                    DO:

                        IF job-hdr.blank-no EQ 0 THEN
                            FIND FIRST eb WHERE
                                eb.company   EQ job-hdr.company AND
                                eb.est-no    EQ job-hdr.est-no AND
                                eb.form-no   EQ job-hdr.frm
                                NO-LOCK NO-ERROR.
                        ELSE
                            FIND FIRST eb WHERE
                                eb.company   EQ job-hdr.company AND
                                eb.est-no    EQ job-hdr.est-no AND
                                eb.form-no   EQ job-hdr.frm AND
                                eb.blank-no EQ job-hdr.blank-no
                                NO-LOCK NO-ERROR.

                        IF AVAILABLE eb THEN v-up = eb.num-up.

                        IF job-hdr.n-on NE 0 THEN v-up = job-hdr.n-on.

                        FIND FIRST ef
                            WHERE ef.company EQ job-hdr.company
                            AND ef.est-no  EQ job-hdr.est-no
                            AND ef.form-no EQ job-hdr.frm
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

                        v-on = v-up * v-out.

                        IF mch-act.blank-no NE 0 THEN
                            FIND FIRST est-op
                                WHERE est-op.company EQ job-hdr.company
                                AND est-op.est-no  EQ job-hdr.est-no
                                AND est-op.s-num   EQ mch-act.frm
                                AND est-op.b-num  EQ mch-act.blank-no
                                AND est-op.m-code  EQ mch-act.m-code
                                AND est-op.op-pass EQ mch-act.pass
                                AND est-op.dept    EQ mch-act.dept
                                AND est-op.line    LT 500
                                NO-LOCK NO-ERROR.
                        ELSE
                            FIND FIRST est-op
                                WHERE est-op.company EQ job-hdr.company
                                AND est-op.est-no  EQ job-hdr.est-no
                                AND est-op.s-num   EQ mch-act.frm
                                AND est-op.m-code  EQ mch-act.m-code
                                AND est-op.op-pass EQ mch-act.pass
                                AND est-op.dept    EQ mch-act.dept
                                AND est-op.line    LT 500
                                NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE est-op THEN
                        DO:
                            IF mch-act.blank-no NE 0 THEN
                                FIND FIRST est-op
                                    WHERE est-op.company EQ job-hdr.company
                                    AND est-op.est-no  EQ job-hdr.est-no
                                    AND est-op.s-num   EQ mch-act.frm
                                    AND (est-op.b-num  EQ mch-act.blank-no OR
                                    mch-act.blank-no EQ 0)
                                    AND est-op.op-pass EQ mch-act.pass
                                    AND est-op.dept    EQ mch-act.dept
                                    AND est-op.line    LT 500
                                    NO-LOCK NO-ERROR.
                            ELSE
                                FIND FIRST est-op
                                    WHERE est-op.company EQ job-hdr.company
                                    AND est-op.est-no  EQ job-hdr.est-no
                                    AND est-op.s-num   EQ mch-act.frm
                                    AND est-op.op-pass EQ mch-act.pass
                                    AND est-op.dept    EQ mch-act.dept
                                    AND est-op.line    LT 500
                                    NO-LOCK NO-ERROR.
                        END.

                        IF AVAILABLE est-op THEN
                            RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).

                        ELSE v-out = 1.

                        v-on = v-on / v-out.
                    END.

                    IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                        tt-raw-prod.date-qty-msf = tt-raw-prod.date-qty-msf
                            + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                    IF mch-act.op-date LE fi_as-of-date THEN
                    DO:
                        IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                            tt-raw-prod.mtd-qty-msf = tt-raw-prod.mtd-qty-msf
                                + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                        tt-raw-prod.ytd-qty-msf = tt-raw-prod.ytd-qty-msf
                            + (mch-act.qty * itemfg.t-sqft * v-on / 1000).
                    END.
                END.
            END.
            ELSE
                IF job-code.cat EQ "MR" THEN
                DO:
                    IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                        tt-raw-prod.date-mr-hrs = tt-raw-prod.date-mr-hrs
                            + mch-act.hours.

                    IF mch-act.op-date LE fi_as-of-date THEN
                    DO:
                        IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                            tt-raw-prod.mtd-mr-hrs = tt-raw-prod.mtd-mr-hrs
                                + mch-act.hours.

                        tt-raw-prod.ytd-mr-hrs = tt-raw-prod.ytd-mr-hrs
                            + mch-act.hours.
                    END.
                END.
                ELSE
                    IF job-code.cat EQ "DT" THEN
                    DO:
                        IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                            tt-raw-prod.date-dt-charge = tt-raw-prod.date-dt-charge
                                + mch-act.hours.

                        IF mch-act.op-date LE fi_as-of-date THEN
                        DO:
                            IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                                tt-raw-prod.mtd-dt-charge = tt-raw-prod.mtd-dt-charge
                                    + mch-act.hours.


                            tt-raw-prod.ytd-dt-charge = tt-raw-prod.ytd-dt-charge
                                + mch-act.hours.
                        END.
                    END.
                    ELSE
                    DO:
                        IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                            tt-raw-prod.date-dt-nc = tt-raw-prod.date-dt-nc
                                + mch-act.hours.

                        IF mch-act.op-date LE fi_as-of-date THEN
                        DO:
                            IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                                tt-raw-prod.mtd-dt-nc = tt-raw-prod.mtd-dt-nc
                                    + mch-act.hours.

                            tt-raw-prod.ytd-dt-nc = tt-raw-prod.ytd-dt-nc
                                + mch-act.hours.
                        END.
                    END.
        END. /*avail job-code */

    END. /*each tt-raw-prod*/

    FOR EACH work-tmp,
        FIRST tt-raw-prod WHERE
        tt-raw-prod.m-code EQ work-tmp.m-code:

        IF work-tmp.blank-no NE 0 THEN
            FIND FIRST job-mch WHERE
                job-mch.company  = fi_company AND
                job-mch.job      EQ work-tmp.job AND
                job-mch.frm      = work-tmp.frm AND
                job-mch.blank-no = work-tmp.blank-no AND
                job-mch.m-code   = work-tmp.m-code AND
                job-mch.pass     = work-tmp.pass
                NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST job-mch WHERE
                job-mch.company  = fi_company AND
                job-mch.job      EQ work-tmp.job AND
                job-mch.frm      = work-tmp.frm AND
                job-mch.m-code   = work-tmp.m-code AND
                job-mch.pass     = work-tmp.pass
                NO-LOCK NO-ERROR.

        IF NOT AVAILABLE job-mch THEN
        DO:
            IF work-tmp.blank-no NE 0 THEN
                FIND FIRST job-mch WHERE
                    job-mch.company EQ fi_company AND
                    job-mch.job      EQ work-tmp.job AND
                    job-mch.frm      EQ work-tmp.frm AND
                    job-mch.blank-no = work-tmp.blank-no AND
                    job-mch.m-code   EQ work-tmp.m-code
                    NO-LOCK NO-ERROR.
            ELSE
                FIND FIRST job-mch WHERE
                    job-mch.company EQ fi_company AND
                    job-mch.job      EQ work-tmp.job AND
                    job-mch.frm      EQ work-tmp.frm AND
                    job-mch.m-code   EQ work-tmp.m-code
                    NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ fi_company AND
                job-mch.job     EQ work-tmp.job AND
                job-mch.frm     EQ work-tmp.frm AND
                job-mch.m-code  EQ work-tmp.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ fi_company AND
                job-mch.job     EQ work-tmp.job AND
                job-mch.frm     EQ work-tmp.frm AND
                job-mch.m-code  EQ work-tmp.m-code
                NO-LOCK NO-ERROR.

        IF AVAILABLE job-mch THEN
        DO:
            std-hrs-var = (IF work-tmp.qty NE 0 AND job-mch.speed NE 0 THEN
                work-tmp.qty / job-mch.speed ELSE job-mch.run-hr)
                + job-mch.mr-hr.

            IF job-mch.start-date EQ tt-raw-prod.DATE THEN
                tt-raw-prod.date-std-hrs = tt-raw-prod.date-std-hrs
                    + std-hrs-var.

            IF job-mch.start-date LE fi_as-of-date THEN
            DO:
                IF MONTH(job-mch.start-date) EQ v-month-as-of-date THEN
                    tt-raw-prod.mtd-std-hrs = tt-raw-prod.mtd-std-hrs
                        + std-hrs-var.

                tt-raw-prod.ytd-std-hrs = tt-raw-prod.ytd-std-hrs
                    + std-hrs-var.
            END.
        END.

    END. /*each work-tmp*/

    FOR EACH tt-raw-prod:

        ASSIGN
            tt-raw-prod.date-eff = (IF (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs +
                               tt-raw-prod.date-dt-charge) NE 0 THEN
                               (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge) * 100)
                               ELSE 0)
       tt-raw-prod.date-util = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)
       tt-raw-prod.date-dt-perc = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-dt-nc / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)

       tt-raw-prod.mtd-eff = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-util = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                              tt-raw-prod.mtd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-dt-perc = (IF (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs +
                                 tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.mtd-dt-nc / (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) * 100)
                                 ELSE 0)
       tt-raw-prod.ytd-eff = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-util = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-dt-perc = (IF (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs +
                                 tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.ytd-dt-nc / (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) * 100)
                                 ELSE 0).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-sales-proc C-Win 
PROCEDURE raw-sales-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*from HY */
    DEFINE VARIABLE from-date  AS DATE    NO-UNDO.
    DEFINE VARIABLE to-date    AS DATE    NO-UNDO.
    DEFINE VARIABLE ld-cost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE date-index AS DATE    NO-UNDO.

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-raw-sales.

    ASSIGN 
        from-date = DATE(1,1,YEAR(fi_as-of-date) - 1)
        to-date   = DATE(12,31,YEAR(fi_as-of-date)).

    DO date-index = from-date TO to-date:

        CREATE tt-raw-sales.
        ASSIGN 
            tt-raw-sales.DATE = date-index.
        RELEASE tt-raw-sales.
    END.

    FOR EACH cust WHERE
        cust.company EQ fi_company
        NO-LOCK:

        FOR EACH ar-inv FIELDS() WHERE
            ar-inv.company  EQ fi_company AND
            ar-inv.posted   EQ YES AND
            ar-inv.cust-no  EQ cust.cust-no AND
            ar-inv.inv-date GE from-date AND
            ar-inv.inv-date LE to-date AND
            ar-inv.type    NE "FC" 
            NO-LOCK:

            CREATE tt-report.
            ASSIGN
                tt-report.key-09 = cust.cust-no
                tt-report.key-10 = "ar-inv"
                tt-report.rec-id = RECID(ar-inv).
        END.

        FOR EACH ar-cash FIELDS(c-no ) WHERE 
            ar-cash.company    EQ fi_company AND
            ar-cash.cust-no    EQ cust.cust-no AND
            ar-cash.check-date GE from-date AND
            ar-cash.check-date LE to-date AND
            ar-cash.posted     EQ YES
            USE-INDEX ar-cash
            NO-LOCK,
            EACH ar-cashl FIELDS(company actnum) WHERE
            ar-cashl.c-no    EQ ar-cash.c-no AND
            ar-cashl.posted  EQ YES AND
            ar-cashl.memo    EQ YES AND
            CAN-FIND(FIRST account WHERE
            account.company EQ ar-cashl.company AND
            account.actnum  EQ ar-cashl.actnum AND
            account.type    EQ "R")
            NO-LOCK:

            CREATE tt-report.
            ASSIGN
                tt-report.key-09 = cust.cust-no
                tt-report.key-10 = "ar-cashl"
                tt-report.rec-id = RECID(ar-cashl).
        END.
    END.

    FOR EACH tt-report:

        IF tt-report.key-10 EQ "ar-inv" THEN 
        DO:
            FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

            FIND FIRST tt-raw-sales WHERE
                tt-raw-sales.DATE EQ ar-inv.inv-date.

            FOR EACH ar-invl
                WHERE ar-invl.x-no    EQ ar-inv.x-no
                NO-LOCK:

                FIND FIRST itemfg
                    WHERE itemfg.company EQ fi_company
                    AND itemfg.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.

                RUN salrep/salecost.p (3, /*Invoice Cost*/
                    ROWID(ar-invl),
                    ar-invl.job-no,
                    ar-invl.job-no2,
                    ar-invl.ship-qty,
                    OUTPUT ld-cost).

                ASSIGN
                    tt-raw-sales.date-qty  = tt-raw-sales.date-qty +
                                 ar-invl.ship-qty
                    tt-raw-sales.date-msf  = tt-raw-sales.date-msf +
                                 (IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                  ELSE
                                  IF AVAILABLE itemfg THEN
                                    (ar-invl.ship-qty * itemfg.t-sqft / 1000)
                                  ELSE 0)
                    tt-raw-sales.date-cost = tt-raw-sales.date-cost + ld-cost
                    tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                  ((IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                                    ELSE
                                    IF AVAILABLE itemfg THEN
                                      (ar-invl.ship-qty * itemfg.weight-100 / 100)
                                    ELSE 0) / 2000)
                    tt-raw-sales.date-amt  = tt-raw-sales.date-amt + ar-invl.amt.
            END.
        END.

        ELSE
            IF tt-report.key-10 EQ "ar-cashl" THEN 
            DO:
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                FIND FIRST tt-raw-sales WHERE
                    tt-raw-sales.DATE EQ ar-cash.check-date.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ fi_company
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        AND (ar-invl.billable OR NOT ar-invl.misc)
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:

                        FIND FIRST itemfg
                            WHERE itemfg.company EQ fi_company
                            AND itemfg.i-no    EQ ar-invl.i-no
                            NO-LOCK NO-ERROR.

                        RUN salrep/salecost.p (3, /*Invoice Cost*/
                            ROWID(ar-invl),
                            oe-retl.job-no,
                            oe-retl.job-no2,
                            oe-retl.tot-qty-return,
                            OUTPUT ld-cost).

                        ASSIGN
                            tt-raw-sales.date-qty  = tt-raw-sales.date-qty -
                                   oe-retl.tot-qty-return
                            tt-raw-sales.date-msf  = tt-raw-sales.date-msf -
                                  (IF AVAILABLE itemfg THEN
                                  (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                                  ELSE 0)
                            tt-raw-sales.date-tons = tt-raw-sales.date-tons +
                                    ((IF AVAILABLE itemfg THEN
                                     (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                                     ELSE 0) / 2000)
                            tt-raw-sales.date-cost = tt-raw-sales.date-cost + ld-cost.

                    END.
                END.

                tt-raw-sales.date-amt = tt-raw-sales.date-amt + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END.
    END.

    FOR EACH tt-raw-sales:
        tt-raw-sales.date-net-profit = IF tt-raw-sales.date-amt NE 0 THEN
            (tt-raw-sales.date-amt - tt-raw-sales.date-cost) /
            tt-raw-sales.date-amt
            ELSE 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-salesmen-proc C-Win 
PROCEDURE raw-salesmen-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-pct           AS DECIMAL FORMAT "99.99" NO-UNDO.
    DEFINE VARIABLE v-amt           LIKE ar-inv.gross FORMAT "->,>>>,>>9.99" NO-UNDO.    
    DEFINE VARIABLE v-cost          LIKE itemfg.t-sqft FORMAT "->,>>9.999" NO-UNDO.
    DEFINE VARIABLE v-sqft          LIKE ar-inv.t-cost FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE ld-inv-pct      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-start-of-year AS DATE    NO-UNDO.
    DEFINE VARIABLE v-end-of-year   AS DATE    NO-UNDO.
    DEFINE VARIABLE v-this-month    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-index         AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-data.

    ASSIGN
        v-start-of-year = DATE(1,1,YEAR(fi_as-of-date))
        v-end-of-year   = DATE(12,31,YEAR(fi_as-of-date))
        v-this-month    = MONTH(fi_as-of-date).

    /*from HT*/
    FOR EACH ar-inv FIELDS(company cust-no x-no inv-date)
        WHERE ar-inv.company  EQ fi_company
        AND ar-inv.posted   EQ YES
        AND ar-inv.inv-date GE v-start-of-year
        AND ar-inv.inv-date LE v-end-of-year
        AND ar-inv.type    NE "FC"
        NO-LOCK,
        FIRST cust FIELDS(sman)
        WHERE cust.company EQ ar-inv.company
        AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK,

        EACH ar-invl FIELDS(sman s-pct i-no actnum)
        WHERE ar-invl.x-no EQ ar-inv.x-no
        AND (ar-invl.billable OR NOT ar-invl.misc)
        NO-LOCK:

        {sa/sa-sman6.i ar-inv.inv-date "ar-invl" }
    END. /*each ar-inv*/

    FOR EACH cust FIELDS(cust-no sman)
        WHERE cust.company EQ fi_company
        NO-LOCK,
        EACH ar-cash FIELDS(c-no cust-no check-date)
        WHERE ar-cash.company    EQ fi_company
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE v-start-of-year
        AND ar-cash.check-date LE v-end-of-year
        AND ar-cash.posted     EQ YES
        NO-LOCK,

        EACH ar-cashl FIELDS(company actnum inv-no dscr c-no amt-paid amt-disc)
        WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
        WHERE account.company EQ ar-cashl.company
        AND account.actnum  EQ ar-cashl.actnum
        AND account.type    EQ "R")
        NO-LOCK:

        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        IF AVAILABLE oe-retl THEN
            FIND FIRST ar-invl
                WHERE ar-invl.company EQ fi_company
                AND ar-invl.cust-no EQ ar-cash.cust-no
                AND ar-invl.inv-no  EQ ar-cashl.inv-no
                AND ar-invl.i-no    EQ oe-retl.i-no
                AND (ar-invl.billable OR NOT ar-invl.misc)
                NO-LOCK NO-ERROR.

        IF ar-cashl.inv-no NE 0                                                       AND
            (AVAILABLE ar-invl                             OR
            (NOT AVAILABLE reftable AND
            NOT ar-cashl.dscr MATCHES "*oe return*") OR
            SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
            FOR EACH b-ar-invl FIELDS(billable misc sman s-pct i-no actnum)
                WHERE b-ar-invl.company EQ ar-cashl.company
                AND b-ar-invl.cust-no EQ cust.cust-no
                AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
                NO-LOCK:

                IF NOT (b-ar-invl.billable OR NOT b-ar-invl.misc) THEN NEXT.
                IF NOT (NOT AVAILABLE ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl)) THEN NEXT.

                {sa/sa-sman6.i ar-cash.check-date "ar-cashl" "b-"}
            END.

        ELSE
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.key-02 = cust.sman
                tt-report.rec-id = RECID(ar-cashl)
                tt-report.DATE   = ar-cash.check-date.
        END.
    END. /*each cust*/

    FOR EACH tt-report,
        FIRST tt-raw-salesmen WHERE
        tt-raw-salesmen.sman EQ tt-report.key-02
        BREAK BY tt-report.key-02:

        FIND FIRST w-data
            WHERE w-data.w-sman-no EQ tt-report.key-02
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE w-data THEN 
        DO:
            CREATE w-data.
            w-data.w-sman-no = tt-report.key-02.
        END.

        FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

            FIND FIRST itemfg
                WHERE itemfg.company EQ fi_company
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.

            ASSIGN
                v-pct  = 1
                v-amt  = ar-invl.amt
                v-cost = ar-invl.t-cost
                v-sqft = IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                    ELSE
                    IF AVAILABLE itemfg THEN
                      (itemfg.t-sqft * ar-invl.ship-qty / 1000) ELSE 0.

            IF v-amt  EQ ? THEN v-amt  = 0.
            IF v-cost EQ ? THEN v-cost = 0.
            IF v-sqft EQ ? THEN v-sqft = 0.

            DO i = 1 TO 3:
                IF ar-invl.sman[i] EQ tt-report.key-02 THEN
                    ASSIGN
                        v-pct = ar-invl.s-pct[i] / 100
                        i     = 3.
            END.

            IF v-pct EQ 0 THEN
            DO i = 1 TO 3:
                IF i EQ 1 THEN j = 0.
                IF ar-invl.sman[i] NE "" THEN j = j + 1.
                IF i EQ 3 THEN v-pct = 1 / j.
            END.

            IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.

            IF ar-inv.inv-date EQ tt-raw-salesmen.DATE THEN
                ASSIGN
                    w-data.w-sqft[1] = w-data.w-sqft[1] + (v-sqft * v-pct)
                    w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
                    w-data.w-cost[1] = w-data.w-cost[1] + (v-cost * v-pct).

            IF ar-inv.inv-date LE fi_as-of-date THEN
            DO:
                IF MONTH(ar-inv.inv-date) EQ v-this-month THEN
                DO:
                    ASSIGN
                        w-data.w-sqft[2] = w-data.w-sqft[2] + (v-sqft * v-pct)
                        w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                        w-data.w-cost[2] = w-data.w-cost[2] + (v-cost * v-pct).
                END.

                ASSIGN
                    w-data.w-sqft[3] = w-data.w-sqft[3] + (v-sqft * v-pct)
                    w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
                    w-data.w-cost[3] = w-data.w-cost[3] + (v-cost * v-pct).
            END.

            ASSIGN
                v-index                      = MONTH(ar-inv.inv-date)
                tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt * v-pct)
                tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] + (v-sqft  * v-pct).
        END.
        ELSE 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
                    v-cost = 0
                    v-sqft = 0
                    v-pct  = 1.

                RELEASE itemfg.
                RELEASE ar-invl.
                RELEASE oe-retl.

                FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

                IF NOT AVAILABLE ar-invl THEN
                    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ fi_company
                        AND itemfg.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    v-sqft = IF AVAILABLE itemfg THEN
                        oe-retl.tot-qty-return * itemfg.t-sqft / 1000
                        ELSE 0.

                    IF v-sqft EQ ? THEN v-sqft = 0.

                    RUN salrep/salecost.p(3,
                        ROWID(ar-invl),
                        oe-retl.job-no,
                        oe-retl.job-no,
                        oe-retl.tot-qty-return,
                        OUTPUT v-cost).
                END.
                ELSE
                    IF AVAILABLE ar-invl THEN 
                    DO:
                        ld-inv-pct = 0.
                        FOR EACH b-ar-invl FIELDS(amt) WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                            ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                            ACCUMULATE 1 (TOTAL).
                        END.
                        ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                              (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                           ELSE (ACCUM TOTAL 1))
                           ELSE (ar-invl.amt / ld-inv-pct).

                        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

                        v-amt = v-amt * ld-inv-pct.

                        IF v-sqft EQ ? THEN v-sqft = 0.

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] EQ tt-report.key-02 THEN
                                ASSIGN
                                    v-pct = ar-invl.s-pct[i] / 100
                                    i     = 3.
                        END.

                        IF v-pct EQ 0 THEN
                        DO i = 1 TO 3:
                            IF i EQ 1 THEN j = 0.
                            IF ar-invl.sman[i] NE "" THEN j = j + 1.
                            IF i EQ 3 THEN v-pct = 1 / j.
                        END.

                        IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
                    END.

                IF ar-cash.check-date EQ tt-raw-salesmen.DATE THEN
                    ASSIGN
                        w-data.w-sqft[1] = w-data.w-sqft[1] - (v-sqft * v-pct)
                        w-data.w-amt[1]  = w-data.w-amt[1]  + (v-amt  * v-pct)
                        w-data.w-cost[1] = w-data.w-cost[1] - (v-cost * v-pct).

                IF ar-cash.check-date LE fi_as-of-date THEN
                DO:
                    IF MONTH(ar-cash.check-date) EQ v-this-month THEN
                        ASSIGN
                            w-data.w-sqft[2] = w-data.w-sqft[2] - (v-sqft * v-pct)
                            w-data.w-amt[2]  = w-data.w-amt[2]  + (v-amt  * v-pct)
                            w-data.w-cost[2] = w-data.w-cost[2] - (v-cost * v-pct).

                    ASSIGN
                        w-data.w-sqft[3] = w-data.w-sqft[3] - (v-sqft * v-pct)
                        w-data.w-amt[3]  = w-data.w-amt[3]  + (v-amt  * v-pct)
                        w-data.w-cost[3] = w-data.w-cost[3] - (v-cost * v-pct).
                END.

                ASSIGN
                    v-index                      = MONTH(ar-cash.check-date)
                    tt-raw-salesmen.amt[v-index] = tt-raw-salesmen.amt[v-index] + (v-amt  * v-pct)
                    tt-raw-salesmen.msf[v-index] = tt-raw-salesmen.msf[v-index] - (v-sqft * v-pct).
            END.
        END.

        IF LAST-OF(tt-report.key-02) THEN
        DO:
            ASSIGN
                tt-raw-salesmen.date-msf  = tt-raw-salesmen.date-msf + w-data.w-sqft[1]
                tt-raw-salesmen.date-amt  = tt-raw-salesmen.date-amt + w-data.w-amt[1]
                tt-raw-salesmen.date-sf   = tt-raw-salesmen.date-sf + (w-data.w-sqft[1] * 1000)
                tt-raw-salesmen.date-cost = tt-raw-salesmen.date-cost + w-data.w-cost[1]
                tt-raw-salesmen.mtd-msf   = tt-raw-salesmen.mtd-msf + w-data.w-sqft[2] 
                tt-raw-salesmen.mtd-amt   = tt-raw-salesmen.mtd-amt + w-data.w-amt[2]
                tt-raw-salesmen.mtd-sf    = tt-raw-salesmen.mtd-sf + (w-data.w-sqft[2] * 1000)
                tt-raw-salesmen.mtd-cost  = tt-raw-salesmen.mtd-cost + w-data.w-cost[2]
                tt-raw-salesmen.ytd-msf   = tt-raw-salesmen.ytd-msf + w-data.w-sqft[3]
                tt-raw-salesmen.ytd-amt   = tt-raw-salesmen.ytd-amt + w-data.w-amt[3]
                tt-raw-salesmen.ytd-sf    = tt-raw-salesmen.ytd-sf + (w-data.w-sqft[3] * 1000)
                tt-raw-salesmen.ytd-cost  = tt-raw-salesmen.ytd-cost + w-data.w-cost[3].

            DELETE w-data.
        END.
    END.

    FOR EACH tt-raw-salesmen:

        ASSIGN
            tt-raw-salesmen.date-profit = IF tt-raw-salesmen.date-amt NE 0 THEN
                                          (tt-raw-salesmen.date-amt - tt-raw-salesmen.date-cost) /
                                          tt-raw-salesmen.date-amt * 100
                                       ELSE 0
            tt-raw-salesmen.mtd-profit  = IF tt-raw-salesmen.mtd-amt NE 0 THEN
                                         (tt-raw-salesmen.mtd-amt - tt-raw-salesmen.mtd-cost) /
                                         tt-raw-salesmen.mtd-amt * 100
                                      ELSE 0
            tt-raw-salesmen.ytd-profit  = IF tt-raw-salesmen.ytd-amt NE 0 THEN
                                         (tt-raw-salesmen.ytd-amt - tt-raw-salesmen.ytd-cost) /
                                         tt-raw-salesmen.ytd-amt * 100
                                      ELSE 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-proc C-Win 
PROCEDURE reftable-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO counter = 1 TO BROWSE browse-machine:NUM-SELECTED-ROWS:
        BROWSE browse-machine:FETCH-SELECTED-ROW(counter).

        CREATE tt-raw-prod.
        ASSIGN 
            tt-raw-prod.m-code = mach.m-code
            tt-raw-prod.DATE   = fi_as-of-date.
        RELEASE tt-raw-prod.
    END.

    DO counter = 1 TO BROWSE browse-sales-forecast:NUM-SELECTED-ROWS:
        BROWSE browse-sales-forecast:FETCH-SELECTED-ROW(counter).

        CREATE tt-sales-forecast.
        tt-sales-forecast.company = company.company.
        RELEASE tt-sales-forecast.
    END.

    DO counter = 1 TO BROWSE browse-ar:NUM-SELECTED-ROWS:
        BROWSE browse-ar:FETCH-SELECTED-ROW(counter).

        CREATE tt-ar-dso.
        tt-ar-dso.actnum = account.actnum.
        RELEASE tt-ar-dso.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE released-sales-forecast-proc C-Win 
PROCEDURE released-sales-forecast-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*OR12 Invoice*/

    DEFINE INPUT PARAMETER ip-start-next-month AS DATE NO-UNDO.

    DEFINE VARIABLE v-type       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty        LIKE oe-rel.qty NO-UNDO.
    DEFINE VARIABLE ld           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-price      LIKE oe-ordl.price NO-UNDO.
    DEFINE VARIABLE v-value-head AS LOG       NO-UNDO.

    FOR EACH tt-sales-forecast:

        EMPTY TEMP-TABLE tt-report.

    FOR EACH oe-ordl FIELDS(company ord-no i-no LINE s-man)
        WHERE oe-ordl.company EQ tt-sales-forecast.company
          AND oe-ordl.opened  EQ YES
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
        USE-INDEX opened NO-LOCK:

    /* RUN oe/cleanrel.p (ROWID(oe-ordl)).*/

    FOR EACH oe-rel FIELDS(i-no cust-no)
        WHERE oe-rel.company   EQ tt-sales-forecast.company
        AND oe-rel.ord-no    EQ oe-ordl.ord-no
        AND oe-rel.i-no      EQ oe-ordl.i-no
        AND oe-rel.line      EQ oe-ordl.line
        AND oe-rel.rel-date  GE 01/01/0001
        AND oe-rel.rel-date  LT ip-start-next-month
        NO-LOCK:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

        IF v-type EQ "Z" THEN
        DO li = 1 TO EXTENT(oe-ordl.s-man):
            IF oe-ordl.s-man[li] NE "" THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.rec-id = RECID(oe-rel)
                    tt-report.key-04 = STRING(oe-ordl.ord-no).
                IF li GT 1 THEN
                    tt-report.is-duplicate = YES.
            END.
        END.
    END. /* each oe-rel*/
END. /*each oe-ordl*/

FOR EACH tt-report WHERE tt-report.is-duplicate = NO
    BREAK BY tt-report.key-04:

    RELEASE oe-rel.
    RELEASE oe-rell.

    FIND FIRST oe-rel WHERE
        RECID(oe-rel) EQ tt-report.rec-id 
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-rel THEN 
    DO:
        FOR EACH oe-rell FIELDS(r-no)
            WHERE oe-rell.company  EQ tt-sales-forecast.company
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND CAN-FIND(FIRST oe-relh
            WHERE oe-relh.r-no    EQ oe-rell.r-no
            AND oe-relh.posted  EQ NO
            AND oe-relh.deleted EQ NO)
            USE-INDEX ord-no NO-LOCK:
            tt-report.rec-id = RECID(oe-rell).
            LEAVE.
        END.

        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ tt-sales-forecast.company
            AND oe-ordl.ord-no  EQ oe-rel.ord-no
            AND oe-ordl.i-no    EQ oe-rel.i-no
            AND oe-ordl.line    EQ oe-rel.line
            NO-LOCK.
    END. /* avail oe-rel*/

    FIND FIRST oe-rell
        WHERE RECID(oe-rell) EQ tt-report.rec-id
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-rell THEN
    DO:
        FIND FIRST oe-ordl WHERE
            oe-ordl.company EQ tt-sales-forecast.company AND
            oe-ordl.ord-no  EQ oe-rell.ord-no AND
            oe-ordl.i-no    EQ oe-rell.i-no AND
            oe-ordl.line    EQ oe-rell.line
            NO-LOCK.

        v-qty = oe-rell.qty.
    END.

    ELSE
        v-qty = oe-rel.qty.

    RELEASE b-oe-ordl.
    IF oe-ordl.is-a-component THEN
        FIND FIRST b-oe-ordl
            WHERE b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
            AND b-oe-ordl.line    EQ oe-ordl.set-hdr-line
            AND b-oe-ordl.is-a-component EQ NO
            NO-LOCK NO-ERROR.

    RELEASE b-itemfg.

    v-value-head = NO.

    IF AVAILABLE b-oe-ordl AND tg_set-comp EQ YES THEN
    DO:
        v-value-head = YES.

        IF FIRST-OF(tt-report.key-04) THEN
            v-price = b-oe-ordl.t-price.
        ELSE
            v-price = 0.
    END.
    ELSE
    DO:
        IF AVAILABLE b-oe-ordl THEN
            FIND FIRST b-itemfg
                WHERE b-itemfg.company EQ b-oe-ordl.company
                AND b-itemfg.i-no    EQ b-oe-ordl.i-no
                NO-LOCK NO-ERROR.

        RELEASE itemfg.
        IF AVAILABLE b-itemfg THEN
            FIND FIRST itemfg
                WHERE itemfg.company EQ oe-ordl.company
                AND itemfg.i-no    EQ oe-ordl.i-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE itemfg THEN 
        DO:
            IF itemfg.std-tot-cost NE 0 THEN
                ld = (itemfg.std-tot-cost * oe-ordl.qty) /
                    (b-itemfg.std-tot-cost * b-oe-ordl.qty).
            ELSE
                ld = (itemfg.weight-100 * oe-ordl.qty) /
                    (b-itemfg.weight-100 * b-oe-ordl.qty).

            v-price = b-oe-ordl.t-price * ld / b-oe-ordl.qty.
        END.
        ELSE v-price = oe-ordl.t-price / oe-ordl.qty.
    END.

    IF v-price EQ ? THEN v-price = 0.

    IF v-value-head EQ NO THEN
        tt-sales-forecast.released-amt = tt-sales-forecast.released-amt + (v-price * v-qty).
    ELSE
        tt-sales-forecast.released-amt = tt-sales-forecast.released-amt + v-price.

    ASSIGN
        tt-sales-forecast.released-qty  = tt-sales-forecast.released-qty + v-qty
        tt-sales-forecast.released-cost = tt-sales-forecast.released-cost + ((v-qty / 1000) * oe-ordl.cost).

    RELEASE itemfg.

    FIND FIRST itemfg WHERE
        itemfg.company EQ oe-ordl.company AND
        itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE itemfg THEN
        tt-sales-forecast.released-msf = tt-sales-forecast.released-msf + (v-qty * itemfg.t-sqft / 1000).

END. /* each tt-report*/

END. /*each tt-sales-forecast*/

FOR EACH tt-sales-forecast:
    tt-sales-forecast.released-profit = ROUND(tt-sales-forecast.released-amt -
        tt-sales-forecast.released-cost,2).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DO WITH FRAME {&FRAME-NAME}:

        RUN raw-op-proc. /*Raw OP*/
        RUN raw-prod-proc. /*Raw Production*/
        RUN raw-sales-proc. /*Raw Sales*/
        RUN raw-prod-cat-proc. /*Raw Sales PC*/
        RUN raw-salesmen-proc. /*Raw Salesmen*/
        RUN ap-ar-proc. /*AP/AR*/
        RUN sales-forecast-proc. 
        RUN dso-proc.
        RUN salrep\dashboard.p(INPUT fi_company,
            INPUT fi_as-of-date,
            INPUT v-dso,
            INPUT tg_round).
        RUN saveparameters.
        RUN custom\usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-forecast-proc C-Win 
PROCEDURE sales-forecast-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE start-month      AS DATE NO-UNDO.
    DEFINE VARIABLE start-next-month AS DATE NO-UNDO.

    start-month = DATE(MONTH(fi_as-of-date),1,YEAR(fi_as-of-date)).

    IF MONTH(fi_as-of-date) NE 12 THEN
        start-next-month = DATE(MONTH(fi_as-of-date) + 1,1,YEAR(fi_as-of-date)).
    ELSE
        start-next-month = DATE(1,1,YEAR(fi_as-of-date) + 1).

    RUN invoiced-sales-forecast-proc(INPUT start-month, INPUT start-next-month).
    RUN backlog-sales-forecast-proc(INPUT start-month, INPUT start-next-month).
    RUN released-sales-forecast-proc(INPUT start-next-month).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveparameters C-Win 
PROCEDURE saveparameters :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-user AS CHARACTER NO-UNDO.

    v-user = USERID("NOSWEAT").

    DO TRANSACTION:
        ASSIGN 
            i = 0 .
        FOR EACH tt-raw-prod:
          
            FIND FIRST user-print EXCLUSIVE-LOCK
                WHERE user-print.company    EQ cocode        
                AND user-print.program-id EQ "HM1" 
                AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
            IF NOT AVAILABLE user-print THEN 
            DO:
                CREATE user-print .
                ASSIGN
                    user-print.company    = cocode            
                    user-print.program-id = "HM1"     
                    user-print.user-id    = USERID(LDBNAME(1)) .
            END.
            i = i + 1 .
            ASSIGN 
                user-print.field-value[i] = tt-raw-prod.m-code  .


        END.  /* FOR EACH tt-raw-prod */

      
        i = 0 .
        FOR EACH tt-ar-dso:
          
            FIND FIRST user-print EXCLUSIVE-LOCK
                WHERE user-print.company    EQ cocode        
                AND user-print.program-id EQ "HM1Acct" 
                AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
            IF NOT AVAILABLE user-print THEN 
            DO:
                CREATE user-print .
                ASSIGN
                    user-print.company    = cocode            
                    user-print.program-id = "HM1Acct"     
                    user-print.user-id    = USERID(LDBNAME(1)) .
            END.
            i = i + 1 .
            ASSIGN 
                user-print.field-value[i] = tt-ar-dso.actnum  .

        END.  /* FOR EACH tt-ar-dso */

      
        i = 0.
        FOR EACH tt-sales-forecast:
        
            FIND FIRST user-print EXCLUSIVE-LOCK
                WHERE user-print.company    EQ cocode        
                AND user-print.program-id EQ "HM1SF" 
                AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
            IF NOT AVAILABLE user-print THEN 
            DO:
                CREATE user-print .
                ASSIGN
                    user-print.company    = cocode            
                    user-print.program-id = "HM1SF"     
                    user-print.user-id    = USERID(LDBNAME(1)) .
            END.
            i = i + 1 .
            ASSIGN 
                user-print.field-value[i] = tt-sales-forecast.company  .

        END.  /* tt-sales-forecast */

      
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

