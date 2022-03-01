&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-boomh.w

  Description: Bookings Highlights

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
{salrep/dashbook.i NEW}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD DATE      AS DATE
    FIELD row-id    AS ROWID
    FIELD qty       AS DECIMAL
    FIELD amt       LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc      AS LOG
    FIELD cost      AS DECIMAL
    FIELD msf       AS DECIMAL.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD w-sman-no AS CHARACTER
    FIELD w-sqft    LIKE itemfg.t-sqft FORMAT "->>>9.999" EXTENT 4
    FIELD w-amt     LIKE ar-inv.gross FORMAT "->>>,>>9.99" EXTENT 4
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

DEFINE BUFFER b-oe-ordl FOR oe-ordl.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company 

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
    SIZE 68 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fi_as-of-date AT ROW 2.71 COL 21.2 COLON-ALIGNED
    fi_company AT ROW 2.71 COL 51.6 COLON-ALIGNED
    btn-ok AT ROW 5.52 COL 17.4
    btn-cancel AT ROW 5.52 COL 39.6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    BGCOLOR 15 
    RECT-9 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 72 BY 6.14
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
        TITLE              = "Bookings Highlights"
        HEIGHT             = 6.14
        WIDTH              = 72
        MAX-HEIGHT         = 7.95
        MAX-WIDTH          = 72
        VIRTUAL-HEIGHT     = 7.95
        VIRTUAL-WIDTH      = 72
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bookings Highlights */
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
ON WINDOW-CLOSE OF C-Win /* Bookings Highlights */
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

                ASSIGN {&displayed-objects}.

                EMPTY TEMP-TABLE tt-raw-op.
                EMPTY TEMP-TABLE tt-report.
                EMPTY TEMP-TABLE w-data.

                IF NOT CAN-FIND(FIRST company WHERE
                    company.company EQ fi_company) THEN
                DO:
                    MESSAGE "Invalid Company."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
                    LEAVE.
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

                    APPLY  "close" TO THIS-PROCEDURE.

                END.
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

    ASSIGN
        fi_company    = cocode
        fi_as-of-date = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    /* {custom/usrprint.i} */

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
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
    DISPLAY fi_as-of-date fi_company 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-9 fi_as-of-date fi_company btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
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
        NO-LOCK,
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DO WITH FRAME {&FRAME-NAME}:

        RUN raw-op-proc. /*Raw OP*/
        RUN salrep\dashboard-book.p(INPUT fi_company,
            INPUT fi_as-of-date).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

