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

DEFINE VARIABLE v-pct       AS DECIMAL       FORMAT ">9.99" NO-UNDO.
DEFINE VARIABLE v-date      AS DATE          FORMAT "99/99/9999" INIT TODAY NO-UNDO.

DEFINE VARIABLE v-amt       AS DECIMAL       NO-UNDO.
DEFINE VARIABLE v-cr-db-amt AS DECIMAL       NO-UNDO.
DEFINE VARIABLE v-disc-amt  AS DECIMAL       NO-UNDO.
DEFINE VARIABLE save_id     AS RECID         NO-UNDO.
DEFINE VARIABLE udate       AS DATE          INIT TODAY NO-UNDO.
DEFINE VARIABLE uperiod     AS INTEGER       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tran-date fin-apr end_cust begin_cust ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date fin-apr end_cust begin_cust ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win       AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Cust #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Cust #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fin-apr    AS DECIMAL   FORMAT "->>,>>9.99":U INITIAL 0 
    LABEL "Finance Charge APR" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date  AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.24.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 1.95 COL 45.2 COLON-ALIGNED
    fin-apr AT ROW 3.43 COL 45.2 COLON-ALIGNED
    end_cust AT ROW 5.14 COL 68.4 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 6
    begin_cust AT ROW 5.19 COL 28.4 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 8
    tbAutoClose AT ROW 6.95 COL 28 WIDGET-ID 64
    btn-ok AT ROW 7.95 COL 27.8
    btn-cancel AT ROW 7.95 COL 52
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 9.81
    BGCOLOR 15 FONT 6.


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
        TITLE              = "Finance Charge Creation"
        HEIGHT             = 8.48
        WIDTH              = 94.6
        MAX-HEIGHT         = 46.48
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 46.48
        VIRTUAL-WIDTH      = 256
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Finance Charge Creation */
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
ON WINDOW-CLOSE OF C-Win /* Finance Charge Creation */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust # */
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
        DEFINE VARIABLE choice AS LOG NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        choice = YES.
        MESSAGE "Are you sure you wish to create finance charges?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE choice.
        IF choice THEN RUN bill-finance.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fin-apr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fin-apr C-Win
ON LEAVE OF fin-apr IN FRAME FRAME-A /* Finance Charge APR */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* As of */
    DO:
        ASSIGN {&self-name}.
        DO WITH FRAME {&frame-name}:

            FIND FIRST period                   
                WHERE period.company EQ cocode
                AND period.pst     LE tran-date
                AND period.pend    GE tran-date
                NO-LOCK NO-ERROR.
            IF AVAILABLE period THEN uperiod = (period.pnum).

            ELSE 
            DO:
                MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
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

    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    tran-date = TODAY.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bill-finance C-Win 
PROCEDURE bill-finance :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("general").

    ASSIGN 
        v-date = tran-date
        v-pct  = fin-apr.

    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cocode
        /* gdm - 11050901 */
        AND cust.cust-no GE begin_cust
        AND cust.cust-no LE end_cust
        /* gdm - 11050901 */
        AND cust.fin-chg EQ YES
        AND LOOKUP(cust.active,"A,E") GT 0:

        v-amt = 0.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date LE v-date
            AND ar-inv.terms    NE "CASH"
            AND ar-inv.terms    NE "FCHG"
            NO-LOCK,

            FIRST terms
            WHERE terms.t-code    EQ ar-inv.terms
            AND v-date          GT ar-inv.inv-date + terms.net-days
            NO-LOCK:

            STATUS DEFAULT "Processing...  Customer: " + trim(cust.cust-no) + "   " +
                "Invoice: " + trim(STRING(ar-inv.inv-no)).

            /* Inserted because AR stores gross wrong */
            v-amt = v-amt +
                IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt
                THEN ar-inv.net ELSE ar-inv.gross.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN
                    IF ar-cashl.dscr MATCHES "*CREDIT MEMO CREATED FROM OE RETURN*" THEN
                        v-amt = v-amt - ar-cashl.amt-disc.
                    ELSE
                        IF ar-cashl.amt-paid + ar-cashl.amt-disc NE 0 THEN
                            v-amt = v-amt + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                        ELSE
                            v-amt = v-amt + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
                ELSE
                    v-amt = v-amt + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
            END.
        END. /* each ar-inv */

        FOR EACH ar-cash
            WHERE ar-cash.company     EQ cust.company
            AND ar-cash.cust-no     EQ cust.cust-no
            AND ar-cash.posted      EQ YES
            AND (ar-cash.check-date LE v-date OR
            ar-cash.check-date EQ ?)
            USE-INDEX ar-cash NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no       EQ ar-cash.c-no
            AND ar-cashl.inv-no     EQ 0
            AND ar-cashl.posted     EQ YES
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.inv-no NE 0 THEN 
            DO:
                FIND FIRST ar-inv
                    WHERE ar-inv.company     EQ cust.company
                    AND ar-inv.inv-no      EQ ar-cashl.inv-no
                    AND ar-inv.inv-date    GT v-date
                    USE-INDEX inv-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN NEXT.
            END.

            IF ar-cashl.memo THEN 
            DO:
                /* CTS CM/DM signs are reversed *****************************/
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) LT 0 THEN
                    ASSIGN
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.

                ELSE
                    IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                        ASSIGN
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN
                    v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1.

            v-amt = v-amt + v-cr-db-amt - v-disc-amt.
        END. /* for each ar-cashl record */

        IF v-amt * (v-pct / 100 / 12) GT 0 THEN 
        DO TRANSACTION:
            FIND FIRST shipto
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ cust.cust-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE shipto THEN
                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.      

            CREATE ar-inv.
            ASSIGN
                ar-inv.type         = "FC"
                ar-inv.terms        = "FCHG"
                ar-inv.terms-d      = "Finance Charge"
                ar-inv.company      = cocode
                ar-inv.inv-date     = v-date
                ar-inv.due-date     = v-date
                ar-inv.cust-no      = cust.cust-no
                ar-inv.cust-name    = cust.name
                ar-inv.addr[1]      = cust.addr[1]
                ar-inv.addr[2]      = cust.addr[2]
                ar-inv.city         = cust.city
                ar-inv.state        = cust.state
                ar-inv.zip          = cust.zip
                ar-inv.ship-id      = IF AVAILABLE shipto THEN shipto.ship-id ELSE ""
                ar-inv.carrier      = cust.carrier
                ar-inv.period       = uperiod
                ar-inv.net          = v-amt * (v-pct / 100 / 12)
                ar-inv.due          = ar-inv.net
                ar-inv.gross        = ar-inv.net
                ar-inv.curr-code[1] = cust.curr-code    .

            FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode EXCLUSIVE-LOCK.

            ASSIGN
                ar-inv.inv-no    = ar-ctrl.last-inv + 1
                ar-ctrl.last-inv = ar-inv.inv-no.

            /* {ar/ar-invl.a} */
            x = 1.

            FIND LAST ar-invl WHERE ar-invl.x-no = ar-inv.x-no USE-INDEX x-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE ar-invl THEN x = ar-invl.line + 1.

            CREATE ar-invl.
            ASSIGN 
                ar-invl.x-no    = ar-inv.x-no
                ar-invl.company = cocode
                ar-invl.cust-no = ar-inv.cust-no
                ar-invl.inv-no  = ar-inv.inv-no
                ar-invl.qty     = 0
                ar-invl.line    = x
                ar-invl.po-no   = ar-inv.po-no.

            ASSIGN
                ar-invl.qty        = 1
                ar-invl.unit-pr    = ar-inv.net
                ar-invl.pr-qty-uom = "EA"
                ar-invl.amt        = ar-inv.net
                ar-invl.actnum     = ar-ctrl.onac
                ar-invl.inv-date   = ar-inv.inv-date.

        END.
    END.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Finance Charge Creation Process is completed." VIEW-AS ALERT-BOX.

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
    DISPLAY tran-date fin-apr end_cust begin_cust tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 tran-date fin-apr end_cust begin_cust tbAutoClose btn-ok 
        btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

