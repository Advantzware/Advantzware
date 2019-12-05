&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-process AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 purge_date begin_order end_order ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS purge_date begin_order end_order 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE VARIABLE begin_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE purge_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Purge Orders Prior To" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
    SIZE 89 BY 8.57.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    purge_date AT ROW 7.19 COL 44 COLON-ALIGNED
    begin_order AT ROW 9.57 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_order AT ROW 9.57 COL 63 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    btn-process AT ROW 15.29 COL 21
    btn-cancel AT ROW 15.29 COL 53
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 5.29 COL 5
    "" VIEW-AS TEXT
    SIZE 2.2 BY .95 AT ROW 1.95 COL 88
    BGCOLOR 11 
    RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
    "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
    SIZE 84 BY .95 AT ROW 1.95 COL 4
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
    SIZE 76 BY .95 AT ROW 2.91 COL 8
    BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.2 BY 3.81
    BGCOLOR 11 .

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
        TITLE              = "Purge Orders"
        HEIGHT             = 17.71
        WIDTH              = 90.2
        MAX-HEIGHT         = 19.76
        MAX-WIDTH          = 98.2
        VIRTUAL-HEIGHT     = 19.76
        VIRTUAL-WIDTH      = 98.2
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
/* REPARENT FRAME */
ASSIGN 
    FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
    btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Orders */
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
ON WINDOW-CLOSE OF C-Win /* Purge Orders */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        RUN run-process.
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
    /* check security */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.

    purge_date = TODAY.

    RUN enable_UI.

    {methods/nowait.i}
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
    DISPLAY purge_date begin_order end_order 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 purge_date begin_order end_order btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-B IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :

    DEFINE VARIABLE v-post-date AS DATE INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-first-ord LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE v-last-ord LIKE oe-ord.ord-no NO-UNDO.

    DEFINE BUFFER b-boll FOR oe-boll.
    DEFINE BUFFER b-rell FOR oe-rell.

    DO WITH FRAME {&frame-name}:
        ASSIGN
            purge_date
            begin_order
            end_order.
    END.

    ASSIGN
        v-post-date = purge_date
        v-first-ord = begin_order
        v-last-ord  = end_order
        v-process   = NO.

    MESSAGE 
        "Are you sure you want to delete the orders within the selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    IF v-process THEN DO:
        FOR EACH oe-ord NO-LOCK 
            WHERE oe-ord.company  EQ cocode
            AND oe-ord.ord-date LT v-post-date
            AND oe-ord.ord-no   GE v-first-ord
            AND oe-ord.ord-no   LE v-last-ord
            USE-INDEX ordate
            TRANSACTION:

            FOR EACH oe-rel EXCLUSIVE WHERE 
                oe-rel.company EQ cocode AND 
                oe-rel.ord-no  EQ oe-ord.ord-no
                USE-INDEX ord-item:
                DELETE oe-rel.
            END. /* oe-rel */

            FOR EACH oe-rell EXCLUSIVE WHERE 
                oe-rell.company EQ oe-ord.company AND 
                oe-rell.ord-no  EQ oe-ord.ord-no
                USE-INDEX ord-no:
                DELETE oe-rell.
            END. /* oe-rell */

            FIND FIRST oe-relh EXCLUSIVE WHERE 
                oe-relh.company EQ oe-ord.company AND 
                oe-relh.ord-no EQ oe-ord.ord-no 
                USE-INDEX order 
                NO-ERROR.
            IF AVAIL oe-relh 
            AND NOT CAN-FIND(FIRST b-rell WHERE 
                            b-rell.r-no EQ oe-relh.r-no) THEN
                DELETE oe-relh.

            FOR EACH oe-boll EXCLUSIVE WHERE 
                oe-boll.company EQ oe-ord.company AND 
                oe-boll.ord-no  EQ oe-ord.ord-no
                USE-INDEX ord-no:
                DELETE oe-boll.
            END. /* oe-boll */

            FIND FIRST oe-bolh EXCLUSIVE WHERE 
                oe-bolh.company EQ oe-ord.company AND 
                oe-bolh.b-no EQ oe-ord.ord-no 
                USE-INDEX order-no
                NO-ERROR.

            IF AVAIL oe-bolh THEN FOR EACH inv-head EXCLUSIVE WHERE 
                inv-head.company EQ cocode AND  
                inv-head.bol-no EQ oe-bolh.bol-no
                USE-INDEX bolno:

                FOR EACH inv-line EXCLUSIVE WHERE 
                    inv-line.r-no    EQ inv-head.r-no AND 
                    inv-line.company EQ inv-head.company 
                    USE-INDEX r-no:
                    DELETE inv-line.
                END. /* inv-line */

                FOR EACH inv-misc EXCLUSIVE WHERE 
                    inv-misc.r-no    EQ inv-head.r-no AND 
                    inv-misc.company EQ inv-head.company
                    USE-INDEX r-no:
                    DELETE inv-misc.
                END. /* inv-misc */

                DELETE inv-head.
            END. /* inv-head */

            IF AVAIL oe-bolh
            AND NOT CAN-FIND(FIRST b-boll WHERE 
                            b-boll.b-no EQ oe-bolh.b-no) THEN 
                DELETE oe-bolh.


            FOR EACH oe-ordl NO-LOCK WHERE 
                oe-ordl.company EQ cocode AND 
                oe-ordl.ord-no  EQ oe-ord.ord-no
                USE-INDEX ord-no:

                FOR EACH job EXCLUSIVE WHERE 
                    job.company EQ cocode AND 
                    job.job-no  EQ oe-ordl.job-no AND 
                    job.job-no2 EQ oe-ordl.job-no2
                    USE-INDEX job-no:

                    RUN jc/jc-dall.p (RECID(job)).

                    FOR EACH job-hdr EXCLUSIVE WHERE 
                        job-hdr.company EQ cocode AND 
                        job-hdr.job     EQ job.job AND 
                        job-hdr.job-no  EQ job.job-no AND 
                        job-hdr.job-no2 EQ job.job-no2
                        USE-INDEX job-no:

                        {util/dljobkey.i}

                        DELETE job-hdr.
                    END.

                    FOR EACH job-mat EXCLUSIVE WHERE 
                        job-mat.company EQ job.company AND 
                        job-mat.job     EQ job.job AND 
                        job-mat.job-no  EQ job.job-no AND 
                        job-mat.job-no2 EQ job.job-no2
                        USE-INDEX job:
                        DELETE job-mat.
                    END.

                    FOR EACH job-mch EXCLUSIVE WHERE 
                        job-mch.company EQ job.company AND 
                        job-mch.job     EQ job.job AND 
                        job-mch.job-no  EQ job.job-no AND 
                        job-mch.job-no2 EQ job.job-no2
                        USE-INDEX job:
                        DELETE job-mch.
                    END.

                    FOR EACH job-prep EXCLUSIVE WHERE 
                        job-prep.company EQ job.company AND 
                        job-prep.job     EQ job.job AND 
                        job-prep.job-no  EQ job.job-no AND 
                        job-prep.job-no2 EQ job.job-no2
                        USE-INDEX prep-idx:
                        DELETE job-prep.
                    END.

                    FOR EACH job-farm EXCLUSIVE WHERE 
                        job-farm.company EQ job.company AND 
                        job-farm.job-no  EQ job.job-no AND 
                        job-farm.job-no2 EQ job.job-no2
                        USE-INDEX job-no:
                        DELETE job-farm.
                    END.

                    FOR EACH job-farm-rctd EXCLUSIVE WHERE 
                        job-farm-rctd.company EQ job.company AND 
                        job-farm-rctd.job-no  EQ job.job-no AND 
                        job-farm-rctd.job-no2 EQ job.job-no2
                        USE-INDEX job-farm-rctd:
                        DELETE job-farm-rctd.
                    END.

                    FOR EACH pc-prdd EXCLUSIVE WHERE 
                        pc-prdd.company EQ cocode AND 
                        pc-prdd.job     EQ job.job AND 
                        pc-prdd.job-no  EQ job.job-no AND 
                        pc-prdd.job-no2 EQ job.job-no2
                        USE-INDEX job-no:
                        DELETE pc-prdd.
                    END.

                    FOR EACH fg-act EXCLUSIVE WHERE 
                        fg-act.company EQ cocode AND 
                        fg-act.job     EQ job.job AND 
                        fg-act.job-no  EQ job.job-no AND 
                        fg-act.job-no2 EQ job.job-no2
                        USE-INDEX job-no:
                        DELETE fg-act.
                    END.

                    FOR EACH mat-act EXCLUSIVE WHERE 
                        mat-act.company EQ cocode AND 
                        mat-act.job     EQ job.job AND 
                        mat-act.job-no  EQ job.job-no AND 
                        mat-act.job-no2 EQ job.job-no2
                        USE-INDEX job:
                        DELETE mat-act.
                    END.

                    FOR EACH mch-act EXCLUSIVE WHERE 
                        mch-act.company EQ cocode AND 
                        mch-act.job     EQ job.job AND 
                        mch-act.job-no  EQ job.job-no AND 
                        mch-act.job-no2 EQ job.job-no2
                        USE-INDEX job-no:
                        DELETE mch-act.
                    END.

                    FOR EACH misc-act EXCLUSIVE WHERE 
                        misc-act.company EQ cocode AND 
                        misc-act.job     EQ job.job AND 
                        misc-act.job-no  EQ job.job-no AND 
                        misc-act.job-no2 EQ job.job-no2
                        USE-INDEX misc-idx:
                        DELETE misc-act.
                    END.

                    FOR EACH fg-bin EXCLUSIVE WHERE 
                        fg-bin.company    EQ cocode AND 
                        fg-bin.job-no     EQ job.job-no AND 
                        fg-bin.job-no2    EQ job.job-no2 AND 
                        fg-bin.i-no       NE "" AND 
                        fg-bin.qty        EQ 0
                        USE-INDEX job:
                        /* Moved here so indexing ok on for each */
                        IF TRIM(fg-bin.i-no) EQ "" THEN NEXT.
                        DELETE fg-bin.
                    END.

                    IF job.exported THEN DO:
                        job.stat = "X".
                        RUN jc/kiwiexp2.p (RECID(job)).
                    END.

                    DELETE job.
                END.

                FIND CURRENT oe-ordl EXCLUSIVE.
                DELETE oe-ordl.
            END.

            FIND CURRENT oe-ord EXCLUSIVE.
            DELETE oe-ord.
        END. /* oe-ord */

        MESSAGE 
            TRIM(c-win:TITLE) + " Process Is Completed." 
            VIEW-AS ALERT-BOX.
        
        APPLY "close" TO THIS-PROCEDURE.
    
    END.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

