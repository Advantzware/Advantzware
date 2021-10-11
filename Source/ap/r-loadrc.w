&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-recreg.w

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

{ap/reconcil.i NEW}


DEFINE TEMP-TABLE tt-fileload NO-UNDO
    FIELDS tt-linefeed AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 btn-browse fi_loadfile fi_expFile ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_loadfile fi_expFile tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-browse 
    LABEL "Browse" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE fi_expFile  AS CHARACTER FORMAT "X(50)":U 
    LABEL "Exception File" 
    VIEW-AS FILL-IN 
    SIZE 62.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_loadfile AS CHARACTER FORMAT "X(50)":U 
    VIEW-AS FILL-IN 
    SIZE 62.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 9.05.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    btn-browse AT ROW 4.57 COL 81.4 WIDGET-ID 8
    fi_loadfile AT ROW 4.71 COL 16.4 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    fi_expFile AT ROW 6.71 COL 16.4 COLON-ALIGNED WIDGET-ID 4
    tbAutoClose AT ROW 10.76 COL 30.2 WIDGET-ID 64
    btn-ok AT ROW 11.76 COL 30
    btn-cancel AT ROW 11.76 COL 52.6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    "Load File:" VIEW-AS TEXT
    SIZE 11 BY .62 AT ROW 4.91 COL 7 WIDGET-ID 6
    RECT-7 AT ROW 1.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 99 BY 13.57
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
        TITLE              = "A/P Load Bank Reconciliation File"
        HEIGHT             = 12.19
        WIDTH              = 98.8
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
ON END-ERROR OF C-Win /* A/P Load Bank Reconciliation File */
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
ON WINDOW-CLOSE OF C-Win /* A/P Load Bank Reconciliation File */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-browse C-Win
ON CHOOSE OF btn-browse IN FRAME FRAME-A /* Browse */
    DO:
        DEFINE VARIABLE v-okflg    AS LOG       NO-UNDO.

        DEFINE VARIABLE v-loadFile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-path     AS CHARACTER NO-UNDO INIT "c:/tmp".


        SYSTEM-DIALOG GET-FILE v-loadFile
            TITLE 'Select File to load'
            FILTERS    "Listing Files (*.txt)" "*.txt",
            "All Files (*.*)" "*.*"
            INITIAL-DIR v-path
            MUST-EXIST USE-FILENAME 
            UPDATE v-okflg.

        IF NOT v-okflg THEN RETURN NO-APPLY.

        IF TRIM(v-loadFile) NE "" 
            THEN ASSIGN fi_loadfile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-loadFile.
        ELSE RETURN NO-APPLY.

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


        IF TRIM(fi_loadfile) EQ "" OR
            SEARCH(fi_loadFile) EQ ? THEN 
        DO:

            MESSAGE 
                " Please enter a valid file to load "
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

            RETURN NO-APPLY.

        END.


        RUN process-file.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_loadfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_loadfile C-Win
ON HELP OF fi_loadfile IN FRAME FRAME-A
    DO:
        APPLY "CHOOSE" TO btn-browse IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_loadfile C-Win
ON LEAVE OF fi_loadfile IN FRAME FRAME-A
    DO:

        ASSIGN 
            fi_loadfile = fi_loadfile:SCREEN-VALUE.


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
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    btn-browse:LOAD-IMAGE("Graphics/32x32/browse.png").
    RUN enable_UI.

    fi_expFile:HIDDEN = TRUE.

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
    DISPLAY fi_loadfile fi_expFile tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 btn-browse fi_loadfile fi_expFile tbAutoClose btn-ok btn-cancel 
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
        SAVE-AS
        USE-FILENAME

        UPDATE OKpressed.

    IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-file C-Win 
PROCEDURE process-file :
    DEFINE VARIABLE v-acctnum AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-chcknum AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-statcd  AS CHARACTER FORMAT "x(1)" NO-UNDO.
    DEFINE VARIABLE v-amount  AS CHARACTER FORMAT "x(12)" NO-UNDO.
    DEFINE VARIABLE v-pddate  AS CHARACTER FORMAT "x(6)" NO-UNDO.
    DEFINE VARIABLE v-batch   AS CHARACTER FORMAT "x(8)" NO-UNDO.
    DEFINE VARIABLE v-addtl   AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-fllr    AS CHARACTER FORMAT "x(18)" NO-UNDO.
    DEFINE VARIABLE v-msg     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-iflg    AS LOG       INIT YES NO-UNDO.

    DEFINE VARIABLE v-icnt    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-zcnt    AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE v-int     AS INTEGER   NO-UNDO.

    SESSION:SET-WAIT-STATE("general").

    EMPTY TEMP-TABLE tt-fileload.

    INPUT FROM VALUE(fi_loadFile).
    REPEAT:
        CREATE tt-fileload.
        IMPORT tt-fileload.tt-linefeed.
    END.

    RUN ap/reconcil.p.

    FOR EACH tt-fileload:

        IF TRIM(tt-fileload.tt-linefeed) EQ "" THEN  
        DO:
            DELETE tt-fileload.
            NEXT.
        END.

        ASSIGN
            v-acctnum = SUBSTR(tt-fileload.tt-linefeed,1,10) 
            v-chcknum = SUBSTR(tt-fileload.tt-linefeed,11,10) 
            v-statcd  = SUBSTR(tt-fileload.tt-linefeed,21,1) 
            v-amount  = SUBSTR(tt-fileload.tt-linefeed,22,12) 
            v-pddate  = SUBSTR(tt-fileload.tt-linefeed,34,6) 
            v-batch   = SUBSTR(tt-fileload.tt-linefeed,40,8) 
            v-addtl   = SUBSTR(tt-fileload.tt-linefeed,48,15) 
            v-fllr    = SUBSTR(tt-fileload.tt-linefeed,63,18).

        /* CLEANUP */ 
        /* CLEAN UP RECORDS  */
        v-zcnt = 0.
        DO v-icnt = 1 TO LENGTH(v-chcknum):

            ASSIGN 
                v-int = INT(SUBSTR(v-chcknum,v-icnt,1)).

            IF v-int EQ 0 
                THEN v-zcnt = v-zcnt + 1.
            ELSE LEAVE.
        END.

        IF v-zcnt GT 0 
            THEN ASSIGN v-chcknum = SUBSTR(v-chcknum,v-zcnt + 1).

        /*
            IF SUBSTR(v-chcknum, LENGTH(v-chcknum),1) = "0"
              THEN
                ASSIGN 
                   v-chcknum = TRIM(v-chcknum,"0") + "0".
              ELSE
                ASSIGN 
                   v-chcknum = TRIM(v-chcknum,"0").
        */
        ASSIGN
            v-amount = SUBSTR(v-amount,1,LENGTH(v-amount) - 2) + "." + 
                    SUBSTR(v-amount,LENGTH(v-amount) - 1).

        IF SUBSTR(v-amount, LENGTH(v-amount) - 1,2) = "00"
            THEN
            ASSIGN 
                v-amount = TRIM(v-amount,"0") + "00".
        ELSE
            ASSIGN 
                v-amount = TRIM(v-amount,"0").

        FIND FIRST reconcile NO-LOCK
            WHERE reconcile.tt-number = v-chcknum
            AND reconcile.tt-amt = DEC(v-amount) NO-ERROR.
        IF AVAILABLE reconcile THEN 
        DO:

            IF v-statcd = "R" THEN reconcile.tt-cleared = YES.

            RUN reconcile-file.

            DELETE tt-fileload.

        END.           
    END.

    FIND FIRST tt-fileload NO-LOCK NO-ERROR.
    IF AVAILABLE tt-fileload THEN 
    DO:
        v-iflg = YES.
        OUTPUT TO VALUE(SUBSTR(fi_loadFile,1,LENGTH(fi_loadFile) - 4 ) +
            "_exp" + SUBSTR(fi_loadFile,LENGTH(fi_loadFile) - 3)
            ).

        FOR EACH tt-fileload.
            EXPORT tt-fileload.tt-linefeed.
        END.
        OUTPUT CLOSE.

        ASSIGN 
            fi_expFile:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            fi_expFile:SCREEN-VALUE                  = SUBSTR(fi_loadFile,1,LENGTH(fi_loadFile) - 4 ) +
                            "_exp" + SUBSTR(fi_loadFile,LENGTH(fi_loadFile) - 3)
            fi_expFile:SENSITIVE                     = FALSE.


    END.

    IF v-iflg 
        THEN v-msg = " Please check the exception file. ".
    ELSE v-msg = "".

    MESSAGE "Load Complete !!" v-msg
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*
APPLY "CHOOSE" TO btn-cancel IN FRAME {&FRAME-NAME}.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reconcile-file C-Win 
PROCEDURE reconcile-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ap-pay FOR ap-pay.

    FIND CURRENT reconcile NO-LOCK NO-ERROR.

    IF tt-type EQ 1 THEN 
    DO /*TRANSACTION*/ :

        FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.
        IF AVAILABLE ap-pay 
            THEN
            IF ap-pay.d-no NE 0  AND
                NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) 
                THEN 
            DO:
                FIND FIRST bf-ap-pay
                    WHERE bf-ap-pay.company   EQ ap-pay.company
                    AND bf-ap-pay.check-act EQ ap-pay.check-act
                    AND bf-ap-pay.check-no  EQ ap-pay.d-no EXCLUSIVE-LOCK NO-ERROR.
            END.
            ELSE 
                FIND bf-ap-pay EXCLUSIVE-LOCK 
                    WHERE ROWID(bf-ap-pay) EQ ROWID(ap-pay) NO-ERROR.


        IF AVAILABLE bf-ap-pay THEN 
        DO:

            bf-ap-pay.cleared = reconcile.tt-cleared.

            FOR EACH ap-pay EXCLUSIVE-LOCK
                WHERE ap-pay.company EQ bf-ap-pay.company
                AND ap-pay.d-no    EQ bf-ap-pay.check-no
                AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)             
                USE-INDEX d-no:

                ASSIGN 
                    ap-pay.cleared = bf-ap-pay.cleared.

            END.
        END.

    END. /*tt-type eq 1*/

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

