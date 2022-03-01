&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl/r-glmcloM.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: sewa singh

  Created: 01/18/2021

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
DEFINE INPUT PARAMETER ipcModule AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-invalid AS LOG       NO-UNDO.

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

DEFINE VARIABLE save_id    AS RECID.
DEFINE VARIABLE time_stamp AS ch.

DEFINE VARIABLE start-date AS DATE    INITIAL 01/01/1901 NO-UNDO.
DEFINE VARIABLE end-date   AS DATE    INITIAL 01/01/1901 NO-UNDO.
DEFINE VARIABLE tot-all    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE tot-tx     LIKE tot-all NO-UNDO.
DEFINE VARIABLE tot-act    LIKE tot-all NO-UNDO.
DEFINE VARIABLE tot-jrnl   LIKE tot-all NO-UNDO.
DEFINE VARIABLE open-amt   LIKE tot-all NO-UNDO.
DEFINE VARIABLE net-inc    AS DECIMAL NO-UNDO.
DEFINE VARIABLE per-open   AS INTEGER FORMAT ">9" NO-UNDO.
DEFINE VARIABLE per-status LIKE period.pstat NO-UNDO.
DEFINE VARIABLE fiscal-yr  LIKE period.yr NO-UNDO.

DEFINE BUFFER b-racct FOR account.
DEFINE BUFFER b-cacct FOR account.
DEFINE VARIABLE uperiod      AS INTEGER   NO-UNDO.
DEFINE VARIABLE choice       AS LOG       NO-UNDO.
DEFINE VARIABLE lv-font-no   AS CHARACTER FORMAT "X(256)":U INITIAL "11" NO-UNDO.
DEFINE VARIABLE lv-ornt      AS CHARACTER INITIAL "P" NO-UNDO.
DEFINE VARIABLE cModuleTitle AS CHARACTER NO-UNDO.

ASSIGN 
    time_stamp = STRING(TIME,"hh:mmam")
    .
DEFINE STREAM excel.
IF ipcModule EQ "AP" THEN
    ASSIGN cModuleTitle = "Close A/P – Payables" .
ELSE IF ipcModule EQ "PO" THEN
        ASSIGN cModuleTitle = "Close P/O – Purchasing" .
    ELSE IF ipcModule EQ "OP" THEN
            ASSIGN cModuleTitle = "Close O/P - Order Processing" .
        ELSE IF ipcModule EQ "WIP" THEN
                ASSIGN cModuleTitle = "Close WIP - Work In Process" .
            ELSE IF ipcModule EQ "RM" THEN
                    ASSIGN cModuleTitle = "Close R/M - Inventory" .
                ELSE IF ipcModule EQ "FG" THEN
                        ASSIGN cModuleTitle = "Close F/G –Inventory" .
                    ELSE IF ipcModule EQ "BR" THEN
                            ASSIGN cModuleTitle = "Close B/R - Bank Reconciliation" .
                        ELSE IF ipcModule EQ "AR" THEN
                                ASSIGN cModuleTitle = "Close A/R – Receivables" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tb_sub-report tb_close-sub btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiText tran-year tran-period tb_sub-report ~
tb_close-sub 

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

DEFINE VARIABLE fiText      AS CHARACTER FORMAT "X(300)":U INITIAL "This operation will perform a CLOSE on your first open period for this sub ledger" 
    VIEW-AS FILL-IN 
    SIZE 78 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-year   AS INTEGER   FORMAT ">>>>":U INITIAL 0 
    LABEL "Year" 
    VIEW-AS FILL-IN 
    SIZE 8.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 6.57.

DEFINE VARIABLE tb_close-sub  AS LOGICAL INITIAL NO 
    LABEL "Close Sub Ledger" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sub-report AS LOGICAL INITIAL NO 
    LABEL "Sub Ledger Report" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fiText AT ROW 2.43 COL 9 COLON-ALIGNED NO-LABELS
    tran-year AT ROW 3.62 COL 20 COLON-ALIGNED WIDGET-ID 6
    tran-period AT ROW 3.62 COL 39 COLON-ALIGNED
    tb_sub-report AT ROW 5.52 COL 21.8 WIDGET-ID 10
    tb_close-sub AT ROW 6.1 COL 21.8 WIDGET-ID 12
    btn-ok AT ROW 9.1 COL 29.2
    btn-cancel AT ROW 9.1 COL 51.8
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    RECT-7 AT ROW 1.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 11.14
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
        TITLE              = cModuleTitle
        HEIGHT             = 10.19
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN fiText IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    fiText:READ-ONLY IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_close-sub:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sub-report:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tran-period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-year IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tran-year:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* cModuleTitle */
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
ON WINDOW-CLOSE OF C-Win /* cModuleTitle */
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
        ASSIGN 
            tran-period
            uperiod = tran-period
            .

        RUN check-date (YES).
        IF v-invalid THEN RETURN NO-APPLY. 
  
        RUN pCheckClosePermission .
        IF v-invalid THEN RETURN NO-APPLY.

        ASSIGN /*rd-dest*/
         
            tran-period
            uperiod = tran-period
            .
      
        IF tb_close-sub THEN 
        DO:
            choice = NO.
            MESSAGE " Close " ipcModule " Period" uperiod VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
            IF choice THEN 
            DO:
                RUN close-month.
                MESSAGE "Closing " ipcModule " Period is completed. " VIEW-AS ALERT-BOX INFORMATION.       
                APPLY "close" TO THIS-PROCEDURE.
            END.
        END.
  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_close-sub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_close-sub C-Win
ON VALUE-CHANGED OF tb_close-sub IN FRAME FRAME-A /* Close Sub Ledger */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sub-report
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sub-report C-Win
ON VALUE-CHANGED OF tb_sub-report IN FRAME FRAME-A /* Sub Ledger Report */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
    DO:
        ASSIGN {&self-name}.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date (NO).
            IF v-invalid THEN 
            DO:
                ASSIGN 
                    SELF:SCREEN-VALUE = "".
                APPLY 'entry' TO tran-year.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-year C-Win
ON LEAVE OF tran-year IN FRAME FRAME-A /* Year */
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

    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
  
    tran-year = YEAR(TODAY) .
    tran-period = (MONTH(TODAY))  .
  
    FIND company NO-LOCK WHERE 
        company.company EQ cocode
        NO-ERROR.
    IF NOT AVAILABLE company THEN 
    DO:
        MESSAGE 
            "Company " + cocode + " does not exist in the company file."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    IF NOT company.yend-per THEN 
    DO:
        MESSAGE 
            "PRIOR YEAR NOT CLOSED.  MUST CLOSE PRIOR YEAR!!!" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    FIND FIRST period NO-LOCK WHERE 
        period.company EQ company.company AND
        period.pstat EQ TRUE AND
        ((period.subLedgerAP NE "C" AND ipcModule EQ "AP") OR
        (period.subLedgerPO NE "C" AND ipcModule EQ "PO") OR
        (period.subLedgerOP NE "C" AND ipcModule EQ "OP") OR
        (period.subLedgerWIP NE "C" AND ipcModule EQ "WIP") OR
        (period.subLedgerRM NE "C" AND ipcModule EQ "RM") OR
        (period.subLedgerFG NE "C" AND ipcModule EQ "FG") OR
        (period.subLedgerBR NE "C" AND ipcModule EQ "BR") OR
        (period.subLedgerAR NE "C" AND ipcModule EQ "AR"))
        NO-ERROR.
    IF AVAILABLE period THEN ASSIGN 
            tran-year   = period.yr
            tran-period = period.pnum.
        
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}
    DO WITH FRAME {&frame-name}:
        tb_sub-report:HIDDEN IN FRAME FRAME-A    = TRUE.
        APPLY "entry" TO tran-year.
    END.
    
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
    DEFINE INPUT PARAMETER ip-oktogo AS LOG NO-UNDO.

    DEFINE BUFFER alt-period FOR period.


    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.yr   EQ tran-year
            AND period.pnum EQ tran-period           
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN 
        DO:
            IF period.subLedgerAP EQ "C" AND ipcModule EQ "AP" THEN 
            DO:
                MESSAGE "Payables - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            ELSE IF period.subLedgerPO EQ "C" AND ipcModule EQ "PO" THEN 
                DO:
                    MESSAGE "Purchasing - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                    v-invalid = YES.
                END.
                ELSE IF period.subLedgerOP EQ "C" AND ipcModule EQ "OP" THEN 
                    DO:
                        MESSAGE "Order Processing - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                        v-invalid = YES.
                    END.
                    ELSE IF period.subLedgerWIP EQ "C" AND ipcModule EQ "WIP" THEN 
                        DO:
                            MESSAGE "Work In Process - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                            v-invalid = YES.
                        END.
                        ELSE IF period.subLedgerRM EQ "C" AND ipcModule EQ "RM" THEN 
                            DO:
                                MESSAGE "R/M Inventory - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                                v-invalid = YES.
                            END.
                            ELSE IF period.subLedgerFG EQ "C" AND ipcModule EQ "FG" THEN 
                                DO:
                                    MESSAGE "F/G Inventory - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                                    v-invalid = YES.
                                END.
                                ELSE IF period.subLedgerBR EQ "C" AND ipcModule EQ "BR" THEN 
                                    DO:
                                        MESSAGE "Bank Reconciliation - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                                        v-invalid = YES.
                                    END.
                                    ELSE IF period.subLedgerAR EQ "C" AND ipcModule EQ "AR" THEN 
                                        DO:
                                            MESSAGE "A/R Receivables - Already Closed. " VIEW-AS ALERT-BOX ERROR.
                                            v-invalid = YES.
                                        END.
        /*else do:
          find first alt-period
              where alt-period.company             eq cocode
                and alt-period.pst - period.pend   eq 1
                and (alt-period.pnum - period.pnum eq 1     or
                     (alt-period.pnum              eq 1 and
                      period.pnum eq company.num-per))
                and alt-period.pstat               eq yes
            no-lock no-error.
          if not avail alt-period then do:
            MESSAGE "NEXT PERIOD NOT DEFINED.  MUST DEFINE NEXT PERIOD!!!"
                VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
          end.
          /* CODE FOR VERIFYING CLOSE OF ALL PRIOR PERIODS */
          else do:
            find first alt-period where alt-period.company eq cocode
                                    AND alt-period.yr   EQ tran-year
                                    AND alt-period.pnum EQ tran-period
                                  no-lock no-error.
            if avail alt-period then fiscal-yr = alt-period.yr.
            find first alt-period where alt-period.company eq cocode
                     and (alt-period.yr     lt fiscal-yr or
                         (alt-period.yr    eq fiscal-yr and
                          alt-period.pnum  lt period.pnum))
                     and alt-period.pstat   eq yes
                     no-lock no-error.
            if avail alt-period then do:
              ASSIGN per-open   = alt-period.pnum
                     per-status = alt-period.pstat.
              MESSAGE "PRIOR MONTH(S) NOT CLOSED.  MUST CLOSE ALL PRIOR MONTHS!!!"
                    VIEW-AS ALERT-BOX ERROR.
              v-invalid = YES.
            end.
            ELSE
            if period.pnum eq 1 AND ip-oktogo then do:
              MESSAGE "YOU ARE ABOUT TO CLOSE PERIOD 1." skip(1)
                      "YOU MUST MAKE SURE THE PRIOR FISCAL YEAR END PROCEDURE HAS BEEN RUN!!!"
                      skip(2)
                      "Do You Want to Continue and Close the Month? " VIEW-AS ALERT-BOX BUTTON YES-NO
                      update choice .
            end.
          end.
        END.   */
        /*tran-period:SCREEN-VALUE = string(period.pnum).*/
        END.

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-period VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-month C-Win 
PROCEDURE close-month :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.

    DEFINE BUFFER b-period FOR period.

    SESSION:SET-WAIT-STATE ("general").

    FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    FIND FIRST company WHERE company.company EQ cocode.
    FIND FIRST b-racct
        WHERE b-racct.company EQ cocode
        AND b-racct.actnum  EQ gl-ctrl.ret
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-racct THEN 
    DO ON ENDKEY UNDO, RETURN:
        MESSAGE "Unable to Find Retained Earnings Account from G/L Control File."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    FIND FIRST b-cacct
        WHERE b-cacct.company EQ cocode
        AND b-cacct.actnum  EQ gl-ctrl.contra
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-cacct THEN 
    DO ON ENDKEY UNDO, RETURN:
        MESSAGE "Unable to Find Profit Contra Account from G/L Control File." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
           
    RUN GL_CloseMonthModule(cocode, tran-year, uperiod, ipcModule). /* Company,Year,Period,Module*/ 
      
  
    SESSION:SET-WAIT-STATE ("").

/* message "Current accounting period changed to " uperiod VIEW-AS ALERT-BOX.*/

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
    DISPLAY fiText tran-year tran-period tb_sub-report tb_close-sub 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 tb_sub-report tb_close-sub btn-ok btn-cancel 
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

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */

    /* /*Use Progress Print. Always use Font#9 in Registry (set above) */
       RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                              INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                      /* use-dialog(1) and landscape(2) */
      */
    RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt). /* open file-name, title */ 

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckClosePermission C-Win 
PROCEDURE pCheckClosePermission :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        IF AVAILABLE company AND ((ipcModule EQ "AP" AND NOT company.subLedgerAP) OR
            (ipcModule EQ "PO" AND NOT company.subLedgerPO) OR
            (ipcModule EQ "OP" AND NOT company.subLedgerOP) OR 
            (ipcModule EQ "WIP" AND NOT company.subLedgerWIP) OR 
            (ipcModule EQ "RM" AND NOT company.subLedgerRM) OR 
            (ipcModule EQ "FG" AND NOT company.subLedgerFG) OR 
            (ipcModule EQ "BR" AND NOT company.subLedgerBR) OR 
            (ipcModule EQ "AR" AND NOT company.subLedgerAR) ) THEN
        DO:
            MESSAGE "This sub ledger is not configured to be closed separately than the period.  Change setting or proceed to close the period" VIEW-AS ALERT-BOX INFORMATION.
            v-invalid = YES.        
        END.
    
    END.

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
            ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
                    ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                        parm-lbl-list = parm-lbl-list + lv-field-hdl:HELP + "," 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-period C-Win 
PROCEDURE valid-period :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

