&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-estmar.w

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
&GLOBAL-DEFINE summary-sheet 1
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File   AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE num-groups   AS INTEGER   NO-UNDO.
DEFINE VARIABLE group-ok     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL   NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/* {ce/msfcalc.i} */
{ce/print4.i "new shared" "new shared"}


DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
/*DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-summ AS LOG INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE fr-tot-pre AS DECIMAL.
DEFINE NEW SHARED VARIABLE gEstSummaryOnly AS LOG NO-UNDO.
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.
DEFINE NEW SHARED BUFFER xop FOR est-op.
  */
DEFINE BUFFER bf-probe FOR probe .
DEFINE STREAM st-excel.

DEFINE NEW SHARED VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper            AS COMPONENT-HANDLE NO-UNDO. 
DEFINE            VARIABLE v-cell             AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE t-dwg              AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE t-name             AS CHARACTER        FORMAT "x(40)" NO-UNDO.
DEFINE            VARIABLE t-fnd              AS LOGICAL          INIT "False" NO-UNDO.
DEFINE            VARIABLE t-seq              AS INTEGER          NO-UNDO.
DEFINE            VARIABLE inRowCount         AS INTEGER          NO-UNDO INITIAL 1.
DEFINE            VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE LvLineCnt          AS INTEGER          NO-UNDO.
DEFINE            VARIABLE CurrDir            AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE LvCtr              AS INTEGER          NO-UNDO.
DEFINE            VARIABLE v-dir              AS CHARACTER        FORMAT "X(80)" NO-UNDO.
DEFINE            VARIABLE vcTemplateFile     AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE row-count          AS INTEGER          INIT 6 NO-UNDO.

DEFINE            VARIABLE lv-brd-l           LIKE eb.len NO-UNDO.
DEFINE            VARIABLE lv-brd-w           LIKE lv-brd-l NO-UNDO.
DEFINE            VARIABLE lv-brd-sq          AS DEC              FORMAT ">>>>9.9<<<<" NO-UNDO.
DEFINE            VARIABLE lv-brd-sf          AS DEC              FORMAT ">>>>>9.9<<" NO-UNDO.
DEFINE            VARIABLE lv-brd-wu          LIKE lv-brd-sq NO-UNDO.
DEF BUFFER bf-est FOR est.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slsmn ~
end_slsmn begin_est end_est begin_date end_date begin_date-2 end_date-2 

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
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Add Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date-2  AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Mod Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_est     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn   AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Add Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date-2    AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Mod Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_est       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn     AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust-no AT ROW 3.14 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3.14 COL 69 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_slsmn AT ROW 4.1 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 4.1 COL 69 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_est AT ROW 5.05 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Estimate"
    end_est AT ROW 5.05 COL 69 COLON-ALIGNED HELP
    "Enter Ending Estimate"
    begin_date AT ROW 6 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 6 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_date-2 AT ROW 6.95 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date-2 AT ROW 6.95 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    btn-ok AT ROW 10.29 COL 26
    btn-cancel AT ROW 10.29 COL 56
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 94.4 BY 12.29.


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
        TITLE              = "Customer Margin Analysis"
        HEIGHT             = 13.1
        WIDTH              = 95.6
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
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "Parm".

ASSIGN 
    begin_date-2:PRIVATE-DATA IN FRAME FRAME-A = "Parm".

ASSIGN 
    begin_est:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "Parm".

ASSIGN 
    end_date-2:PRIVATE-DATA IN FRAME FRAME-A = "Parm".

ASSIGN 
    end_est:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Margin Analysis */
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
ON WINDOW-CLOSE OF C-Win /* Customer Margin Analysis */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Add Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Beginning Mod Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
    DO:
        DEFINE VARIABLE char-val    AS cha   NO-UNDO.
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

        IF char-val <> "" THEN 
        DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE     = eb.est-no
                    lv-eb-tmpid            = RECID(eb)    
                    begin_est:SCREEN-VALUE = eb.est-no.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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

        RUN run-report.
        STATUS DEFAULT "Processing Complete".

   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Add Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-2 C-Win
ON LEAVE OF end_date-2 IN FRAME FRAME-A /* Ending Mod Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
    DO:
        DEFINE VARIABLE char-val    AS cha   NO-UNDO.
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

        IF char-val <> "" THEN 
        DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE   = eb.est-no
                    lv-eb-tmpid          = RECID(eb)    
                    end_est:SCREEN-VALUE = eb.est-no.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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

    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
APPLY "entry" TO begin_cust-no.
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
    DISPLAY begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est end_est 
        begin_date end_date begin_date-2 end_date-2 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 begin_cust-no end_cust-no begin_slsmn end_slsmn begin_est 
        end_est begin_date end_date begin_date-2 end_date-2 btn-ok btn-cancel 
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
/*RUN custom/d-print.w (fi_file).*/

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
                         /* font #*/ /* use-dialog(1) and landscape(2) */
                         */
/*RUN custom/prntproc.p (fi_file,int(lv-font-no),lv-ornt).*/
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
/*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    {sys/form/r-topw.f}

    DEFINE VARIABLE fest        LIKE est.est-no NO-UNDO.
    DEFINE VARIABLE test        LIKE fest NO-UNDO.

    DEFINE VARIABLE li          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-box-size LIKE quoteitm.size NO-UNDO.
    DEFINE VARIABLE lv-die-size LIKE quoteitm.size NO-UNDO.
    DEFINE VARIABLE lv-format   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li-colors   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-qty      LIKE probe.est-qty NO-UNDO.
    DEFINE VARIABLE ld-costm    LIKE probe.full-cost NO-UNDO.
    DEFINE VARIABLE ld-costt    AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE ld-price    LIKE probe.sell-price NO-UNDO.
    DEFINE VARIABLE ld-mar      AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 7 NO-UNDO.
    DEFINE VARIABLE ld-pct      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE k_frac      AS DECIMAL   INIT "6.25" NO-UNDO.
    DEFINE VARIABLE v-tons      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE qm          AS DECIMAL.
    DEFINE VARIABLE lv-eqty     LIKE est-op.qty NO-UNDO.
    DEFINE VARIABLE v-brd-cost  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE CALL_id     AS RECID     NO-UNDO.
    DEFINE VARIABLE dTonCost    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE iAvg        AS INTEGER   NO-UNDO INIT 0 .
    DEFINE BUFFER reftable-fm FOR reftable.
    DEFINE BUFFER probe-ref   FOR reftable.
    DEFINE BUFFER probe-fm    FOR reftable.
    DEFINE VARIABLE iprEstRowid    AS ROWID     NO-UNDO.
    DEFINE VARIABLE iprEstQtyRowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE iprEbRowid     AS ROWID     NO-UNDO.
    DEFINE VARIABLE iprEfRowid     AS ROWID     NO-UNDO.
    DEFINE VARIABLE iprProbeRowid  AS ROWID     NO-UNDO.

    DEFINE VARIABLE cCust          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dEstDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE cEstNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSellPrice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTonCost       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTons          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCalcPctsVal2  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCtrl2-9       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCtrl2-10      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCtrl2-1       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFullCost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dNetProfitDol  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cNetProfitPct  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrdNo         AS CHARACTER NO-UNDO.

    ASSIGN 
        row-count = 6 .

    /**************************** Excel Initilization Starts *********************************/

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        v-dir = users.user_program[2] + "\".
    ELSE
        v-dir = "c:\tmp\".

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication.

    FILE-INFO:FILE-NAME = "template\CustMarginAnalysis.xlt". /* CustMarginAnalysis*/

    /* Set the Excel Template to be used. */
    ASSIGN 
        chFile = SEARCH (FILE-INFO:FULL-PATHNAME) no-error.
  
    IF SEARCH (chFile) = ? THEN 
    DO:
        MESSAGE 'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY 'CLOSE':U TO THIS-PROCEDURE.
    END.

    /* Make Excel visible. */
    ASSIGN
        chFile = FILE-INFO:FULL-PATHNAME.


    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

    chExcelApplication:VISIBLE = TRUE.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.
  
    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    chWorkbook:WorkSheets({&summary-sheet}):Activate NO-ERROR.
    ASSIGN
        chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})  .
    /**************************** Excel Initilization End *********************************/


    ASSIGN
        str-tit2 = TRIM(c-win:TITLE) + ""
        {sys/inc/ctrtext.i str-tit2 112}

        fest     = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
        test     = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

    {sys/inc/print1.i}

    {sys/inc/outprint.i  VALUE(74)}

    SESSION:SET-WAIT-STATE ("general").

    /*IF td-show-parm THEN RUN show-param.*/
    VIEW FRAME r-top.
    MAIN-LOOP:
    FOR EACH est NO-LOCK
        WHERE est.company  EQ cocode
          AND est.est-no   GE fest
          AND est.est-no   LE test
          AND est.est-date GE begin_date
          AND est.est-date LE end_date
          AND est.mod-date GE begin_date-2
          AND est.mod-date LE end_date-2
          AND (est.est-type EQ 4 OR est.est-type EQ 8)
        ,

        FIRST est-qty NO-LOCK
        WHERE est-qty.company EQ est.company
          AND est-qty.est-no  EQ est.est-no
        ,

        EACH eb NO-LOCK
        WHERE eb.company  EQ est.company
          AND eb.est-no   EQ est.est-no
          AND eb.cust-no  GE begin_cust-no
          AND eb.cust-no  LE end_cust-no
          AND eb.sman     GE begin_slsmn
          AND eb.sman     LE end_slsmn
        /* AND (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))*/
        ,

        FIRST ef NO-LOCK
        WHERE ef.company EQ eb.company
          AND ef.est-no  EQ eb.est-no
          AND ef.form-no EQ eb.form-no
        ,

        EACH probe NO-LOCK
        WHERE probe.company   EQ est.company
          AND probe.est-no    EQ est.est-no
          AND probe.full-cost NE ?

        BREAK BY est.est-no DESCENDING
        BY probe.est-qty
        BY probe.probe-date
        BY probe.probe-time:
              
        IF LAST-OF(probe.est-qty) THEN 
        DO:

        {custom/statusMsg.i " 'Processing Estimate#:  '  + eb.est-no  "}

            ASSIGN
                iprEstRowid    = ROWID(est)
                iprEstQtyRowid = ROWID(est-qty)
                iprEbRowid     = ROWID(eb)
                iprEfRowid     = ROWID(ef)
                iprProbeRowid  = ROWID(probe)
                .


            RUN util/custMarginCalc.p (
                INPUT  iprEstRowid,
                INPUT  iprEstQtyRowid,
                INPUT  iprEbRowid,
                INPUT  iprEfRowid,
                INPUT  iprProbeRowid,

                OUTPUT  cCust,
                OUTPUT  dEstDate,
                OUTPUT  cEstNo,
                OUTPUT  cSellPrice,
                OUTPUT  cTonCost,
                OUTPUT  cTons,
                OUTPUT  dCalcPctsVal2,
                OUTPUT  dCtrl2-9 ,
                OUTPUT  dCtrl2-10,
                OUTPUT  dCtrl2-1,
                OUTPUT  dFullCost,
                OUTPUT  dNetProfitDol,
                OUTPUT  cNetProfitPct,
                OUTPUT  cOrdNo
                ).

if cCust <> "" then do:
            DISPLAY cCust            FORMAT "x(8)" COLUMN-LABEL "Customer"
                dEstDate           COLUMN-LABEL "date"
                cOrdNo             COLUMN-LABEL "Ord#"
                cEstNo       FORMAT "x(8)"
                COLUMN-LABEL "Est#"
                cSellPrice      COLUMN-LABEL "Selling Price"
                cTons                COLUMN-LABEL "Board Tons"
                ""                    COLUMN-LABEL "Board Cost / Ton (ERP)"
                ""                     COLUMN-LABEL "Board Cost / Ton (Actual)"
                ""                    COLUMN-LABEL  "Board Pad"
                dCalcPctsVal2        COLUMN-LABEL "GSA MU B"
                dCtrl2-9               COLUMN-LABEL "GSA MU M"
                dCtrl2-10              COLUMN-LABEL "GSA Labor"
                dFullCost             COLUMN-LABEL "ERP Cost"
                ""                    COLUMN-LABEL "ERP Margin ($)"
                ""           COLUMN-LABEL "ERP Margin (%)"
                ""              COLUMN-LABEL "Est. Cost"
                ""           COLUMN-LABEL "Est. Margin ($)"
                ""            COLUMN-LABEL "Est. Margin (%)"           
                WITH FRAME est DOWN NO-BOX STREAM-IO WIDTH 300.
        
            ASSIGN
                chWorkSheet:Range("C" + STRING(row-count)):VALUE = cCust .
                chWorkSheet:Range("D" + STRING(row-count)):VALUE = dEstDate .
                chWorkSheet:Range("E" + STRING(row-count)):VALUE = cOrdNo .
                chWorkSheet:Range("F" + STRING(row-count)):VALUE = cEstNo .
                chWorkSheet:Range("G" + STRING(row-count)):VALUE = cSellPrice .
                chWorkSheet:Range("H" + STRING(row-count)):VALUE = cTons .
                chWorkSheet:Range("I" + STRING(row-count)):VALUE =  cTonCost .
                /*chWorkSheet:Range("K" + STRING(row-count)):VALUE = "" .*/
                /*chWorkSheet:Range("K" + STRING(row-count)):VALUE = "" .*/
                chWorkSheet:Range("L" + STRING(row-count)):VALUE =  dCalcPctsVal2 .
                chWorkSheet:Range("M" + STRING(row-count)):VALUE = dCtrl2-9 .
                chWorkSheet:Range("N" + STRING(row-count)):VALUE = dCtrl2-10  .
                chWorkSheet:Range("O" + STRING(row-count)):VALUE = dCtrl2-1 .
                chWorkSheet:Range("P" + STRING(row-count)):VALUE = dFullCost . 
                chWorkSheet:Range("Q" + STRING(row-count)):VALUE = dNetProfitDol .
                chWorkSheet:Range("R" + STRING(row-count)):VALUE = cNetProfitPct .
          
            row-count = row-count + 1 .
end.   
        END. 
    END.

    chWorkSheet:ROWS(string(row-count) + ":1200"):EntireRow:Delete.

    /*OUTPUT STREAM st-excel CLOSE.*/
    /* RELEASE OBJECTS */

    /* size the columns automatically */
    chWorkbook:WorkSheets({&summary-sheet}):Columns("D:I"):AutoFit.
  

    /* let the user in */
    ASSIGN 
        chExcelApplication:VISIBLE = TRUE.
    chWorkbook:WorkSheets({&summary-sheet}):Activate NO-ERROR.

    ASSIGN
        chWorkSheet                       = chExcelApplication:Sheets:item({&summary-sheet})
        /* enable screen updating */
        chExcelApplication:ScreenUpdating = TRUE.


    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
    /*
    RELEASE xeb .
    RELEASE xop .
    RELEASE eb.
    RELEASE ef.
    RELEASE probe .
      */
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

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

