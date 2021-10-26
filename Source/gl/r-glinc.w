&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
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


DEFINE NEW SHARED VARIABLE v-ptd         AS DATE    FORMAT "99/99/9999" INITIAL TODAY NO-UNDO.
DEFINE NEW SHARED VARIABLE v-s-cos-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-cos-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-s-oper-no   LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-oper-no   LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-s-gen-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-gen-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-s-inc-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-inc-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-s-oth-no    LIKE account.actnum NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-oth-no    LIKE account.actnum NO-UNDO.

DEFINE            VARIABLE ptd-sales     AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-sales     AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ptd-cos       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-cos       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ptd-oper      AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-oper      AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ptd-gen       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-gen       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ptd-inc       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-inc       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ptd-oth       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ytd-oth       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-sales AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-sales AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-cos   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-cos   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-oper  AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-oper  AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-gen   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-gen   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-inc   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-inc   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-oth   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-oth   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-gross AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-gross AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ptd-exp   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE tot-ytd-exp   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v-ptd-per     AS DECIMAL FORMAT "->>9.99" NO-UNDO.
DEFINE            VARIABLE v-ytd-per     AS DECIMAL FORMAT "->>9.99" NO-UNDO.
DEFINE            VARIABLE v-year        LIKE period.yr.
DEFINE            VARIABLE v-period      LIKE period.pnum.
DEFINE            VARIABLE per-loop      AS INTEGER NO-UNDO.

DEFINE BUFFER xperiod FOR period.

DEFINE VARIABLE save_id    AS RECID NO-UNDO.
DEFINE VARIABLE time_stamp AS ch.
time_stamp = STRING(TIME, "HH:MMam").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-6 RECT-7 tran-date beg_acct-1 ~
end_acct-1 beg_acct-2 end_acct-2 beg_acct-3 end_acct-3 beg_acct-4 ~
end_acct-4 beg_acct-5 end_acct-5 rd-dest tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period beg_acct-1 ~
end_acct-1 beg_acct-2 end_acct-2 beg_acct-3 end_acct-3 beg_acct-4 ~
end_acct-4 beg_acct-5 end_acct-5 rd-dest tbAutoClose 

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

DEFINE VARIABLE beg_acct-1     AS CHARACTER FORMAT "X(25)":U 
    LABEL "Cost of Sales" 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-2     AS CHARACTER FORMAT "X(25)":U 
    LABEL "Operating Expense" 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-3     AS CHARACTER FORMAT "X(25)":U 
    LABEL "General & Admin" 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-4     AS CHARACTER FORMAT "X(25)":U 
    LABEL "Income Expense" 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-5     AS CHARACTER FORMAT "X(25)":U 
    LABEL "Other Expense" 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-1     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-2     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-3     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-4     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-5     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1 NO-UNDO.

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
    LABEL "Transaction Date" 
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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-11
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 98 BY 6.91.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 4.48.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 11.7.

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
    tran-date AT ROW 2.52 COL 45.8 COLON-ALIGNED
    tran-period AT ROW 3.52 COL 45.8 COLON-ALIGNED
    beg_acct-1 AT ROW 7.57 COL 23.2 COLON-ALIGNED
    end_acct-1 AT ROW 7.57 COL 61.2 COLON-ALIGNED NO-LABELS
    beg_acct-2 AT ROW 8.52 COL 23.2 COLON-ALIGNED
    end_acct-2 AT ROW 8.52 COL 61.2 COLON-ALIGNED NO-LABELS
    beg_acct-3 AT ROW 9.48 COL 23.2 COLON-ALIGNED
    end_acct-3 AT ROW 9.48 COL 61.2 COLON-ALIGNED NO-LABELS
    beg_acct-4 AT ROW 10.43 COL 23.2 COLON-ALIGNED
    end_acct-4 AT ROW 10.43 COL 61.2 COLON-ALIGNED NO-LABELS
    beg_acct-5 AT ROW 11.38 COL 23.2 COLON-ALIGNED
    end_acct-5 AT ROW 11.38 COL 61.2 COLON-ALIGNED NO-LABELS
    rd-dest AT ROW 14 COL 5 NO-LABELS
    lv-font-no AT ROW 14.1 COL 40.8 COLON-ALIGNED
    lines-per-page AT ROW 14.1 COL 95 COLON-ALIGNED
    lv-ornt AT ROW 14.14 COL 50.6 NO-LABELS
    lv-font-name AT ROW 15.38 COL 37.2 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 16.48 COL 31
    tbAutoClose AT ROW 18.33 COL 31.2 WIDGET-ID 64
    btn-ok AT ROW 19.33 COL 31
    btn-cancel AT ROW 19.33 COL 57.8
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 13.24 COL 4
    "To Account#" VIEW-AS TEXT
    SIZE 15 BY .71 AT ROW 6.62 COL 74.2
    "From Account#" VIEW-AS TEXT
    SIZE 17 BY .71 AT ROW 6.62 COL 34.2
    "  Account Ranges" VIEW-AS TEXT
    SIZE 22 BY .95 AT ROW 5.48 COL 38.2
    FONT 6
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 5
    RECT-11 AT ROW 6.05 COL 4
    RECT-6 AT ROW 13.67 COL 3
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 104.6 BY 21
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
        TITLE              = "GL Financial Statements"
        HEIGHT             = 19.86
        WIDTH              = 104
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

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tran-period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* GL Financial Statements */
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
ON WINDOW-CLOSE OF C-Win /* GL Financial Statements */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lk-recid AS RECID     NO-UNDO.

        CASE FOCUS:NAME :
            WHEN "beg_acct-1"  OR 
            WHEN "beg_acct-2" OR 
            WHEN "beg_acct-3" OR 
            WHEN "beg_acct-4" OR 
            WHEN "beg_acct-5" OR
            WHEN "end_acct-1"  OR 
            WHEN "end_acct-2" OR 
            WHEN "end_acct-3" OR 
            WHEN "end_acct-4" OR 
            WHEN "end_acct-5"
            THEN 
                DO:
                    RUN windows/l-acct.w (g_company,"E",FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                            .
                    END.
                END.
        END CASE.

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
        ASSIGN rd-dest
            tran-date
            tran-period
            beg_acct-1 END_acct-1
            beg_acct-2 END_acct-2
            beg_acct-3 END_acct-3
            beg_acct-4 END_acct-4
            beg_acct-5 END_acct-5
            .

        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.       

        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE. 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
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

    FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.  
    TRAN-date = TODAY.

    RUN init-proc.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "GW3" }
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
        IF AVAILABLE period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
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
    DISPLAY tran-date tran-period beg_acct-1 end_acct-1 beg_acct-2 end_acct-2 
        beg_acct-3 end_acct-3 beg_acct-4 end_acct-4 beg_acct-5 end_acct-5 
        rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-11 RECT-6 RECT-7 tran-date beg_acct-1 end_acct-1 beg_acct-2 
        end_acct-2 beg_acct-3 end_acct-3 beg_acct-4 end_acct-4 beg_acct-5 
        end_acct-5 rd-dest tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
    /*------------------------------------------------------------------------------
      Purpose:     gl/gl-inc1.p
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN gl/gl-inc1.p.


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
    /*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
         DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
         DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
     */
    RUN CUSTOM\PRNTPROC.P (list-name,INT(LV-FONT-NO),LV-ORNT). /* open file-name, title */ 

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(LV-FONT-NO),LV-ORNT). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {sys/form/r-top3.f}

    FORM HEADER
        SKIP(1)
        "PTD Post" TO 40 "%Sales" TO 50
        "YTD Post" TO 65 "%Sales" TO 75
        "========" TO 40 "======" TO 50
        "========" TO 65 "======" TO 75
        WITH FRAME head-columns NO-BOX NO-LABELS WIDTH 80 PAGE-TOP STREAM-IO.

    FORM
        account.dscr AT 1 FORMAT "x(25)"
        ptd-sales TO 40
        v-ptd-per TO 50
        ytd-sales TO 65
        v-ytd-per TO 75
        WITH FRAME line-item DOWN NO-BOX NO-LABELS WIDTH 80 STREAM-IO.

    ASSIGN 
        v-ptd       = tran-date
        v-s-cos-no  = beg_acct-1
        v-e-cos-no  = END_acct-1
        v-s-oper-no = beg_acct-2
        v-e-oper-no = END_acct-2
        v-s-gen-no  = beg_acct-3
        v-e-gen-no  = END_acct-3
        v-s-inc-no  = beg_acct-4
        v-e-inc-no  = END_acct-4
        v-s-oth-no  = beg_acct-5
        v-e-oth-no  = END_acct-5
        .

    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.
    SESSION:SET-WAIT-STATE("general").
    tot-ytd-sales = 0.
    tot-ptd-sales = 0.
    ptd-sales = 0.
    ytd-sales = 0.
    tot-ptd-cos = 0.
    tot-ytd-cos = 0.
    tot-ptd-gross = 0.
    tot-ytd-gross = 0.
    tot-ptd-gen = 0.
    tot-ytd-gen = 0.

    FIND FIRST period WHERE period.company = cocode AND
        period.pst <= v-ptd AND
        period.pend >= v-ptd NO-LOCK NO-ERROR.
    IF AVAILABLE period THEN
        ASSIGN v-year   = period.yr
            v-period = period.pnum.

    str-tit  = coname + " - " + loname.
    str-tit2 = "INCOME STATEMENT" .

    str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

    x = (56 - length(str-tit)) / 2.
    str-tit  = FILL(" ",x) + str-tit .
    x = (57 - length(str-tit2)) / 2.
    str-tit2 = FILL(" ",x) + str-tit2 .
    x = (78 - length(str-tit3)) / 2.
    str-tit3 = FILL(" ",x) + str-tit3 .

    VIEW FRAME r-top.
    VIEW FRAME head-columns.

    FIND FIRST period WHERE period.company = cocode AND
        period.pst <= v-ptd AND
        period.pend >= v-ptd NO-LOCK NO-ERROR.
    IF AVAILABLE period THEN
        ASSIGN v-year   = period.yr
            v-period = period.pnum.

    str-tit  = coname + " - " + loname.
    str-tit2 = "INCOME STATEMENT" .

    str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

    x = (56 - length(str-tit)) / 2.
    str-tit  = FILL(" ",x) + str-tit .
    x = (57 - length(str-tit2)) / 2.
    str-tit2 = FILL(" ",x) + str-tit2 .
    x = (78 - length(str-tit3)) / 2.
    str-tit3 = FILL(" ",x) + str-tit3 .

    VIEW FRAME r-top.
    VIEW FRAME head-columns.

    /*  Sales Totals */
    FOR EACH account WHERE account.company EQ cocode AND
        account.type EQ "R" USE-INDEX type NO-LOCK:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
        END.


        DO per-loop = 1 TO (v-period - 1):
            FIND FIRST xperiod WHERE xperiod.company = cocode AND
                xperiod.pnum = per-loop AND
                xperiod.yr = v-year
                NO-LOCK NO-ERROR.
            IF AVAILABLE xperiod THEN
                FOR EACH glhist NO-LOCK
                    WHERE glhist.company EQ account.company
                    AND glhist.actnum  EQ account.actnum
                    AND glhist.period  EQ per-loop
                    AND glhist.tr-date GE xperiod.pst
                    AND glhist.tr-date LE xperiod.pend
                    :
                    tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
                END.
        END.

        ASSIGN 
            tot-ytd-sales = tot-ytd-sales + account.cyr-open.

    END.  /* Sales Totals for each */

    ASSIGN 
        tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
        tot-ptd-sales = - tot-ptd-sales
        tot-ytd-sales = - tot-ytd-sales.

    PUT "========  Sales  ========" SKIP(1).

    /*  Sales Totals */
    FOR EACH account WHERE account.company EQ cocode AND
        account.type EQ "R" NO-LOCK
        USE-INDEX type BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-sales = ptd-sales + glhist.tr-amt.
        END.


        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-sales = ytd-sales + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((- ptd-sales / tot-ptd-sales) * 100)
                ytd-sales = ytd-sales + ptd-sales + account.cyr-open
                v-ytd-per = ((- ytd-sales / tot-ytd-sales) * 100).
            DISPLAY account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
                (- ytd-sales) @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                ptd-sales = 0
                ytd-sales = 0.
        END.
    END.  /* Sales Totals for each */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Sales" AT 1
        tot-ptd-sales TO 40
        100.00 TO 50
        tot-ytd-sales TO 65
        100.00 TO 75 SKIP(1).
    PUT "====  Cost of Sales  ====" SKIP(1).

    /*  Cost of Sales */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-cos-no AND
        account.actnum <= v-e-cos-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-cos = ptd-cos + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-cos = ytd-cos + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
                ytd-cos   = ytd-cos + ptd-cos + account.cyr-open
                v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
            DISPLAY account.dscr ptd-cos @ ptd-sales v-ptd-per
                ytd-cos @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-cos = tot-ptd-cos + ptd-cos
                tot-ytd-cos = tot-ytd-cos + ytd-cos
                ptd-cos     = 0
                ytd-cos     = 0.
        END.
    END.  /* Cost of Sales */

    ASSIGN 
        tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
        tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Cost of Sales" AT 1 tot-ptd-cos TO 40
        ((tot-ptd-cos / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-cos TO 65
        ((tot-ytd-cos / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Gross Margin" tot-ptd-gross TO 40
        ((tot-ptd-gross / tot-ptd-sales) * 100) TO 50
        tot-ytd-gross TO 65
        ((tot-ytd-gross / tot-ytd-sales) * 100) TO 75 SKIP(1).
    PUT "===  Operating Expenses  ===" SKIP(1).

    /*  Operating Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-oper-no AND
        account.actnum <= v-e-oper-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-oper = ptd-oper + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-oper = ytd-oper + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-oper / tot-ptd-sales) * 100)
                ytd-oper  = ytd-oper + ptd-oper + account.cyr-open
                v-ytd-per = ((ytd-oper / tot-ytd-sales) * 100).
            DISPLAY account.dscr ptd-oper @ ptd-sales v-ptd-per
                ytd-oper @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-oper = tot-ptd-oper + ptd-oper
                tot-ytd-oper = tot-ytd-oper + ytd-oper
                ptd-oper     = 0
                ytd-oper     = 0.
        END.
    END.  /* Operating Expenses */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Operating Expenses" AT 1 tot-ptd-oper TO 40
        ((tot-ptd-oper / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-oper TO 65
        ((tot-ytd-oper / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "General & Administrative" SKIP(1).

    /*  General/Admin Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-gen-no AND
        account.actnum <= v-e-gen-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FIND FIRST period WHERE period.company = cocode AND
            period.pst <= v-ptd AND
            period.pend >= v-ptd NO-LOCK NO-ERROR.

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum 
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-gen = ptd-gen + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-gen = ytd-gen + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
                ytd-gen   = ytd-gen + ptd-gen + account.cyr-open
                v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
            DISPLAY account.dscr ptd-gen @ ptd-sales v-ptd-per
                ytd-gen @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-gen = tot-ptd-gen + ptd-gen
                tot-ytd-gen = tot-ytd-gen + ytd-gen
                ptd-gen     = 0
                ytd-gen     = 0.
        END.
    END.  /* Gen & Admin Expenses */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total General & Admin" AT 1 tot-ptd-gen TO 40
        ((tot-ptd-gen / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-gen TO 65
        ((tot-ytd-gen / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "Income Tax Expenses" SKIP(1).

    /*  Income Tax Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-inc-no AND
        account.actnum <= v-e-inc-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FIND FIRST period WHERE period.company = cocode AND
            period.pst <= v-ptd AND
            period.pend >= v-ptd NO-LOCK NO-ERROR.

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-inc = ptd-inc + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-inc = ytd-inc + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
                ytd-inc   = ytd-inc + ptd-inc + account.cyr-open
                v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
            DISPLAY account.dscr ptd-inc @ ptd-sales v-ptd-per
                ytd-inc @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-inc = tot-ptd-inc + ptd-inc
                tot-ytd-inc = tot-ytd-inc + ytd-inc
                ptd-inc     = 0
                ytd-inc     = 0.
        END.
    END.  /* Operating Expenses */

    ASSIGN 
        tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
        tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Income Tax Expense" AT 1 tot-ptd-inc TO 40
        ((tot-ptd-inc / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-inc TO 65
        ((tot-ytd-inc / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Operating Expenses" AT 1
        tot-ptd-exp TO 40
        ((tot-ptd-exp / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-exp TO 65
        ((tot-ytd-exp / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "Net Income Before Taxes"
        (tot-ptd-gross - tot-ptd-exp) TO 40 FORMAT "->>,>>>,>>9.99"
        (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) TO 50
        (tot-ytd-gross - tot-ytd-exp) TO 65 FORMAT "->>,>>>,>>9.99"
        (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) TO 75 SKIP.
    PUT "==============" TO 40 "=======" TO 50
        "==============" TO 65 "=======" TO 75 SKIP(1).

    /*  Other Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-oth-no AND
        account.actnum <= v-e-oth-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FIND FIRST period WHERE period.company = cocode AND
            period.pst <= v-ptd AND
            period.pend >= v-ptd NO-LOCK NO-ERROR.

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period 
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-oth = ptd-oth + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-oth = ytd-oth + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
                ytd-oth   = ytd-oth + ptd-oth + account.cyr-open
                v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
            DISPLAY account.dscr ptd-oth @ ptd-sales v-ptd-per
                ytd-oth @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-oth = tot-ptd-oth + ptd-oth
                tot-ytd-oth = tot-ytd-oth + ytd-oth
                ptd-oth     = 0
                ytd-oth     = 0.
        END.
    END.  /* Other Expenses */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.

    PUT "Net Income After Taxes"
        ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) TO 40 FORMAT "->>,>>>,>>9.99"
        ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) TO 50
        ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) TO 65 FORMAT "->>,>>>,>>9.99"
        ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) TO 75 SKIP.
    PUT "==============" TO 40 "=======" TO 50
        "==============" TO 65 "=======" TO 75 SKIP. 

    SESSION:SET-WAIT-STATE("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sdf C-Win 
PROCEDURE sdf :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    {sys/form/r-top3.f}

    FORM HEADER
        SKIP(1)
        "PTD Post" TO 40 "%Sales" TO 50
        "YTD Post" TO 65 "%Sales" TO 75
        "========" TO 40 "======" TO 50
        "========" TO 65 "======" TO 75
        WITH FRAME head-columns NO-BOX NO-LABELS WIDTH 80 PAGE-TOP STREAM-IO.

    FORM
        account.dscr AT 1 FORMAT "x(25)"
        ptd-sales TO 40
        v-ptd-per TO 50
        ytd-sales TO 65
        v-ytd-per TO 75
        WITH FRAME line-item DOWN NO-BOX NO-LABELS WIDTH 80 STREAM-IO.

    ASSIGN 
        v-ptd       = tran-date
        v-s-cos-no  = beg_acct-1
        v-e-cos-no  = END_acct-1
        v-s-oper-no = beg_acct-2
        v-e-oper-no = END_acct-2
        v-s-gen-no  = beg_acct-3
        v-e-gen-no  = END_acct-3
        v-s-inc-no  = beg_acct-4
        v-e-inc-no  = END_acct-4
        v-s-oth-no  = beg_acct-5
        v-e-oth-no  = END_acct-5
        .

    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.
    SESSION:SET-WAIT-STATE("general").
    tot-ytd-sales = 0.
    tot-ptd-sales = 0.
    ptd-sales = 0.
    ytd-sales = 0.
    tot-ptd-cos = 0.
    tot-ytd-cos = 0.
    tot-ptd-gross = 0.
    tot-ytd-gross = 0.
    tot-ptd-gen = 0.
    tot-ytd-gen = 0.

    FIND FIRST period WHERE period.company = cocode AND
        period.pst <= v-ptd AND
        period.pend >= v-ptd NO-LOCK NO-ERROR.
    IF AVAILABLE period THEN
        ASSIGN v-year   = period.yr
            v-period = period.pnum.

    str-tit  = coname + " - " + loname.
    str-tit2 = "INCOME STATEMENT" .

    str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

    x = (56 - length(str-tit)) / 2.
    str-tit  = FILL(" ",x) + str-tit .
    x = (57 - length(str-tit2)) / 2.
    str-tit2 = FILL(" ",x) + str-tit2 .
    x = (78 - length(str-tit3)) / 2.
    str-tit3 = FILL(" ",x) + str-tit3 .

    VIEW FRAME r-top.
    VIEW FRAME head-columns.

    /*  Sales Totals */
    FOR EACH account WHERE account.company EQ cocode AND
        account.type EQ "R" USE-INDEX type NO-LOCK:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
        END.

        DO per-loop = 1 TO (v-period - 1):
            FIND FIRST xperiod WHERE xperiod.company = cocode AND
                xperiod.pnum = per-loop AND
                xperiod.yr = v-year
                NO-LOCK NO-ERROR.
            IF AVAILABLE xperiod THEN
                FOR EACH glhist NO-LOCK
                    WHERE glhist.company EQ account.company
                    AND glhist.actnum  EQ account.actnum
                    AND glhist.period  EQ per-loop
                    AND glhist.tr-date GE xperiod.pst
                    AND glhist.tr-date LE xperiod.pend
                    :
                    tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
                END.
        END.

        ASSIGN 
            tot-ytd-sales = tot-ytd-sales + account.cyr-open.

    END.  /* Sales Totals for each */

    ASSIGN 
        tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
        tot-ptd-sales = - tot-ptd-sales
        tot-ytd-sales = - tot-ytd-sales.

    PUT "========  Sales  ========" SKIP(1).

    /*  Sales Totals */
    FOR EACH account WHERE account.company EQ cocode AND
        account.type EQ "R" NO-LOCK
        USE-INDEX type BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-sales = ptd-sales + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-sales = ytd-sales + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = IF tot-ptd-sales <> 0 THEN ((- ptd-sales / tot-ptd-sales) * 100)
                         ELSE 0
                ytd-sales = ytd-sales + ptd-sales + account.cyr-open
                v-ytd-per = IF tot-ytd-sales <> 0 THEN ((- ytd-sales / tot-ytd-sales) * 100)
                         ELSE 0.

            DISPLAY account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
                (- ytd-sales) @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN STREAM-IO.     
            DOWN WITH FRAME line-item.
            ASSIGN 
                ptd-sales = 0
                ytd-sales = 0.
        END.
    END.  /* Sales Totals for each */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Sales" AT 1
        tot-ptd-sales TO 40
        100.00 TO 50
        tot-ytd-sales TO 65
        100.00 TO 75 SKIP(1).
    PUT "====  Cost of Sales  ====" SKIP(1).

    /*  Cost of Sales */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-cos-no AND
        account.actnum <= v-e-cos-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-cos = ptd-cos + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-cos = ytd-cos + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
                ytd-cos   = ytd-cos + ptd-cos + account.cyr-open
                v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
            IF v-ptd-per = ? THEN v-ptd-per = 0.
            IF v-ytd-per = ? THEN v-ytd-per = 0.
            DISPLAY account.dscr ptd-cos @ ptd-sales v-ptd-per
                ytd-cos @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-cos = tot-ptd-cos + ptd-cos
                tot-ytd-cos = tot-ytd-cos + ytd-cos
                ptd-cos     = 0
                ytd-cos     = 0.
        END.
    END.  /* Cost of Sales */
    ASSIGN 
        tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
        tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Cost of Sales" AT 1 tot-ptd-cos TO 40
        ((tot-ptd-cos / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-cos TO 65
        ((tot-ytd-cos / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Gross Margin" tot-ptd-gross TO 40
        ((tot-ptd-gross / tot-ptd-sales) * 100) TO 50
        tot-ytd-gross TO 65
        ((tot-ytd-gross / tot-ytd-sales) * 100) TO 75 SKIP(1).
    PUT "===  Operating Expenses  ===" SKIP(1).

    /*  Operating Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-oper-no AND
        account.actnum <= v-e-oper-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-oper = ptd-oper + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-gen = ytd-gen + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
                ytd-gen   = ytd-gen + ptd-gen + account.cyr-open
                v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
            IF v-ptd-per = ? THEN v-ptd-per = 0.
            IF v-ytd-per = ? THEN v-ytd-per = 0.
            DISPLAY account.dscr ptd-gen @ ptd-sales v-ptd-per
                ytd-gen @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-gen = tot-ptd-gen + ptd-gen
                tot-ytd-gen = tot-ytd-gen + ytd-gen
                ptd-gen     = 0
                ytd-gen     = 0.
        END.
    END.  /* Gen & Admin Expenses */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total General & Admin" AT 1 tot-ptd-gen TO 40
        ((tot-ptd-gen / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-gen TO 65
        ((tot-ytd-gen / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "Income Tax Expenses" SKIP(1).

    /*  Income Tax Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-inc-no AND
        account.actnum <= v-e-inc-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FIND FIRST period WHERE period.company = cocode AND
            period.pst <= v-ptd AND
            period.pend >= v-ptd NO-LOCK NO-ERROR.

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-inc = ptd-inc + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-inc = ytd-inc + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
                ytd-inc   = ytd-inc + ptd-inc + account.cyr-open
                v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
            IF v-ptd-per = ? THEN v-ptd-per = 0.
            IF v-ytd-per = ? THEN v-ytd-per = 0.
            DISPLAY account.dscr ptd-inc @ ptd-sales v-ptd-per
                ytd-inc @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-inc = tot-ptd-inc + ptd-inc
                tot-ytd-inc = tot-ytd-inc + ytd-inc
                ptd-inc     = 0
                ytd-inc     = 0.
        END.
    END.  /* Operating Expenses */


    ASSIGN 
        tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
        tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Income Tax Expense" AT 1 tot-ptd-inc TO 40
        ((tot-ptd-inc / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-inc TO 65
        ((tot-ytd-inc / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.
    PUT "Total Operating Expenses" AT 1
        tot-ptd-exp TO 40
        ((tot-ptd-exp / tot-ptd-sales) * 100.00) TO 50
        tot-ytd-exp TO 65
        ((tot-ytd-exp / tot-ytd-sales) * 100.00) TO 75 SKIP(1).
    PUT "Net Income Before Taxes"
        (tot-ptd-gross - tot-ptd-exp) TO 40 FORMAT "->>,>>>,>>9.99"
        (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) TO 50
        (tot-ytd-gross - tot-ytd-exp) TO 65 FORMAT "->>,>>>,>>9.99"
        (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) TO 75 SKIP.
    PUT "==============" TO 40 "=======" TO 50
        "==============" TO 65 "=======" TO 75 SKIP(1).

    /*  Other Expenses */
    FOR EACH account WHERE account.company EQ cocode AND
        account.actnum >= v-s-oth-no AND
        account.actnum <= v-e-oth-no
        NO-LOCK USE-INDEX account BREAK BY account.actnum:

        FIND FIRST period WHERE period.company = cocode AND
            period.pst <= v-ptd AND
            period.pend >= v-ptd NO-LOCK NO-ERROR.

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ v-period
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend
            :
            ptd-oth = ptd-oth + glhist.tr-amt.
        END.

        IF LAST-OF(account.actnum) THEN
        DO:

            DO per-loop = 1 TO (v-period - 1):
                FIND FIRST xperiod WHERE xperiod.company = cocode AND
                    xperiod.pnum = per-loop AND
                    xperiod.yr = v-year
                    NO-LOCK NO-ERROR.
                IF AVAILABLE xperiod THEN
                    FOR EACH glhist NO-LOCK
                        WHERE glhist.company EQ account.company
                        AND glhist.actnum  EQ account.actnum
                        AND glhist.period  EQ per-loop
                        AND glhist.tr-date GE xperiod.pst
                        AND glhist.tr-date LE xperiod.pend
                        :
                        ytd-oth = ytd-oth + glhist.tr-amt.
                    END.
            END.

            ASSIGN 
                v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
                ytd-oth   = ytd-oth + ptd-oth + account.cyr-open
                v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
            IF v-ptd-per = ? THEN v-ptd-per = 0.
            IF v-ytd-per = ? THEN v-ytd-per = 0.
            DISPLAY account.dscr ptd-oth @ ptd-sales v-ptd-per
                ytd-oth @ ytd-sales v-ytd-per
                WITH FRAME line-item OVERLAY DOWN.
            DOWN WITH FRAME line-item.
            ASSIGN 
                tot-ptd-oth = tot-ptd-oth + ptd-oth
                tot-ytd-oth = tot-ytd-oth + ytd-oth
                ptd-oth     = 0
                ytd-oth     = 0.
        END.
    END.  /* Other Expenses */

    PUT "--------------" TO 40 "-------" TO 50
        "--------------" TO 65 "-------" TO 75 SKIP.

    PUT "Net Income After Taxes"
        ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) TO 40 FORMAT "->>,>>>,>>9.99"
        ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) TO 50
        ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) TO 65 FORMAT "->>,>>>,>>9.99"
        ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) TO 75 SKIP.
    PUT "==============" TO 40 "=======" TO 50
        "==============" TO 65 "=======" TO 75 SKIP.

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

