&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:             util/fgHisPur.w
  Description:      Utility to Purge fg-rcpth, fg-rdtl and fg-rctd records
  Input Parms:      <none>
  Output Parms:     <none>
  Author:           Ron Stark
  Created:          01/12/2000
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
DEFINE VARIABLE v-process AS LOG NO-UNDO.
DEFINE VARIABLE cFileName AS CHAR NO-UNDO.
DEFINE BUFFER bfg-rcpth FOR fg-rcpth.

DEFINE STREAM file1.
DEFINE STREAM file2.
DEFINE STREAM file3.
DEFINE STREAM file4.

ASSIGN
    cocode = gcompany
    locode = gloc
    cFileName = STRING(YEAR(TODAY),"9999") + 
                STRING(MONTH(TODAY),"99") + 
                STRING(DAY(TODAY),"99") + 
                STRING(TIME) + ".d".

OUTPUT STREAM file1 TO VALUE("c:\tmp\fg-rcpth" + "-" + cFileName).
OUTPUT STREAM file2 TO VALUE("c:\tmp\fg-rdtlh" + "-" + cFileName).
OUTPUT STREAM file3 TO VALUE("c:\tmp\fg-rctd" + "-" + cFileName).
OUTPUT STREAM file3 TO VALUE("c:\tmp\fg-rdtlh" + "-" + cFileName).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no begin_date ~
end_date begin_type end_type begin_job-no begin_job-no2 end_job-no ~
end_job-no2 btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_date end_date ~
begin_type end_type begin_job-no begin_job-no2 end_job-no end_job-no2 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Begin Trans Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(1)":U 
    LABEL "From Trans Type" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Trans Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(1)":U INITIAL "z" 
    LABEL "To Trans Type" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_i-no AT ROW 6.62 COL 22 COLON-ALIGNED HELP
    "Enter Beginning FG Item Number"
    end_i-no AT ROW 6.62 COL 64 COLON-ALIGNED HELP
    "Enter Ending FG ItemNumber"
    begin_date AT ROW 8.29 COL 22 COLON-ALIGNED HELP
    "Enter Beginning Transaction Date"
    end_date AT ROW 8.29 COL 64 COLON-ALIGNED HELP
    "Enter Ending Transaction date"
    begin_type AT ROW 10 COL 22 COLON-ALIGNED HELP
    "Enter Beginning Trans Type" WIDGET-ID 2
    end_type AT ROW 10 COL 64 COLON-ALIGNED HELP
    "Enter Ending Trans Time" WIDGET-ID 4
    begin_job-no AT ROW 11.67 COL 22 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 11.67 COL 38 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 11.67 COL 64 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 11.67 COL 80 COLON-ALIGNED HELP
    "Enter Ending Job Number"
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
    SIZE 90.2 BY 17.71.

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
        TITLE              = "Purge FG History"
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
   FRAME-NAME                                                           */
ASSIGN
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
    btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge FG History */
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
ON WINDOW-CLOSE OF C-Win /* Purge FG History */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type C-Win
ON HELP OF begin_type IN FRAME FRAME-A /* Trans Code */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN windows/l-tranCd.w (begin_type:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN 
            begin_type:SCREEN-VALUE = ENTRY(1,char-val).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type C-Win
ON HELP OF end_type IN FRAME FRAME-A /* Trans Code */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN windows/l-tranCd.w (end_type:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN 
            end_type:SCREEN-VALUE = ENTRY(1,char-val).
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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
    /* check security */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
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
    DISPLAY begin_i-no end_i-no begin_date end_date begin_type end_type 
        begin_job-no begin_job-no2 end_job-no end_job-no2 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_i-no end_i-no begin_date end_date begin_type end_type 
        begin_job-no begin_job-no2 end_job-no end_job-no2 btn-process 
        btn-cancel 
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
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
    DEFINE VARIABLE fitm        LIKE itemfg.i-no         INIT "".
    DEFINE VARIABLE titm        LIKE fitm                INIT "zzzzzzzzzzzzzzz".
    DEFINE VARIABLE fdat        LIKE fg-rcpth.trans-date INIT 01/01/01.
    DEFINE VARIABLE tdat        LIKE fdat                INIT TODAY.
    DEFINE VARIABLE fjob        LIKE fg-bin.job-no       INIT "".
    DEFINE VARIABLE tjob        LIKE fjob                INIT "zzzzzz".
    DEFINE VARIABLE ftyp        AS CHARACTER         INIT "".
    DEFINE VARIABLE ttyp        LIKE ftyp                INIT "z".
    DEFINE VARIABLE v-i-no      LIKE fg-rcpth.i-no.

    DISABLE TRIGGERS FOR LOAD OF fg-rcpth.
    DISABLE TRIGGERS FOR LOAD OF fg-rdtlh.
    DISABLE TRIGGERS FOR LOAD OF fg-rctd.
    
    DO WITH FRAME {&frame-name}:
        ASSIGN {&displayed-objects}.
    END.

    ASSIGN
        fitm      = begin_i-no
        titm      = end_i-no
        fdat      = begin_date
        tdat      = end_date
        fjob      = FILL(" ",6 - length(TRIM(begin_job-no))) + trim(begin_job-no) + string(int(begin_job-no2),"99")
        tjob      = FILL(" ",6 - length(TRIM(end_job-no)))   + trim(end_job-no)   + string(int(end_job-no2),"99")
        v-process = NO
        ftyp     = begin_type
        ttyp     = END_type
        .

    MESSAGE "Are you sure you want to delete the FG History within the " +
        "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    IF v-process THEN 
    DO: 
        /* Note: this may find more records than needed (if job-no2 doesn't match)
           but allows use of index.  "Extra" records removed in next statement */
        FOR EACH fg-rcpth NO-LOCK WHERE 
                fg-rcpth.company    EQ cocode AND 
                fg-rcpth.i-no       GT v-i-no  AND 
                fg-rcpth.i-no       GE fitm AND 
                fg-rcpth.i-no       LE titm AND 
                fg-rcpth.trans-date GE fdat AND 
                fg-rcpth.trans-date LE tdat AND 
                fg-rcpth.rita-code  GE ftyp AND 
                fg-rcpth.rita-code  LE ttyp AND 
                fg-rcpth.job-no     GE FILL(" ",6 - LENGTH(TRIM(begin_job-no))) + TRIM(begin_job-no) AND 
                fg-rcpth.job-no     LE FILL(" ",6 - LENGTH(TRIM(end_job-no))) + TRIM(end_job-no) 
                USE-INDEX tran:
            
            IF (fg-rcpth.job-no EQ TRIM(begin_job-no) AND STRING(fg-rcpth.job-no2,"99") LT end_job-no2)
            OR (fg-rcpth.job-no EQ TRIM(end_job-no) AND STRING(fg-rcpth.job-no2,"99") GT end_job-no2)
            THEN NEXT.

            FOR EACH fg-rdtlh EXCLUSIVE WHERE 
                fg-rdtlh.r-no EQ fg-rcpth.r-no AND 
                fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                TRANSACTION:
                EXPORT STREAM file2 fg-rdtlh.
                DELETE fg-rdtlh.
            END.

            FOR EACH fg-rctd EXCLUSIVE WHERE 
                fg-rctd.company   EQ fg-rcpth.company AND 
                fg-rctd.i-no      EQ fg-rcpth.i-no AND 
                fg-rctd.rita-code EQ "P" AND 
                fg-rctd.job-no    EQ fg-rcpth.job-no AND 
                fg-rctd.job-no2   EQ fg-rcpth.job-no2
                TRANSACTION:
                EXPORT STREAM file3 fg-rctd.
                DELETE fg-rctd.
            END.

            DO TRANSACTION:
                FIND bfg-rcpth EXCLUSIVE WHERE 
                    ROWID(bfg-rcpth) EQ ROWID(fg-rcpth)
                    NO-ERROR.
                IF AVAIL bfg-rcpth THEN DO: 
                    EXPORT STREAM file1 bfg-rcpth.
                    DELETE bfg-rcpth.
                END.
            END.

        END. /* EACH fg-rcpth */

        MESSAGE 
            TRIM(c-win:TITLE) + " Process Is Completed." SKIP 
            "Purged data saved in C:\tmp directory."
            VIEW-AS ALERT-BOX.
        
        APPLY "close" TO THIS-PROCEDURE.
    END.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

