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
DEFINE STREAM st-export.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_emp end_emp begin_mach ~
end_mach begin_start-date end_start-date tbAutoClose btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_emp end_emp begin_mach end_mach ~
begin_start-date end_start-date tbAutoClose 

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
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
    LABEL "&Start Process"
    SIZE 16 BY 1.29 .

DEFINE VARIABLE begin_emp        AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Employee" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach       AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Machine#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_start-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Login Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_emp          AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Employee" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach         AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Machine#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_start-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Login Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.38.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_emp AT ROW 6.24 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Estimate Number"
    end_emp AT ROW 6.24 COL 70 COLON-ALIGNED HELP
    "Enter Ending Employee Number"
    begin_mach AT ROW 7.43 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Machine Number"
    end_mach AT ROW 7.43 COL 70 COLON-ALIGNED HELP
    "Enter Ending Machine Number"
    begin_start-date AT ROW 8.62 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Login Date"
    end_start-date AT ROW 8.62 COL 70 COLON-ALIGNED HELP
    "Enter Ending Login Date"
    tbAutoClose AT ROW 10.76 COL 31 WIDGET-ID 58
    btn-process AT ROW 11.71 COL 31
    btn-cancel AT ROW 11.71 COL 55
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .62 AT ROW 4.97 COL 5
    RECT-17 AT ROW 5.33 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 12.86
    BGCOLOR 15 .

DEFINE FRAME FRAME-B
    "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
    SIZE 84 BY .95 AT ROW 1.95 COL 7
    FGCOLOR 15 FONT 5
    "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
    SIZE 76 BY .95 AT ROW 2.91 COL 11
    FGCOLOR 15 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 3.81
    BGCOLOR 21 .


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
        TITLE              = "TS Purge Zero Time Transmactions"
        HEIGHT             = 12.86
        WIDTH              = 95.8
        MAX-HEIGHT         = 20.71
        MAX-WIDTH          = 104.6
        VIRTUAL-HEIGHT     = 20.71
        VIRTUAL-WIDTH      = 104.6
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* TS Purge Zero Time Transmactions */
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
ON WINDOW-CLOSE OF C-Win /* TS Purge Zero Time Transmactions */
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
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
  
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
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
    DISPLAY begin_emp end_emp begin_mach end_mach begin_start-date end_start-date 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_emp end_emp begin_mach end_mach begin_start-date 
        end_start-date tbAutoClose btn-process btn-cancel 
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
    /* ------------------------------------------------ sys/del/est.p 3/29/95 CTS */
    /* Delete / Archive old estimates                                             */
    /* -------------------------------------------------------------------------- */

    OUTPUT STREAM st-export TO value("c:\tmp\del0trans." + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY))
        + STRING(DAY(TODAY)) + STRING(TIME)  ).

    SESSION:SET-WAIT-STATE("General").

    DO WITH FRAME {&frame-name}:
        ASSIGN {&DISPLAYED-OBJECTS}.  
    END.

    SESSION:SET-WAIT-STATE("").

    MESSAGE "Are you sure you want to delete the all 0 hour Login/Machine/Employee transactions for the " +
        "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    IF v-process THEN 
    DO:
        SESSION:SET-WAIT-STATE("general").

        PUT STREAM st-export "Exporting Emplogin" SKIP.
        /* delete start_date = ? */
        FOR EACH emplogin WHERE emplogin.company = cocode 
            AND emplogin.START_date = ?:
            EXPORT STREAM st-export emplogin.
            DELETE emplogin.
        END.
        /* delete transaction - ended but no hours */
        FOR EACH emplogin WHERE emplogin.company = cocode 
            AND emplogin.employee >= begin_emp
            AND emplogin.employee <= END_emp
            AND emplogin.START_date >= begin_start-date
            AND emplogin.start_date <= END_start-date
            AND emplogin.END_date <> ?
            AND emplogin.TOTAL_time = 0:
            EXPORT STREAM st-export emplogin.
            DELETE emplogin.
        END.
        PUT STREAM st-export "Exporting Machtran" SKIP.      
        FOR EACH machtran WHERE machtran.company = cocode
            AND machtran.machine >= begin_mach
            AND machtran.machine <= END_mach
            AND machtran.START_date >= begin_start-date
            AND machtran.START_date <= END_start-date
            AND machtran.END_date <> ?
            AND machtran.TOTAL_time = 0:
            EXPORT STREAM st-export machtran.
            DELETE machtran.
        END.
        PUT STREAM st-export "Exporting Machemp" SKIP.      
        FOR EACH machtran WHERE machemp.employee >= begin_emp
            AND machemp.employee <= END_emp
            AND machemp.START_date >= begin_start-date
            AND machemp.START_date <= END_start-date
            AND machemp.END_date <> ?
            AND machemp.TOTAL_time = 0:
            EXPORT STREAM st-export machemp.
            DELETE machemp.
        END.
        SESSION:SET-WAIT-STATE("").
        OUTPUT STREAM st-export CLOSE.

        MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
        APPLY "close" TO THIS-PROCEDURE.
    END.

    RETURN NO-APPLY.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

