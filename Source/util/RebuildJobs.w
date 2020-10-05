&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
DEFINE NEW SHARED VARIABLE nufile AS LOG     NO-UNDO.   /* for jc-calc.p */
DEFINE NEW SHARED VARIABLE lv-qty AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE fil_id AS RECID   NO-UNDO.
{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = g_company
    locode = g_loc. 

DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO. 
DEFINE BUFFER bf-job-hdr FOR job-hdr.  

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define QUERY-STRING-FRAME-A FOR EACH item SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH item SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A item
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_job-no fiFromJobNo2 end_job-no ~
fiToJobNo2 btn-process btn-cancel rd_methods tb_prod-qty rd_run-methods ~
RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no fiFromJobNo2 end_job-no ~
fiToJobNo2 rd_methods tb_prod-qty rd_run-methods lbl_opt 

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

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(256)":U 
    LABEL "From Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(256)":U 
    LABEL "To Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromJobNo2   AS INTEGER   FORMAT "99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiToJobNo2     AS INTEGER   FORMAT "99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_opt        AS CHARACTER FORMAT "X(256)":U INITIAL "Select Option:" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 NO-UNDO.

DEFINE VARIABLE rd_methods     AS CHARACTER INITIAL "Recalc" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Rebuild Job", "Rebuild",
    "Recalc Job", "Recalc"
    SIZE 42.6 BY 1 NO-UNDO.

DEFINE VARIABLE rd_run-methods AS CHARACTER INITIAL "Range" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "All Open Jobs", "All",
    "Range of Jobs", "Range"
    SIZE 42.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 9.05.

DEFINE VARIABLE tb_prod-qty AS LOGICAL INITIAL NO 
    LABEL "Jobs with no production quantity" 
    VIEW-AS TOGGLE-BOX
    SIZE 36.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
    item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_job-no AT ROW 8.62 COL 25.8 COLON-ALIGNED WIDGET-ID 2
    fiFromJobNo2 AT ROW 8.62 COL 40 COLON-ALIGNED NO-LABELS WIDGET-ID 6
    end_job-no AT ROW 8.62 COL 60.4 COLON-ALIGNED WIDGET-ID 4
    fiToJobNo2 AT ROW 8.62 COL 74.6 COLON-ALIGNED NO-LABELS WIDGET-ID 8
    btn-process AT ROW 11.95 COL 23
    btn-cancel AT ROW 11.95 COL 54
    rd_methods AT ROW 2.81 COL 26.4 NO-LABELS WIDGET-ID 16
    tb_prod-qty AT ROW 6.19 COL 27.2 WIDGET-ID 20
    rd_run-methods AT ROW 4.33 COL 26.4 NO-LABELS WIDGET-ID 22
    lbl_opt AT ROW 2.70 COL 7.6 COLON-ALIGNED NO-LABELS WIDGET-ID 26
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 1.48 COL 3
    "" VIEW-AS TEXT
    SIZE 2.2 BY .95 AT ROW 1.95 COL 87.8
    BGCOLOR 11 
    RECT-17 AT ROW 1.71 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.6 BY 13.48.


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
        TITLE              = "Job Costing"
        HEIGHT             = 13.71
        WIDTH              = 90.4
        MAX-HEIGHT         = 20.91
        MAX-WIDTH          = 111.2
        VIRTUAL-HEIGHT     = 20.91
        VIRTUAL-WIDTH      = 111.2
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
   FRAME-NAME Custom                                                    */
ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN lbl_opt IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_opt:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

ASSIGN 
    rd_methods:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_run-methods:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prod-qty:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "asi.item"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Recalc Job Costs */
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
ON WINDOW-CLOSE OF C-Win /* Recalc Job Costs */
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
        DEFINE VARIABLE ll-process AS LOG NO-UNDO.
  


        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        ll-process  = NO.
        cLabel = IF rd_methods EQ "rebuild" THEN "Rebuild Job" ELSE "Recalc Job" .
        MESSAGE "Are you sure you want to " + trim(cLabel) +
            " for the selected parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.

        IF ll-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_methods
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_methods C-Win
ON VALUE-CHANGED OF rd_methods IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_run-methods
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_run-methods C-Win
ON VALUE-CHANGED OF rd_run-methods IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pEnableDisableField.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prod-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prod-qty C-Win
ON VALUE-CHANGED OF tb_prod-qty IN FRAME FRAME-A /* Jobs with no production quantity */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.


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

    RUN enable_UI.

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

    {&OPEN-QUERY-FRAME-A}
    GET FIRST FRAME-A.
    DISPLAY begin_job-no fiFromJobNo2 end_job-no fiToJobNo2 rd_methods tb_prod-qty 
        rd_run-methods lbl_opt 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE begin_job-no fiFromJobNo2 end_job-no fiToJobNo2 btn-process btn-cancel 
        rd_methods tb_prod-qty rd_run-methods RECT-17 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableDisableField C-Win 
PROCEDURE pEnableDisableField :      
    
    DO WITH FRAME {&FRAME-NAME}:
        IF rd_run-methods EQ "All" THEN
        DO:
            DISABLE  begin_job-no end_job-no fiFromJobNo2
                fiToJobNo2 .
          
        END.
        ELSE ENABLE begin_job-no end_job-no fiFromJobNo2
                fiToJobNo2 .
    
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    DEFINE VARIABLE dQty            AS DECIMAL   NO-UNDO.    
    DEFINE VARIABLE cOutputFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE iCountTotal     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountChanged   AS INTEGER   NO-UNDO.
   
    DO WITH FRAME {&FRAME-NAME}:
    
        ASSIGN 
            begin_job-no
            end_job-no
            fiFromJobNo2
            fiToJobNo2 
            rd_methods
            rd_run-methods
            tb_prod-qty
            .
                
        SESSION:SET-WAIT-STATE("General").
        
        MAIN-LOOP:        
        FOR EACH job NO-LOCK
            WHERE job.company EQ cocode
            AND job.opened EQ TRUE  /* Per Spec run for opened jobs */
            AND (job.job-no GE begin_job-no OR rd_run-methods EQ "ALL")
            AND (job.job-no LE end_job-no OR rd_run-methods EQ "ALL")
            AND (job.job-no2 GE fiFromJobNo2 OR rd_run-methods EQ "ALL")
            AND (job.job-no2 LE fiToJobNo2 OR rd_run-methods EQ "ALL")
            BREAK BY job.job-no
            BY job.job-no2
            :
            iCountTotal = iCountTotal + 1 .
            IF FIRST-OF(job.job-no2) THEN 
            DO:
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2   
                    AND job-hdr.opened
                    NO-ERROR.
          
                IF NOT AVAILABLE job-hdr THEN 
                    NEXT MAIN-LOOP.
                IF tb_prod-qty THEN
                DO:
                
                    RUN fg/GetProductionQty.p
                        (
                        INPUT job-hdr.company,
                        INPUT job-hdr.job-no,
                        INPUT job-hdr.job-no2,
                        INPUT job-hdr.i-no,
                        INPUT NO,
                        OUTPUT dQty
                        ).

                    /* Per spec, run for jobs that have production qty = 0 */
                    IF dQty NE 0 THEN 
                        NEXT MAIN-LOOP.
                END.
          
                STATUS DEFAULT job.job-no + "-" + STRING(job.job-no2, "99").               
                iCountChanged = iCountChanged + 1.
                IF rd_methods EQ "Rebuild" THEN
                DO:
                    RUN jc/jc-calc.p (RECID(job), YES) NO-ERROR.                    
                END.                  
                ELSE 
                DO:                  
                    RUN jc/jc-calc.p (RECID(job), NO) NO-ERROR.          
                END.
                
            END. /* first of job-no2 */
    
        END. /* Each job, each job-hdr */         
       

        SESSION:SET-WAIT-STATE("").
    END. 
    
    MESSAGE  " Process is Completed." SKIP(2)               
        "Job Processed: " iCountTotal SKIP            
        cLabel ": " iCountChanged SKIP 
        VIEW-AS ALERT-BOX INFORMATION.
    
    APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

