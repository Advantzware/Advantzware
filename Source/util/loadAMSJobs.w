&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\loadAMSJobs.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

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

DEFINE VARIABLE lCheckSimulate  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cJobFileName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutputProcs   AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/* Procedure to prepare and execute API calls */
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

DEFINE TEMP-TABLE ttJob NO-UNDO
    FIELD company   AS CHARACTER
    FIELD jobID     AS CHARACTER
    FIELD jobID2    AS INTEGER
    FIELD location  AS CHARACTER
    FIELD jobStatus AS CHARACTER
    FIELD jobRowID  AS ROWID
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 rd_jstat begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_date end_date btSimulate btExecute 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExecute 
    LABEL "Execute" 
    SIZE 16 BY 1.43.

DEFINE BUTTON btSimulate 
    LABEL "Simulate" 
    SIZE 16 BY 1.43.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no  AS CHARACTER FORMAT "X(9)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no    AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2   AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_jstat     AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
    VIEW-AS FILL-IN 
    SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE rd_jstat      AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "All", "All",
    "Open", "Open"
    SIZE 18 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 83.6 BY 6.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    lbl_jstat AT ROW 2.76 COL 28 COLON-ALIGNED NO-LABELS WIDGET-ID 30
    rd_jstat AT ROW 2.76 COL 43 NO-LABELS WIDGET-ID 32
    begin_job-no AT ROW 4.43 COL 24.6 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 20
    begin_job-no2 AT ROW 4.43 COL 36.6 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 22
    end_job-no AT ROW 4.43 COL 60.8 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 26
    end_job-no2 AT ROW 4.43 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 28
    begin_date AT ROW 6.1 COL 24.6 COLON-ALIGNED WIDGET-ID 18
    end_date AT ROW 6.1 COL 60.8 COLON-ALIGNED HELP
    "Enter Ending Due Date" WIDGET-ID 24
    btSimulate AT ROW 8.95 COL 26.6 WIDGET-ID 42
    btExecute AT ROW 8.95 COL 48.2 WIDGET-ID 40
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 38
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 90.2 BY 10.29
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
        TITLE              = "Load AMS Jobs"
        HEIGHT             = 10.33
        WIDTH              = 90.2
        MAX-HEIGHT         = 10.33
        MAX-WIDTH          = 90.2
        VIRTUAL-HEIGHT     = 10.33
        VIRTUAL-WIDTH      = 90.2
        RESIZE             = NO
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A = "rd_jstat".

ASSIGN 
    rd_jstat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Load AMS Jobs */
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
ON WINDOW-CLOSE OF C-Win /* Load AMS Jobs */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME btExecute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecute C-Win
ON CHOOSE OF btExecute IN FRAME FRAME-A /* Execute */
    DO:   
        lCheckSimulate = YES.
        RUN pRunProcess(YES). 
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME FRAME-A /* Simulate */
    DO:   
        lCheckSimulate = YES.
        RUN pRunProcess(NO).
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
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
    btSimulate:LOAD-IMAGE("Graphics/32x32/Simulate.png").
    btExecute:LOAD-IMAGE("Graphics/32x32/Execute.png").
  
    RUN FileSys_GetTempDirectory(
        OUTPUT cLocation
        ).
  
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
    DISPLAY lbl_jstat rd_jstat begin_job-no begin_job-no2 end_job-no end_job-no2 
        begin_date end_date 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no end_job-no2 
        begin_date end_date btSimulate btExecute 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExecute AS LOGICAL NO-UNDO.    
    
    DEFINE VARIABLE v-stat       AS CHARACTER FORMAT "!" INIT "O".
    DEFINE VARIABLE v-fjob       LIKE job.job-no.
    DEFINE VARIABLE v-tjob       LIKE v-fjob  INIT "zzzzzz".
    DEFINE VARIABLE v-fjob2      LIKE job.job-no2.
    DEFINE VARIABLE v-tjob2      LIKE v-fjob2 INIT 99.
    DEFINE VARIABLE v-fcust      LIKE job-hdr.cust-no INIT "".
    DEFINE VARIABLE v-tcust      LIKE v-fcust INIT "zzzzzzzz".
    DEFINE VARIABLE v-fdate      AS DATE      FORMAT "99/99/9999" INIT 01/01/0001.
    DEFINE VARIABLE v-tdate      LIKE v-fdate INIT 12/31/9999.
    
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckStatus AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr          FOR job-hdr.
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
    
    ASSIGN
        v-stat  = SUBSTR(rd_jstat,1,1)

        v-fjob  = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2)) 
        v-tjob  = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2)) 
        v-fdate = begin_date
        v-tdate = END_date.
    
    IF lCheckSimulate THEN
    DO:
        EMPTY TEMP-TABLE  ttJob. 
          
        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode             
            AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
            TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"999")
            GE v-fjob
            AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
            TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"999")
            LE v-tjob
            AND job-hdr.job-no2 GE int(begin_job-no2)
            AND job-hdr.job-no2 LE int(end_job-no2)
            NO-LOCK,

            FIRST job
            WHERE job.company EQ cocode
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
            AND (v-stat     EQ "A"
            OR (v-stat     EQ "O" AND job.opened))
            USE-INDEX job NO-LOCK:
          
            {custom/statusMsg.i " 'Processing Job#  '  + string(job.job-no) "}
            CREATE ttJob.
            ASSIGN
                ttJob.company   = job.company
                ttJob.jobID     = job.job-no
                ttJob.jobID2    = job.job-no2
                ttJob.location  = job.loc
                ttJob.jobStatus = IF job.opened THEN "Open" ELSE "Close".
            ttJob.jobRowID  = ROWID(job)
                .
           
        END. 
    
        RUN sys/ref/ExcelNameExt.p (INPUT cLocation + "\LoadAMSJob.csv",OUTPUT cJobFileName) .
     
        RUN Output_TempTableToCSV IN hdOutputProcs (
            INPUT TEMP-TABLE ttJob:HANDLE,
            INPUT cJobFileName,
            INPUT TRUE,  /* Export Header */
            INPUT FALSE, /* Auto increment File name */
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
                
        lCheckSimulate = NO .
    
    END.    
    
    IF ipcExecute THEN 
    DO:
        FOR EACH ttJob:
            ASSIGN
                cAPIId       = "SendJobAMS"
                cTriggerID   = "PrintJob"
                cPrimaryID   = ttJob.jobID + "-" + STRING(ttJob.jobID2)
                cDescription = cAPIID + " triggered by " + cTriggerID + " from loadAMSJobs.w for Job: " + cPrimaryID
                .
            
            RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                INPUT  ttJob.company,              /* Company Code (Mandatory) */
                INPUT  ttJob.location,             /* Location Code (Mandatory) */
                INPUT  "SendJobAMS",               /* API ID (Mandatory) */
                INPUT  "",                         /* Client ID (Optional) - Pass empty in case to make request for all clients */
                INPUT  cTriggerID,                 /* Trigger ID (Mandatory) */
                INPUT  "job",                      /* Comma separated list of table names for which data being sent (Mandatory) */
                INPUT  STRING(ttJob.jobRowID),     /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                INPUT  cPrimaryID,                 /* Primary ID for which API is called for (Mandatory) */   
                INPUT  cDescription,               /* Event's description (Optional) */
                OUTPUT lSuccess,                   /* Success/Failure flag */
                OUTPUT cMessage                    /* Status message */
                ). 
            
        END.
    
        EMPTY TEMP-TABLE  ttJob.
        RELEASE job-hdr.
        RELEASE job.
    
        /* Reset context at the end of API calls to clear temp-table 
           data inside OutboundProcs */
        RUN Outbound_ResetContext IN hdOutboundProcs. 
        
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutboundProcs).
        DELETE OBJECT hdOutboundProcs.
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutputProcs).
        DELETE OBJECT hdOutputProcs.
     
    END.    /* ipcExecute */ 

    STATUS DEFAULT "Process Complete" .
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

