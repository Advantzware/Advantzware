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
DEFINE NEW SHARED VARIABLE nufile AS LOG NO-UNDO.   /* for jc-calc.p */
DEFINE NEW SHARED VARIABLE lv-qty AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE fil_id AS RECID NO-UNDO.
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.
DEFINE TEMP-TABLE ttSaveCosts
FIELD job-no LIKE job.job-no
FIELD job-no2 LIKE job.job-no2
FIELD i-no LIKE job-hdr.i-no
FIELD frm LIKE job-hdr.frm
FIELD blank-no LIKE job-hdr.blank-no
FIELD std-fix-cost-before LIKE job.std-fix-cost column-label "Before-Std-Fix-Cost"
FIELD std-lab-cost-before LIKE job.std-lab-cost column-label "Before-Std-lab-Cost"
FIELD std-mat-cost-before LIKE job.std-mat-cost column-label "Before-Std-Mat-Cost"
FIELD std-var-cost-before LIKE job.std-var-cost column-label "Before-Std-Var-Cost"
FIELD std-fix-cost-after LIKE job.std-fix-cost column-label "After-Std-Fix-Cost"
FIELD std-lab-cost-after LIKE job.std-lab-cost column-label "After-Std-lab-Cost"
FIELD std-mat-cost-after LIKE job.std-mat-cost column-label "After-Std-Mat-Cost"
FIELD std-var-cost-after LIKE job.std-var-cost column-label "After-Std-Var-Cost"
INDEX i1 job-no job-no2
.
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE VARIABLE hdOutputProcs AS HANDLE        NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiExportFile fiFromJobNo fiFromJobNo2 ~
fiToJobNo fiToJobNo2 btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fiExportFile fiFromJobNo fiFromJobNo2 ~
fiToJobNo fiToJobNo2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiExportFile AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\jobRecalc.csv" 
     LABEL "Backup File" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromJobNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiToJobNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiToJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiExportFile AT ROW 8.14 COL 29 COLON-ALIGNED WIDGET-ID 10
     fiFromJobNo AT ROW 4.33 COL 29 COLON-ALIGNED WIDGET-ID 2
     fiFromJobNo2 AT ROW 4.33 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiToJobNo AT ROW 5.52 COL 29 COLON-ALIGNED WIDGET-ID 4
     fiToJobNo2 AT ROW 5.52 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     btn-process AT ROW 11.95 COL 23
     btn-cancel AT ROW 11.95 COL 54
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.48 COL 3
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
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
         TITLE              = "Recalc Job Costs"
         HEIGHT             = 13.71
         WIDTH              = 90.4
         MAX-HEIGHT         = 20.91
         MAX-WIDTH          = 111.2
         VIRTUAL-HEIGHT     = 20.91
         VIRTUAL-WIDTH      = 111.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR ll-process AS LOG NO-UNDO.


        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        /* Errors out if file can't be written to */
        OUTPUT TO VALUE(fiExportFile) APPEND.
        PUT UNFORMATTED "".
        OUTPUT CLOSE.
        ll-process  = NO.

        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
            " for the selected parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.

        IF ll-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEF VAR v-mat-list AS CHAR NO-UNDO.


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
  DISPLAY fiExportFile fiFromJobNo fiFromJobNo2 fiToJobNo fiToJobNo2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fiExportFile fiFromJobNo fiFromJobNo2 fiToJobNo fiToJobNo2 btn-process 
         btn-cancel RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    
DEFINE VARIABLE dQty            AS DECIMAL NO-UNDO.    
DEFINE VARIABLE cOutputFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    DO WITH FRAME {&FRAME-NAME}:
    
        ASSIGN 
            fiFromJobNo
            fiToJobNo
            fiFromJobNo2
            fiToJobNo2
            fiExportFile
            .
        cOutputFileName = fiExportFile.
        
        SESSION:SET-WAIT-STATE("General").
        FOR EACH ttSaveCosts:
            DELETE ttSaveCosts.
        END.
        
        FOR EACH job NO-LOCK
            WHERE job.company EQ cocode
            AND job.opened EQ TRUE  /* Per Spec run for opened jobs */
            AND job.job-no GE fiFromJobNo
            AND job.job-no LE fiToJobNo
            AND job.job-no2 GE fiFromJobNo2
            AND job.job-no2 LE fiToJobNo2
            BREAK BY job.job-no
            BY job.job-no2
            :
        
            IF FIRST-OF(job.job-no2) THEN 
            DO:
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2   
                    AND job-hdr.opened
                    NO-ERROR.
          
                IF NOT AVAIL job-hdr THEN 
                    NEXT.
                
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
                    NEXT.
          
                STATUS DEFAULT job.job-no + "-" + STRING(job.job-no2, "99").
                FOR EACH job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2    
                    :         
        
                    CREATE ttSaveCosts.
                    ASSIGN       
                        ttSaveCosts.job-no              = job.job-no
                        ttSaveCosts.job-no2             = job.job-no2
                        ttSaveCosts.i-no                = job-hdr.i-no
                        ttSaveCosts.frm                 = job-hdr.frm
                        ttSaveCosts.blank-no            = job-hdr.blank-no
                        ttSaveCosts.std-fix-cost-before = job-hdr.std-fix-cost
                        ttSaveCosts.std-lab-cost-before = job-hdr.std-lab-cost
                        ttSaveCosts.std-mat-cost-before = job-hdr.std-mat-cost
                        ttSaveCosts.std-var-cost-before = job-hdr.std-var-cost
                        .
                END. 
        
                RUN jc/jc-calc.p (RECID(job), NO) NO-ERROR.
        
                FOR EACH job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2    
                    :    
                    FIND FIRST ttSaveCosts 
                        WHERE ttSaveCosts.job-no = job.job-no
                          AND ttSaveCosts.job-no2 = job.job-no2
                          AND ttSaveCosts.i-no  = job-hdr.i-no
                          AND ttSaveCosts.frm   = job-hdr.frm
                          AND ttSaveCosts.blank-no = job-hdr.blank-no
                          NO-ERROR.
                    IF AVAIL ttSaveCosts THEN 
                        ASSIGN       
                            ttSaveCosts.std-fix-cost-after = job-hdr.std-fix-cost
                            ttSaveCosts.std-lab-cost-after = job-hdr.std-lab-cost
                            ttSaveCosts.std-mat-cost-after = job-hdr.std-mat-cost
                            ttSaveCosts.std-var-cost-after = job-hdr.std-var-cost
                            . 
                END.
            END. /* first of job-no2 */
    
        END. /* Each job, each job-hdr */
        
        RUN Output_TempTableToCSV IN hdOutputProcs ( 
            INPUT TEMP-TABLE ttSaveCosts:HANDLE,
            INPUT cOutputFileName,
            INPUT TRUE,
            INPUT TRUE /* Auto increment File name */,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

        SESSION:SET-WAIT-STATE("").
    END. 
    MESSAGE TRIM(c-win:TITLE) + " Process is Completed..." VIEW-AS ALERT-BOX.
    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE OBJECT hdOutputProcs.
    APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

