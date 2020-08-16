&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: capacityPage.w

  Description: Capacity Schedule Web Page Generation

  Input Parameters: Type: "Est" or "Job" -- Rowid: Est or Job

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.20.2017
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cocode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAccess   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lContinue AS LOGICAL   NO-UNDO.
 
{schedule/scopDir.i}
{{&includes}/ttblDowntime.i NEW}

DEFINE TEMP-TABLE ttJob NO-UNDO LIKE job-mch 
  FIELD rRowID AS ROWID 
  FIELD d-seq  AS INTEGER
  FIELD m-seq  AS INTEGER
  FIELD m-dscr AS CHARACTER
    INDEX ttJob IS PRIMARY frm blank-no d-seq m-seq pass m-code
    .
DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD m-code        AS CHARACTER 
  FIELD job           AS CHARACTER
  FIELD frm           AS INTEGER 
  FIELD blank-no      AS INTEGER 
  FIELD pass          AS INTEGER 
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime   AS DECIMAL
  FIELD startDate     AS DATE
  FIELD startTime     AS INTEGER
  FIELD endDate       AS DATE
  FIELD endTime       AS INTEGER
  FIELD newJob        AS LOGICAL 
    INDEX dataTimeIdx IS PRIMARY startDateTime endDateTime
    INDEX startDate startDate
    INDEX endDate endDate
    .
DEFINE BUFFER bTtblJob FOR ttblJob.
DEFINE BUFFER bJobMch  FOR job-mch.

{{&includes}/htmlDefs.i m-code}

DEFINE TEMP-TABLE ttMachine NO-UNDO
  FIELD m-code AS CHARACTER FORMAT "x(10)" LABEL "Machine"
  FIELD m-dscr AS CHARACTER FORMAT "x(24)" LABEL "Description"
  FIELD d-seq  LIKE mach.d-seq
  FIELD m-seq  LIKE mach.m-seq
    INDEX ttMachine IS PRIMARY m-code
    .
DEFINE TEMP-TABLE ttFolder NO-UNDO
  FIELD folderName AS CHARACTER
  FIELD searched   AS LOGICAL
  .
DEFINE TEMP-TABLE ttResource NO-UNDO
  FIELD resource AS CHARACTER
  .
SESSION:SET-WAIT-STATE ("").

/* check for valid license */
RUN util/CheckModule.p ("ASI", "sbHTML", YES, OUTPUT lAccess).
IF NOT lAccess THEN RETURN.

/* get nk1 setting */
cocode = ipcCompany.
DO TRANSACTION:
    {sys/inc/capacityPage.i}  
END.
IF cCapacityPage EQ "No" THEN DO:
    MESSAGE 
        "Schedule Capacity Page Generatiion is not Enabled." SKIP 
        "See System Administrator for Assistance."
    VIEW-AS ALERT-BOX.
    RETURN.
END. /* if no */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME ttJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJob ttMachine

/* Definitions for BROWSE ttJob                                         */
&Scoped-define FIELDS-IN-QUERY-ttJob ttJob.m-code ttJob.m-dscr ttJob.frm ttJob.blank-no ttJob.pass ttJob.mr-hr ttJob.run-hr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttJob ttJob.frm ttJob.blank-no ttJob.pass ttJob.mr-hr ttJob.run-hr   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttJob ttJob
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttJob ttJob
&Scoped-define SELF-NAME ttJob
&Scoped-define QUERY-STRING-ttJob FOR EACH ttJob
&Scoped-define OPEN-QUERY-ttJob OPEN QUERY {&SELF-NAME} FOR EACH ttJob.
&Scoped-define TABLES-IN-QUERY-ttJob ttJob
&Scoped-define FIRST-TABLE-IN-QUERY-ttJob ttJob

/* Definitions for BROWSE ttMachine                                     */
&Scoped-define FIELDS-IN-QUERY-ttMachine ttMachine.m-code ttMachine.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttMachine   
&Scoped-define SELF-NAME ttMachine
&Scoped-define QUERY-STRING-ttMachine FOR EACH ttMachine
&Scoped-define OPEN-QUERY-ttMachine OPEN QUERY {&SELF-NAME} FOR EACH ttMachine.
&Scoped-define TABLES-IN-QUERY-ttMachine ttMachine
&Scoped-define FIRST-TABLE-IN-QUERY-ttMachine ttMachine

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-ttJob}~
    ~{&OPEN-QUERY-ttMachine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClear ttMachine ttJob btnExit btnOK ~
btnRemove btnReset btnSort 
&Scoped-Define DISPLAYED-OBJECTS baseOnText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/error.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear" 
     SIZE 8 BY 1.91 TOOLTIP "Clear".

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btnOK 
     IMAGE-UP FILE "Graphics/32x32/html_tag.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "Generate Page".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/32x32/delete.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove" 
     SIZE 8 BY 1.91 TOOLTIP "Remove".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/nav_refresh.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnSort 
     IMAGE-UP FILE "Graphics/32x32/sort_up_down.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort" 
     SIZE 8 BY 1.91 TOOLTIP "Sort".

DEFINE VARIABLE baseOnText AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1
     BGCOLOR 14  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttJob FOR 
      ttJob SCROLLING.

DEFINE QUERY ttMachine FOR 
      ttMachine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttJob Dialog-Frame _FREEFORM
  QUERY ttJob DISPLAY
      ttJob.m-code LABEL "Machine" FORMAT "x(10)"
ttJob.m-dscr LABEL "Description" FORMAT "x(30)"
ttJob.frm
ttJob.blank-no LABEL "Blank"
ttJob.pass
ttJob.mr-hr LABEL "MR Hour"
ttJob.run-hr LABEL "Run Hour"
ENABLE
ttJob.frm
ttJob.blank-no
ttJob.pass
ttJob.mr-hr
ttJob.run-hr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81 BY 32.14
         FGCOLOR 1  ROW-HEIGHT-CHARS .67.

DEFINE BROWSE ttMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttMachine Dialog-Frame _FREEFORM
  QUERY ttMachine DISPLAY
      ttMachine.m-code
ttMachine.m-dscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 33.1
         FGCOLOR 1 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnClear AT ROW 15.52 COL 129 WIDGET-ID 22
     baseOnText AT ROW 1 COL 47 NO-LABEL WIDGET-ID 10
     ttMachine AT ROW 1.24 COL 2 WIDGET-ID 300
     ttJob AT ROW 2.19 COL 47 WIDGET-ID 200
     btnExit AT ROW 32.43 COL 129 WIDGET-ID 6
     btnOK AT ROW 2.91 COL 129 WIDGET-ID 4
     btnRemove AT ROW 9.1 COL 129 WIDGET-ID 16
     btnReset AT ROW 12.19 COL 129 WIDGET-ID 18
     btnSort AT ROW 6 COL 129 WIDGET-ID 20
     SPACE(0.00) SKIP(26.43)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Capacity Schedule Page Generation" WIDGET-ID 100.

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB ttMachine baseOnText Dialog-Frame */
/* BROWSE-TAB ttJob ttMachine Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN baseOnText IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       baseOnText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttJob
/* Query rebuild information for BROWSE ttJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttJob.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttJob */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttMachine
/* Query rebuild information for BROWSE ttMachine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttMachine.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttMachine */
&ANALYZE-RESUME

/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Capacity Schedule Page Generation */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear Dialog-Frame
ON CHOOSE OF btnClear IN FRAME Dialog-Frame /* Clear */
DO:
    EMPTY TEMP-TABLE ttJob.
    {&OPEN-QUERY-ttJob}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    SESSION:SET-WAIT-STATE ("General").
    RUN pScheduleJob (iprRowID).
/*    RUN pHTMLPageHorizontal.*/
    RUN pHTMLPageVertical.
    SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove Dialog-Frame
ON CHOOSE OF btnRemove IN FRAME Dialog-Frame /* Remove */
DO:
    APPLY "DEFAULT-ACTION":U TO BROWSE ttJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    RUN pBuildTTJob (ipcType, ipcCompany, iprRowID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSort Dialog-Frame
ON CHOOSE OF btnSort IN FRAME Dialog-Frame /* Sort */
DO:
    {&OPEN-QUERY-ttJob}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttJob
&Scoped-define SELF-NAME ttJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttJob Dialog-Frame
ON DEFAULT-ACTION OF ttJob IN FRAME Dialog-Frame
DO:
    IF AVAILABLE ttJob THEN DO:
        DELETE ttJob.
        {&OPEN-QUERY-ttJob}
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttMachine
&Scoped-define SELF-NAME ttMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttMachine Dialog-Frame
ON DEFAULT-ACTION OF ttMachine IN FRAME Dialog-Frame
DO:
    RUN pAddMachine.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttJob
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 

/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pLoadDowntime.
  RUN pBuildTTMachine.
  RUN pBuildTTJob (ipcType, ipcCompany, iprRowID).
  RUN pRemoveUnusedDowntime.
  RUN enable_UI.
  DISPLAY baseOnText WITH FRAME {&FRAME-NAME}.
  IF cErrorMsg NE "" THEN
  MESSAGE 
      cErrorMsg
  VIEW-AS ALERT-BOX WARNING.
  /* fatal error, bail */
  IF NOT lContinue THEN RETURN.
  IF cCapacityPage EQ "Yes" THEN DO: 
      APPLY "CHOOSE":U TO btnOK.
      RETURN.
  END. /* if yes */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcEnd Dialog-Frame 
PROCEDURE calcEnd :
/*------------------------------------------------------------------------------
  Purpose:     calculate ending date/time based on start date/time & mr/run hrs
  Parameters:  date, time, mr hr, run hr
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipDate AS DATE    NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMR   AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRun  AS DECIMAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opDate AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE totalTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE days      AS INTEGER NO-UNDO.

  IF ipTime EQ ? THEN ipTime = 0.
  IF ipMR EQ ? THEN ipMR     = 0.
  IF ipRun EQ ? THEN ipRun   = 0.
  ASSIGN
    totalTime = ipTime + ipMR * 3600 + ipRun * 3600
    days      = TRUNCATE(totalTime / 86400,0)
    opDate    = ipDate + days
    opTime    = totalTime - days * 86400
    .
  IF opDate EQ ? THEN opDate = ipDate.
  IF opTime EQ ? THEN opTime = ipTime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY baseOnText 
      WITH FRAME Dialog-Frame.
  ENABLE btnClear ttMachine ttJob btnExit btnOK btnRemove btnReset btnSort 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddMachine Dialog-Frame 
PROCEDURE pAddMachine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
    
    CREATE ttJob.
    ASSIGN
        ttJob.m-code   = ttMachine.m-code
        ttJob.m-dscr   = ttMachine.m-dscr
        ttJob.d-seq    = ttMachine.d-seq
        ttJob.m-seq    = ttMachine.m-seq
        rRowID         = ROWID(ttJob)
        .
    {&OPEN-QUERY-ttJob}
    QUERY ttJob:REPOSITION-TO-ROWID(rRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTTJob Dialog-Frame 
PROCEDURE pBuildTTJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE cMachine    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dRunHours   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iDie        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dOnForm     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOnSheet    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iOut        AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE iNumUp      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lPrintedLit AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dSheetLen   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lUnitize    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dPalletQty  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dParts      AS DECIMAL   NO-UNDO.

    DEFINE BUFFER beb FOR eb.

    lContinue = YES.
    EMPTY TEMP-TABLE ttJob.    
    /* order not yet implemented */
    IF ipcType EQ "Order" THEN DO:
        FIND FIRST oe-ord NO-LOCK 
             WHERE ROWID(oe-ord) EQ iprRowID
             NO-ERROR.
        IF NOT AVAILABLE oe-ord OR oe-ord.job-no EQ "" THEN
        ASSIGN
            cErrorMsg = "No Job Exists for this Order."
            lContinue = NO
            .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ oe-ord.company
               AND job.job     EQ oe-ord.j-no
             NO-ERROR.
        IF NOT AVAILABLE job THEN
        ASSIGN
            cErrorMsg = cErrorMsg + "Unable to locate Job for this Order." + CHR(10)
            lContinue = NO
            .
        ASSIGN
            iprRowID = ROWID(job)
            ipcType  = "Job"
            .
    END. /* type eq order */

    IF lContinue AND ipcType EQ "Job" AND iprRowID NE ? THEN DO: 
        FIND job NO-LOCK WHERE ROWID(job) EQ iprRowID NO-ERROR.
        IF NOT AVAILABLE job THEN
        ASSIGN
            cErrorMsg = cErrorMsg + "Job Record Missing." + CHR(10)
            lContinue = NO
            .
        IF lContinue THEN DO:
            FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN
            ASSIGN
                cErrorMsg = cErrorMsg + "Unable to locate Job Header." + CHR(10)
                lContinue = NO
                .
            ELSE IF NOT job-hdr.opened THEN
                 cErrorMsg = cErrorMsg + "Job is Closed." + CHR(10).
            baseOnText = "Based on Job #" + TRIM(job.job-no + "-" + STRING(job.job-no2)).
            FOR EACH job-mch NO-LOCK
                WHERE job-mch.company      EQ job.company
                  AND job-mch.job          EQ job.job
                  AND job-mch.run-complete EQ NO,
                FIRST mach NO-LOCK
                WHERE mach.company EQ job-mch.company
                  AND mach.loc     EQ job.loc
                  AND mach.m-code  EQ job-mch.m-code
                :
                IF job-mch.start-date-su NE ? THEN
                cErrorMsg = cErrorMsg + job-mch.m-code + " Routing already Scheduled." + CHR(10).
                IF job-mch.run-complete THEN
                cErrorMsg = cErrorMsg + job-mch.m-code + " Routing already Run Completed." + CHR(10).
                cMachine = IF mach.sch-m-code NE "" THEN mach.sch-m-code ELSE mach.m-code.
                CREATE ttJob.
                BUFFER-COPY job-mch TO ttJob
                    ASSIGN 
                      ttJob.rRowID = ROWID(job-mch)
                      ttJob.d-seq  = mach.d-seq
                      ttJob.m-seq  = mach.m-seq
                      ttJob.m-code = cMachine
                      ttJob.m-dscr = mach.m-dscr
                      . 
            END. /* each job-mch */
        END. /* if lcontinue */
    END. /* job and iprrowid ne ? */
    
    IF ipcType EQ "Est" AND iprRowID NE ? THEN DO: 
        FIND est NO-LOCK WHERE ROWID(est) EQ iprRowID NO-ERROR.
        IF NOT AVAILABLE est THEN
        ASSIGN
            cErrorMsg = "Estimate Record Missing."
            lContinue = NO
            .
        baseOnText = "Based on Est #" + TRIM(est.est-no).
        FIND FIRST eb NO-LOCK 
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               AND eb.form-no EQ 1
             NO-ERROR.
        IF AVAILABLE eb THEN DO:
            lPrintedLit = CAN-FIND(FIRST prodl
                                   WHERE prodl.company EQ est.company
                                     AND prodl.prolin  EQ 'Printed'
                                     AND prodl.procat  EQ eb.procat).        
            FOR EACH est-op NO-LOCK
                WHERE est-op.company EQ est.company
                  AND est-op.est-no  EQ est.est-no
                  AND est-op.line    LT 500,
                FIRST ef NO-LOCK
                WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est-op.est-no
                  AND ef.form-no EQ est-op.s-num
                   BY est-op.s-num
                   BY est-op.b-num
                   BY est-op.d-seq
                   BY est-op.op-pass
                :
                cMachine  = est-op.m-code.
                FIND FIRST mach NO-LOCK
                     WHERE mach.company EQ est.company
                       AND mach.loc     EQ est.loc
                       AND mach.m-code  EQ cMachine
                     NO-ERROR.
                IF NOT AVAILABLE mach THEN NEXT.
                IF mach.sch-m-code NE "" AND mach.sch-m-code NE mach.m-code THEN DO:
                    cMachine = mach.sch-m-code. 
                    FIND FIRST mach NO-LOCK
                         WHERE mach.company EQ est.company
                           AND mach.loc     EQ est.loc
                           AND mach.m-code  EQ cMachine
                         NO-ERROR.
                    IF NOT AVAILABLE mach THEN NEXT.
                END. /* if sch-m-code ne m-code */
                IF est.est-type EQ 6 THEN DO:
                    dParts = 0.
                    FOR EACH beb FIELDS(quantityPerSet) NO-LOCK
                        WHERE beb.company EQ est.company
                          AND beb.est-no  EQ est.est-no
                          AND beb.form-no NE 0
                        :
                        dParts = dParts
                               + (IF beb.quantityPerSet LT 0 THEN (-1 / beb.quantityPerSet)
                                  ELSE beb.quantityPerSet)
                               .
                    END. /* each beb */
                END. /* if est-type eq 6 */
                ASSIGN
                    dSheetLen = IF est-op.dept EQ "LM" THEN ef.nsh-len ELSE ef.gsh-len
                    dRunHours = 0
                    .
                CASE est.est-type:
                    WHEN 1 OR WHEN 3 THEN DO:
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).                    
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm * iOut) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm * iOut) - est-op.op-waste.
                                 ELSE IF NOT lPrintedLit OR iOut EQ 1 THEN
                                 dRunHours = (est-op.num-sh * iNumUp
                                           * (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out)
                                           * (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l))
                                           - est-op.op-waste
                                           .
                                      ELSE
                                      dRunHours = (est-op.num-sh * iOut) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 1 or 3 */
                    WHEN 2 OR WHEN 4 THEN DO:
                        RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT iNumUp).
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).            
                        IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
                            FIND FIRST ef-nsh OF ef NO-LOCK 
                                 WHERE ef-nsh.pass-no EQ est-op.op-pass
                                   AND ef-nsh.dept    EQ est-op.dept
                                 NO-ERROR.
                            IF AVAILABLE ef-nsh THEN DO:
                                RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT iDie).
                                dOnForm = dOnForm * (est-op.n-out + INT(iDie GT 0)).
                            END. /* avail ef-nsh */
                        END. /* if dc */            
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm * iOut) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm * iOut) - est-op.op-waste.
                                 ELSE IF NOT lPrintedLit OR iOut EQ 1 THEN
                                      dRunHours = (est-op.num-sh * iNumUp * dOnForm) - est-op.op-waste.
                                      ELSE
                                      dRunHours = (est-op.num-sh * iOut * dOnForm) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 2 or 4 */
                    WHEN 5 THEN DO:
                        lUnitize = NO.
                        FOR EACH mstd NO-LOCK
                            WHERE mstd.company EQ mach.company
                              AND mstd.loc     EQ mach.loc
                              AND mstd.m-code  EQ mach.m-code
                            BREAK BY mstd.style DESCENDING
                            :
                            IF LAST(mstd.style) OR mstd.style EQ eb.style THEN DO:
                                lUnitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
                                LEAVE.
                            END. /* last(mstd.style) */
                        END. /* each mstd */
                        IF est-op.op-speed GT 0 THEN DO:
                            IF lUnitize THEN 
                            dRunHours = dPalletQty. /* not set, calc in cec/pr4-cas.p as p-qty */
                            ELSE IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                                 dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                           * (dSheetLen / 12)
                                           .
                                ELSE IF mach.p-type EQ "A" THEN
                                     dRunHours = est.est-qty[1].
                                    ELSE IF est-op.op-sb THEN
                                         dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                         ELSE 
                                         dRunHours = (est-op.num-sh * dOnSheet) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 5 */
                    WHEN 6 THEN DO:
                        lUnitize = NO.
                        FOR EACH mstd NO-LOCK
                            WHERE mstd.company EQ mach.company
                              AND mstd.loc     EQ mach.loc
                              AND mstd.m-code  EQ mach.m-code
                            BREAK BY mstd.style DESCENDING
                            :
                            IF LAST(mstd.style) OR mstd.style EQ eb.style THEN DO:
                                lUnitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
                                LEAVE.
                            END. /* last(mstd.style) */
                        END. /* each mstd */
                        IF lUnitize THEN DO:
                        END. /* if lunitize */
                        ELSE IF mach.p-type EQ "P" THEN
                             dRunHours = (est-op.num-sh - est-op.op-waste) * dParts.
                            ELSE IF mach.therm AND (mach.p-type NE "A" OR est-op.dept EQ "LM") THEN
                                 dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                           * (dSheetLen / 12)
                                           .
                                ELSE IF mach.p-type EQ "A" THEN
                                     dRunHours = est.est-qty[1].
                                    ELSE IF est-op.op-sb THEN
                                         dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                         ELSE 
                                         dRunHours = (est-op.num-sh * dOnSheet) - est-op.op-waste.
                    END. /* 6 */
                    WHEN 8 THEN DO:
                        RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT iNumUp).
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).            
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                 ELSE 
                                 dRunHours = (est-op.num-sh * iNumUp * dOnForm) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 8 */
                END CASE.
                dRunHours = dRunHours / est-op.op-speed.
                IF est-op.n_out_div GT 0 THEN 
                dRunHours = dRunHours / est-op.n_out_div.
                IF dRunHours LT 0 THEN 
                dRunHours = 0.
                CREATE ttJob.
                ASSIGN 
                    ttJob.rRowID   = ROWID(est-op)
                    ttJob.company  = ipcCompany
                    ttJob.m-code   = cMachine
                    ttJob.m-dscr   = mach.m-dscr
                    ttJob.frm      = est-op.s-num
                    ttJob.blank-no = IF (mach.p-type  EQ "B" OR
                                        (est.est-type EQ  3 AND
                                         est-op.dept  EQ "PR")) THEN est-op.b-num
                                     ELSE 0
                    ttJob.pass     = est-op.op-pass
                    ttJob.d-seq    = mach.d-seq
                    ttJob.m-seq    = mach.m-seq
                    ttJob.mr-hr    = est-op.op-mr
                    ttJob.run-hr   = dRunHours
                    . 
            END. /* each est-op */
        END. /* if avail eb */
    END. /* est and iprrowid ne ? */
    
    {&OPEN-QUERY-ttJob}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTTMachine Dialog-Frame 
PROCEDURE pBuildTTMachine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH mach NO-LOCK
        WHERE mach.company EQ ipcCompany
        BREAK BY mach.sch-m-code
        :
        IF FIRST-OF(mach.sch-m-code) THEN DO:
            CREATE ttMachine.
            ASSIGN
                ttMachine.m-code = mach.m-code
                ttMachine.m-dscr = mach.m-dscr
                ttMachine.d-seq  = mach.d-seq
                ttMachine.m-seq  = mach.m-seq
                .
        END. /* first-of */
    END. /* each mach */
    {&OPEN-QUERY-ttMachine}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTtblDowntime Dialog-Frame
PROCEDURE pCreateTtblDowntime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST ttblDowntime
                WHERE ttblDowntime.dayID     EQ tempDowntime.dayID
                  AND ttblDowntime.resource  EQ tempDowntime.resource
                  AND ttblDowntime.startDate EQ tempDowntime.startDate
                  AND ttblDowntime.startTime EQ tempDowntime.startTime
                  AND ttblDowntime.endTime   EQ tempDowntime.endTime) THEN
    RETURN.
    CREATE ttblDowntime.
    BUFFER-COPY tempDowntime TO ttblDowntime.
    ttblDowntime.startDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.startTime).
    ttblDowntime.endDateTime   = numericDateTime(ttblDowntime.startDate,ttblDowntime.endTime).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPageHorizontal Dialog-Frame 
PROCEDURE pHTMLPageHorizontal :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".    
    DEFINE VARIABLE lAltLine    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBGColor    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE iJobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cType1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPercentage AS DECIMAL   NO-UNDO.
    
    FIND FIRST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    dtStartDate = ttblJob.startDate.
    FIND LAST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate.
    dtEndDate = ttblJob.endDate + 1.
    OUTPUT TO "c:\tmp\sbHTML.htm".
    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '<title>Schedule ' baseOnText '</title>' SKIP
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}"><b>Schedule ' baseOnText '</b> (generated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        '  <img src="' SEARCH("Graphics/32x32/asiicon.png")
        '" align="middle">~&nbsp;<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<font face="{&fontFace}"><font color="#FF0000"><b>Projected Completion: </font>'
        ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am")
        '</font></b>' SKIP 
        '  <table align="right" cellspacing="2" cellpadding="8">' SKIP
        '    <tr>' SKIP 
        '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
        '      <td bgcolor="#00CCFF"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
        '      <td bgcolor="#C0BEBE"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
        '      <td bgcolor="#AAD5B9"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
        '      <td bgcolor="#F1FE98"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
        '    </tr>' SKIP  
        '  </table>' SKIP 
        '  <table border="1" cellspacing="0" cellpadding="5" width="100%">' SKIP
        '    <tr>' SKIP
        '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}"><b>'
        'Operation</b></font></td>' SKIP
        .
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                .
        END. /* do dtdate */
    PUT UNFORMATTED '    </tr>' SKIP.
    
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        PUT UNFORMATTED
            '    <tr>' SKIP
            '      <td' cBGColor ' align="left" nowrap WIDTH="250"><font face="{&fontFace}">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" align="left">~&nbsp~&nbsp~&nbsp~&nbsp<b>'
            ttblJob.m-code '</b> (f:<b>' ttblJob.frm
            '</b> b:<b>' ttblJob.blank-no
            '</b> p:<b>' ttblJob.pass ')</b><br>'
            ttblJob.startDate ' - ' STRING(ttblJob.startTime,"hh:mm:ss am") '<br>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am") '</font></td>' SKIP
            .
        DO dtDate = dtStartDate TO dtEndDate:
            EMPTY TEMP-TABLE ttTime.
            iJobs = 0.
            FOR EACH bTtblJob
                WHERE bTtblJob.m-code     EQ ttblJob.m-code
                  AND (bTtblJob.startDate EQ dtDate
                   OR (bTtblJob.startDate LT dtDate
                  AND  bTtbljob.endDate   GT dtDate)
                   OR  bTtblJob.endDate   EQ dtDate)
                BY bTtblJob.startDateTime
                :
                iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0.
                fTimeSlice ("",dtDate,iStartTime,"Job","Start",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iEndTime = IF bTtblJob.endDate EQ dtDate THEN bTtblJob.endTime ELSE 86400.
                fTimeSlice ("",dtDate,iEndTime,"Job","End",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                iJobs = iJobs + 1.
            END. /* each bttbljob */
            FOR EACH ttblDowntime
                WHERE  ttblDowntime.dayID     EQ WEEKDAY(dtDate)
                  AND (ttblDowntime.resource  EQ "<Calendar>"
                   OR  ttblDowntime.resource  EQ ttblJob.m-code)
                  AND (ttblDowntime.startDate EQ dtDate
                   OR  ttblDowntime.startDate EQ ?)
                :
                fTimeSlice ("",dtDate,ttblDowntime.startTime,"DT","Start",NO).
                fTimeSlice ("",dtDate,ttblDowntime.endTime,"DT","End",NO).
            END. /* each ttbldowntime */
            fTimeSlice ("",dtDate,0,"Avail","Start",NO).
            fTimeSlice ("",dtDate,86400,"Avail","End",NO).
            PUT UNFORMATTED
                '      <td' cBGColor ' align="center" nowrap><font face="{&fontFace}">' SKIP
                '        <table border="1" cellspacing="0" cellpadding="8" width="100%">' SKIP
                '          <tr>' SKIP  
                .
            FOR EACH ttTime
                BY ttTime.timeSlice
                :
                IF ttTime.timeSlice EQ 0 THEN DO:
                    ASSIGN
                        iTime  = 0
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2                        
                        .
                    NEXT.
                END. /* timeslice eq 0 */
                IF ttTime.timeType1 EQ "DT"   AND 
                   ttTime.timeType1 NE cType1 AND
                   ttTime.timeType2 NE cType2 THEN
                cType1 = "Avail".
                ELSE IF ttTime.timeType1 EQ "DT"    AND 
                        ttTime.timeType2 EQ "Start" AND
                        cType2           EQ "End"   THEN
                cType1 = "Avail".
                ELSE IF ttTime.timeType2 NE "Start" OR cType2 NE "Start" THEN
                cType1 = ttTime.timeType1.
                dPercentage = ROUND((ttTime.timeSlice - iTime) / 86400 * 100,0).
                IF dPercentage GT 0 THEN
                PUT UNFORMATTED
                    '            <td bgcolor="#'
                    (IF ttTime.newJob AND ttTime.timeType2 EQ "End" THEN "00CCFF" ELSE
                     IF cType1 EQ "Avail" THEN "F1FE98" ELSE
                     IF cType1 EQ "Job"   THEN "AAD5B9" ELSE "C0BEBE")
                    '" align="center" width="' dPercentage '%" nowrap>~&nbsp;'
                    '</td>' SKIP
                    .
                ASSIGN
                    iTime  = ttTime.timeSlice
                    cType1 = ttTime.timeType1
                    cType2 = ttTime.timeType2
                    .
            END. /* each tttime */
            PUT UNFORMATTED
                '          </tr>' SKIP 
                '        </table>' SKIP 
                '      </font></td>' SKIP
                .
        END. /* do dtdate */
        PUT UNFORMATTED
            '    </tr>' SKIP
            .
        ASSIGN
            lAltLine = NOT lAltLine
            cBGColor = IF lAltLine THEN ' bgcolor="EEDFD2"' ELSE ''
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '  </table>' SKIP
        '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '</html>' SKIP
        .
    OUTPUT CLOSE.
    OS-COMMAND NO-WAIT START "c:\tmp\sbHTML.htm".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPageVertical Dialog-Frame
PROCEDURE pHTMLPageVertical:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    &Scoped-define fontFace Comic Sans MS
    &Scoped-define fontFace Tahoma
    
    DEFINE VARIABLE cBGColor    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDays       AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
    DEFINE VARIABLE cHTMLPage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKey        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachines   AS CHARACTER NO-UNDO EXTENT 200.
    DEFINE VARIABLE cPageTitle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPercentage AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE      NO-UNDO.
    DEFINE VARIABLE iDays       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lAltLine    AS LOGICAL   NO-UNDO.
    
    FIND FIRST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate
         NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    dtStartDate = TODAY.
    FIND LAST ttblJob
         WHERE ttblJob.newJob EQ YES 
         USE-INDEX startDate.
    ASSIGN
        dtEndDate = ttblJob.endDate + 1
        iDays     = dtEndDate - dtStartDate + 1
        cMachines = ""
        .
    EMPTY TEMP-TABLE ttTime.
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        ASSIGN
            idx            = idx + 1
            cMachines[idx] = ttblJob.m-code + ","
                           + STRING(ttblJob.frm) + ","
                           + STRING(ttblJob.blank-no) + ","
                           + STRING(ttblJob.pass)
                           .
        DO dtDate = dtStartDate TO dtEndDate:
            FOR EACH ttblDowntime
                WHERE  ttblDowntime.dayID     EQ WEEKDAY(dtDate)
                  AND (ttblDowntime.resource  EQ "<Calendar>"
                   OR  ttblDowntime.resource  EQ ttblJob.m-code)
                  AND (ttblDowntime.startDate EQ dtDate
                   OR  ttblDowntime.startDate EQ ?)
                :
                fTimeSlice (cMachines[idx],dtDate,ttblDowntime.startTime,"DT","Start",NO).
                fTimeSlice (cMachines[idx],dtDate,ttblDowntime.endTime,  "DT","End",  NO).
            END. /* each ttbldowntime */
            iJobs = 0.
            FOR EACH bTtblJob
                WHERE bTtblJob.m-code     EQ ttblJob.m-code
                  AND (bTtblJob.startDate EQ dtDate
                   OR (bTtblJob.startDate LT dtDate
                  AND  bTtbljob.endDate   GT dtDate)
                   OR  bTtblJob.endDate   EQ dtDate)
                BY bTtblJob.startDateTime
                :
                ASSIGN
                    iStartTime = IF bTtblJob.startDate EQ dtDate THEN bTtblJob.startTime ELSE 0
                    iEndTime   = IF bTtblJob.endDate   EQ dtDate THEN bTtblJob.endTime   ELSE 86400
                    iJobs      = iJobs + 1
                    .
                fTimeSlice (cMachines[idx],dtDate,iStartTime,"Job","Start",ROWID(bTtblJob) EQ ROWID(ttblJob)).
                fTimeSlice (cMachines[idx],dtDate,iEndTime,  "Job","End",  ROWID(bTtblJob) EQ ROWID(ttblJob)).                
            END. /* each bttbljob */
            fTimeSlice (cMachines[idx],dtDate,0,    "Avail","Start",NO).
            fTimeSlice (cMachines[idx],dtDate,86400,"Avail","End",  NO).
        END. /* do dtdate */
    END. /* each ttbljob */
    
    DO jdx = 1 TO 3:
        CASE jdx:
            WHEN 1 THEN
            cPageTitle = " by Time".
            WHEN 2 THEN
            cPageTitle = " by Percentage".
            WHEN 3 THEN
            cPageTitle = " by Time / Percentage".
        END CASE.
        cHTMLPage = "c:\tmp\sbHTMLProjected" + STRING(jdx) + ".htm".
        OUTPUT TO VALUE(cHTMLPage).
        PUT UNFORMATTED
            '<html>' SKIP
            '<head>' SKIP
            '<title>Schedule ' baseOnText '</title>' SKIP
            '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
            '</head>' SKIP
            '<a name="Top"></a>' SKIP
            '<form>' SKIP
            '<fieldset>' SKIP
            '  <legend><font face="{&fontFace}"><b>Schedule ' baseOnText cPageTitle '</b> (generated '
            STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
            '~&nbsp;</legend>' SKIP
            '  <img src="' SEARCH("Graphics/32x32/asiicon.png") '" align="middle">~&nbsp;'
            '<b><a href="http://www.advantzware.com" target="_blank">'
            '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
            '~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;<font face="{&fontFace}"><font color="#FF0000"><b>Projected Completion: </font>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am")
            '</font></b>' SKIP 
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#85FEFE"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '  <table border="1" cellspacing="0" cellpadding="0" width="100%" height="80%" style="border-color: white">' SKIP
            .
        RUN pOutputResources.
        DO dtDate = dtStartDate TO dtEndDate:
            PUT UNFORMATTED
                '    <tr style="height: ' INTEGER(90 / iDays) '%;">' SKIP
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                .
            DO idx = 1 TO EXTENT(cMachines):
                IF cMachines[idx] EQ "" THEN LEAVE.
                PUT UNFORMATTED
                    '      <td style="padding: 5px">' SKIP
                    '        <table border="1" cellspacing="0" cellpadding="0" align="center" width="70%" height="140px">' SKIP
                    .
                FOR EACH ttTime
                    WHERE ttTime.timeKey  EQ cMachines[idx]
                      AND ttTime.timeDate EQ dtDate
                       BY ttTime.timeSlice DESCENDING
                       BY ttTime.timeType2 DESCENDING
                    :
                    IF ttTime.timeSlice EQ 86400 THEN DO:
                        ASSIGN
                            iTime  = 86400
                            cType1 = ttTime.timeType1
                            cType2 = ttTime.timeType2                        
                            .
                        NEXT.
                    END. /* timeslice eq 86400 */
    
                    IF ttTime.timeType1 EQ "DT"   AND 
                       ttTime.timeType1 NE cType1 AND
                       ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 EQ "DT"    AND
                            ttTime.timeType2 EQ "End"   AND
                            cType2           EQ "Start" THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType1 NE cType1 AND
                            ttTime.timeType2 NE cType2 THEN
                    cType1 = "Avail".
                    ELSE IF ttTime.timeType2 NE "End" OR cType2 NE "End" THEN
                    cType1 = ttTime.timeType1.
    
                    dPercentage = ROUND((iTime - ttTime.timeSlice) / 86400 * 100,2).
                    IF dPercentage GT 0 THEN DO:
                        PUT UNFORMATTED
                            '          <tr style="height: ' dPercentage '%;">' SKIP
                            '            <td bgcolor="#'
                            (IF ttTime.newJob AND ttTime.timeType2 EQ "Start" THEN "85FEFE" ELSE
                             IF cType1 EQ "Avail" THEN "97F3A0" ELSE
                             IF cType1 EQ "Job"   THEN "A1A5E2" ELSE "FF8585")
                            '" align="center" nowrap><font face="{&fontFace}">'
                            .
                        CASE jdx:
                            WHEN 1 THEN
                                IF dPercentage EQ 100 THEN
                                PUT UNFORMATTED "24:00:00".
                                ELSE
                                PUT UNFORMATTED STRING(iTime - ttTime.timeSlice,"hh:mm:ss").
                            WHEN 2 THEN
                                PUT UNFORMATTED dPercentage '%'.
                            WHEN 3 THEN DO:
                                IF dPercentage EQ 100 THEN
                                PUT UNFORMATTED "24:00:00".
                                ELSE
                                PUT UNFORMATTED STRING(iTime - ttTime.timeSlice,"hh:mm:ss").
                                PUT UNFORMATTED ' / ' dPercentage '%'.
                            END. /* 5 or 6 */
                        END CASE.
                        PUT UNFORMATTED 
                            '</font></td>' SKIP
                            '          </tr>' SKIP
                            .
                    END. /* if gt 0 */
                    ASSIGN
                        iTime  = ttTime.timeSlice
                        cType1 = ttTime.timeType1
                        cType2 = ttTime.timeType2
                        .
                END. /* each tttime */
                PUT UNFORMATTED
                    '        </table>' SKIP 
                    '      </td>' SKIP
                    .
            END. /* do idx */
            PUT UNFORMATTED
                '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF">'
                ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</font></td>' SKIP
                '    </tr>' SKIP
                .
        END. /* do dtdate */
        RUN pOutputResources.
        PUT UNFORMATTED
            '  </table>' SKIP
            '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
            '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
            '  <table align="center" cellspacing="2" cellpadding="8">' SKIP
            '    <tr>' SKIP 
            '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
            '      <td bgcolor="#85FEFE"><font face="{&fontFace}"><b>' baseOnText '</b></font></td>' SKIP  
            '      <td bgcolor="#FF8585"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
            '      <td bgcolor="#A1A5E2"><font face="{&fontFace}"><b>Booked Job</b></font></td>' SKIP
            '      <td bgcolor="#97F3A0"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
            '    </tr>' SKIP  
            '  </table>' SKIP 
            '</fieldset>' SKIP
            '</form>' SKIP
            '</html>' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START VALUE(cHTMLPage).
        PAUSE 1 NO-MESSAGE.
    END. /* do jdx */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadDowntime Dialog-Frame 
PROCEDURE pLoadDowntime :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO FORMAT "X(60)".
    DEFINE VARIABLE cAttrList     AS CHARACTER NO-UNDO FORMAT "X(4)".
    DEFINE VARIABLE cListItems    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResource     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cResourceList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttFolder FOR ttFolder.

    CREATE ttFolder.
    ttFolder.folderName = ".\schedule\data\ASI".
    FOR EACH ttFolder
        WHERE ttFolder.searched EQ NO
        :
        INPUT FROM OS-DIR(ttFolder.folderName) NO-ECHO.
        REPEAT:
            SET cFileName ^ cAttrList.
            IF cAttrList EQ "d" THEN DO:
                IF cFileName EQ "." OR cFileName EQ ".." THEN NEXT.
                CREATE bttFolder.
                bttFolder.folderName = ttFolder.folderName + "\" + cFileName.
            END.
            ELSE
            IF cAttrList EQ "f" AND
               INDEX(cFileName,"downtimes.") NE 0 AND
               INDEX(cFileName,".dat") NE 0 THEN
            cListItems = cListItems + ttFolder.folderName + "\" + cFileName + ",".
        END. /* repeat */
        INPUT CLOSE.
        ttFolder.searched = YES.
    END. /* each ttfolder */
    cListItems = TRIM(cListItems,",").

    EMPTY TEMP-TABLE ttblDowntime.
    DO idx = 1 TO NUM-ENTRIES(cListItems):
        EMPTY TEMP-TABLE ttResource.
        cResourceList = REPLACE(ENTRY(idx,cListItems),"downtimes.Actual","ResourceList").
        INPUT FROM VALUE(SEARCH(cResourceList)) NO-ECHO.
        IMPORT ^.
        REPEAT:
            IMPORT cResource.
            CREATE ttResource.
            ttResource.resource = cResource.
        END. /* repeat */
        INPUT CLOSE.

        INPUT FROM VALUE(SEARCH(ENTRY(idx,cListItems))) NO-ECHO.
        REPEAT:
            IMPORT tempDowntime.
            tempDowntime.dayID = tempDowntime.dayID MODULO 7.
            IF tempDowntime.dayID EQ 0 THEN
            tempDowntime.dayID = 7.
            IF tempDowntime.resource EQ "<Calendar>" THEN DO:
                FOR EACH ttResource
                    :
                    tempDowntime.resource = ttResource.resource.
                    RUN pCreateTtblDowntime.
                END. /* each ttbljob */
            END.
            ELSE
            IF CAN-FIND(FIRST ttResource
                        WHERE ttResource.resource EQ tempDowntime.resource) THEN
            RUN pCreateTtblDowntime.
        END. /* repeat */
        INPUT CLOSE.
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutputResources Dialog-Frame
PROCEDURE pOutputResources:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    PUT UNFORMATTED    
        '    <tr>' SKIP
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        .
    FOR EACH ttblJob
        WHERE ttblJob.newJob EQ YES
        BREAK BY ttblJob.startDateTime
              BY ttblJob.m-code
        :
        PUT UNFORMATTED
            '      <td bgcolor="#576490" align="left" nowrap><font face="{&fontFace}" color="#FFFFFF">'
            '<img src="'
            (IF SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") NE ? THEN
                SEARCH("Graphics/48x48/" + ttblJob.m-code + ".png") ELSE
                SEARCH("Graphics/48x48/gearwheels.png"))
            '" width="48" height="48" align="left">~&nbsp<b>'
            ttblJob.m-code '</b> (Form:<b>' ttblJob.frm
            '</b> Blank:<b>' ttblJob.blank-no
            '</b> Pass:<b>' ttblJob.pass ')</b><br>'
            ttblJob.startDate ' - ' STRING(ttblJob.startTime,"hh:mm:ss am") '<br>'
            ttblJob.endDate ' - ' STRING(ttblJob.endTime,"hh:mm:ss am") '</font></td>' SKIP
            .
    END. /* each ttbljob */
    PUT UNFORMATTED
        '      <td bgcolor="#576490" align="center" nowrap><font face="{&fontFace}" color="#FFFFFF"><b>'
        'Operation</b></font></td>' SKIP
        '    </tr>' SKIP
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveUnusedDowntime Dialog-Frame
PROCEDURE pRemoveUnusedDowntime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH ttblDowntime
        :
        IF NOT CAN-FIND(FIRST ttJob
                        WHERE ttJob.m-code EQ ttblDowntime.resource) THEN
        DELETE ttblDowntime.
    END. /* each ttbldowntime */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScheduleJob Dialog-Frame 
PROCEDURE pScheduleJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iprRowID AS ROWID     NO-UNDO.

  DEFINE VARIABLE lvStartDate     AS DATE      NO-UNDO.
  DEFINE VARIABLE lvStartTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvEndDate       AS DATE      NO-UNDO.
  DEFINE VARIABLE lvEndTime       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvEndDateTime   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvTimeSpan      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvEndDateMR     AS DATE      NO-UNDO.
  DEFINE VARIABLE lvEndTimeMR     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvMachine       AS CHARACTER NO-UNDO.

  ASSIGN
    lvStartDate = TODAY
    lvStartTime = TIME
    .
  EMPTY TEMP-TABLE ttblJob.
  FOR EACH ttJob USE-INDEX ttJob:
    RUN ttblJobCreate (ttJob.company,ttJob.m-code,ROWID(ttJob)).
    RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,ttJob.run-hr,
                 OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
      lvEndDateTime   = numericDateTime(lvEndDate,lvEndTime)
      lvTimeSpan      = timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime)
      .
    RUN firstAvailable (
        ttJob.m-code,
        lvTimeSpan,
        lvStartDateTime,
        lvEndDateTime,
        OUTPUT lvStartDate,
        OUTPUT lvStartTime,
        OUTPUT lvEndDate,
        OUTPUT lvEndTime
        ).
    RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,0,
                 OUTPUT lvEndDateMR,OUTPUT lvEndTimeMR).
    CREATE ttblJob.
    ASSIGN
      ttblJob.m-code        = ttJob.m-code
      ttblJob.job           = ttJob.job-no + '-'
                            + STRING(ttJob.job-no2) + '.'
                            + STRING(ttJob.frm)
      ttblJob.frm           = ttJob.frm
      ttblJob.blank-no      = ttJob.blank-no
      ttblJob.pass          = ttJob.pass
      ttblJob.startDate     = lvStartDate
      ttblJob.startTime     = lvStartTime
      ttblJob.endDate       = lvEndDate
      ttblJob.endTime       = lvEndTime
      ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
      ttblJob.endDateTime   = lvEndDateTime
      ttblJob.newJob        = YES
      lvStartDate           = lvEndDate
      lvStartTime           = lvEndTime
      .
  END. /* each ttjob */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttblJobCreate Dialog-Frame 
PROCEDURE ttblJobCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID   AS ROWID     NO-UNDO.

  DEFINE VARIABLE lvStartDate      AS DATE      NO-UNDO.
  DEFINE VARIABLE lvStartTime      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvEndDate        AS DATE      NO-UNDO.
  DEFINE VARIABLE lvEndTime        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvStartDateTime  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvEndDateTime    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lvTimeSpan       AS INTEGER   NO-UNDO.

  lvStartDateTime = numericDateTime(TODAY,TIME).
  FOR EACH mach NO-LOCK
      WHERE mach.company    EQ ipCompany
        AND mach.sch-m-code EQ ipMachine
      :
      FOR EACH bJobMch NO-LOCK
          WHERE bJobMch.company       EQ mach.company
            AND bJobMch.m-code        EQ mach.m-code
            AND bJobMch.run-complete  EQ NO
            AND bJobMch.start-date-su NE ?
            AND ROWID(bJobMch)        NE ipRowId
             BY bJobMch.start-date-su
             BY bJobMch.start-time-su
             BY bJobMch.end-date
             BY bJobMch.end-time
          :
        IF CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company EQ bJobMch.company
                      AND job-hdr.job     EQ bJobMch.job
                      AND job-hdr.opened  EQ NO) THEN NEXT.
        ASSIGN
          lvEndDate     = bJobMch.end-date
          lvEndTime     = fixTime(bJobMch.end-time)
          lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
          .
        IF lvStartDateTime GT lvEndDateTime THEN NEXT.
        ASSIGN
          lvStartDate = bJobMch.start-date-su
          lvStartTime = fixTime(bJobMch.start-time-su)
          .
        CREATE ttblJob.
        ASSIGN
          ttblJob.m-code        = ipMachine
          ttblJob.job           = bJobMch.job-no + '-'
                                + STRING(bJobMch.job-no2) + '.'
                                + STRING(bJobMch.frm)
          ttblJob.frm           = bJobMch.frm
          ttblJob.blank-no      = bJobMch.blank-no
          ttblJob.pass          = bJobMch.pass
          ttblJob.startDate     = lvStartDate
          ttblJob.startTime     = lvStartTime
          ttblJob.endDate       = lvEndDate
          ttblJob.endTime       = lvEndTime
          ttblJob.endDateTime   = lvEndDateTime
          ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
          ttblJob.newJob        = NO
          .
      END. /* each bjobmch */
  END. /* each mach */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */
