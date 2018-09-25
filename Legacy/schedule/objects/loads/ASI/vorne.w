&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : vorne.w
    Purpose     : capture vorne transactions into machtran

    Syntax      : run vorne.p (input vorne data file)

    Description : Vorne Interface

    Author(s)   : Ron Stark
    Created     :  2.24.2015
    ReWritten   : 10.29.2015
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER lvVorneDat AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
 
{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/{&Board}/calcEnd.i}

DEFINE VARIABLE lvVorneFile AS CHARACTER NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE lvVorneData AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneRun AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneStartDate AS DATE NO-UNDO.
DEFINE VARIABLE lvVorneStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneEnd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneEndDate AS DATE NO-UNDO.
DEFINE VARIABLE lvVorneEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneDuration AS DECIMAL NO-UNDO.
DEFINE VARIABLE lvVorneMRRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneState AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneTranRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneTranRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneEmployee AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneLastName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneFirstName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvAttrList AS CHARACTER NO-UNDO FORMAT 'x(4)'.
DEFINE VARIABLE lvFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvTemp AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProcessed AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvArchive AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvErrorFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobMchRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvShifts AS CHARACTER NO-UNDO INIT 'First,Second,Third,Fourth,Fifth,Sixth'.
DEFINE VARIABLE lvTotalTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvDateLoop AS DATE NO-UNDO.
DEFINE VARIABLE lvPostVorne AS LOGICAL NO-UNDO.
DEFINE VARIABLE lvHoldFile AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttblVorne NO-UNDO
 FIELD vorneResource AS CHAR
 FIELD vorneJob AS CHAR
 FIELD vorneItem AS CHAR
 FIELD vorneSeq AS INT
 FIELD vorneStartDate AS DATE
 FIELD vorneStartTime AS INT
 FIELD vorneEndDate AS DATE
 FIELD vorneEndTime AS INT
 FIELD vorneDuration AS DEC
 FIELD vorneReason AS CHAR
 FIELD vorneState# AS INT
 FIELD vorneState AS CHAR
 FIELD vorneRunQty AS INT
 FIELD vorneRejectQty AS INT
 FIELD vorneEmployee AS CHAR
 FIELD vorneShift AS CHAR
 FIELD vorneTranRunQty AS INT
 FIELD vorneTranRejectQty AS INT
 FIELD deleteFlag AS LOG
   INDEX ttblVorneDetail IS PRIMARY
         vorneResource
         vorneJob
         vorneItem
         vorneSeq
   INDEX ttblVorneSummary
         vorneResource
         vorneJob
         vorneItem
         vorneState#
         vorneShift
         vorneSeq
   INDEX vorneSeq
         vorneSeq
   INDEX empLogin
         vorneResource
         vorneShift
         vorneEmployee
         vorneStartDate
         vorneStartTime
         vorneEndDate
         vorneEndTime
         .

DEFINE BUFFER buffVorne FOR ttblVorne.

DEFINE STREAM sVorne.
DEFINE STREAM sHold.
DEFINE STREAM sProcessed.
DEFINE STREAM sError.

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 selectedShift selectedStartDate ~
selectedEndDate btnPost btnNonPost btnCancel lvVorneDir lvVorneType ~
lvEmpLogin lvVorneBlankEmployee lvRunComplete lvResourceList btnSave ~
btnReset btnCancel-2 
&Scoped-Define DISPLAYED-OBJECTS selectedShift selectedStartDate ~
selectedEndDate lvVorneDir lvVorneType lvEmpLogin lvVorneBlankEmployee ~
lvRunComplete lvResourceList 

/* Custom List Definitions                                              */
/* VorneDatValues,List-2,List-3,List-4,List-5,List-6                    */
&Scoped-define VorneDatValues lvVorneDir lvVorneType lvEmpLogin ~
lvVorneBlankEmployee lvRunComplete lvResourceList 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Cancel (Exit)"
     BGCOLOR 8 .

DEFINE BUTTON btnCancel-2 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Cancel (Exit)"
     BGCOLOR 8 .

DEFINE BUTTON btnNonPost 
     IMAGE-UP FILE "schedule/images/moveresource.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Non-Post"
     BGCOLOR 8 .

DEFINE BUTTON btnPost 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Post"
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Reset"
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 6 BY 1.43 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE VARIABLE lvResourceList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resources" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvRunComplete AS CHARACTER FORMAT "X(256)":U 
     LABEL "Run Complete" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvVorneBlankEmployee AS CHARACTER FORMAT "X(256)":U 
     LABEL "Blank Employee" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvVorneDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vorne Directory" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE selectedEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lvEmpLogin AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vorne", "Vorne",
"Touch Screen", "Touch Screen"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE lvVorneType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Summary", "Summary",
"Detail", "Detail"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE selectedShift AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "First Shift", "First Shift",
"Second Shift", "Second Shift",
"Third Shift", "Third Shift",
"All", "All"
     SIZE 19 BY 3.81 TOOLTIP "Select Shift" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.8 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     selectedShift AT ROW 1.95 COL 2 HELP
          "Select Shift" NO-LABEL
     selectedStartDate AT ROW 1.95 COL 31 COLON-ALIGNED HELP
          "Enter Starting Date"
     selectedEndDate AT ROW 3.14 COL 31 COLON-ALIGNED HELP
          "Enter Ending Date"
     btnPost AT ROW 4.33 COL 33 HELP
          "Click to Post"
     btnNonPost AT ROW 4.33 COL 39 HELP
          "Click to Reflect Transactions w/o Posting"
     btnCancel AT ROW 4.33 COL 45 HELP
          "Click to Cancel and Exit"
     lvVorneDir AT ROW 6.48 COL 16 COLON-ALIGNED
     lvVorneType AT ROW 7.67 COL 19 NO-LABEL
     lvEmpLogin AT ROW 8.86 COL 19 NO-LABEL
     lvVorneBlankEmployee AT ROW 10.05 COL 16 COLON-ALIGNED
     lvRunComplete AT ROW 11.24 COL 16 COLON-ALIGNED
     lvResourceList AT ROW 12.43 COL 16 COLON-ALIGNED
     btnSave AT ROW 13.86 COL 33 HELP
          "Click to Save"
     btnReset AT ROW 13.86 COL 39 HELP
          "Click to Reset Values"
     btnCancel-2 AT ROW 13.86 COL 45 HELP
          "Click to Cancel and Exit"
     "Type:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 7.67 COL 12
     "Employee Login:" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 8.86 COL 2
     "Select Shift to Post ... Enter Date Range" VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 1.24 COL 2
          FONT 6
     RECT-1 AT ROW 6 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.8 BY 14.29.


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
         TITLE              = "Vorne Data Collection"
         HEIGHT             = 14.29
         WIDTH              = 50.8
         MAX-HEIGHT         = 14.29
         MAX-WIDTH          = 50.8
         VIRTUAL-HEIGHT     = 14.29
         VIRTUAL-WIDTH      = 50.8
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET lvEmpLogin IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvResourceList IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvRunComplete IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvVorneBlankEmployee IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvVorneDir IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET lvVorneType IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vorne Data Collection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vorne Data Collection */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME
DO:
  opContinue = FALSE.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel-2 C-Win
ON CHOOSE OF btnCancel-2 IN FRAME DEFAULT-FRAME
DO:
  opContinue = FALSE.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNonPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNonPost C-Win
ON CHOOSE OF btnNonPost IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    selectedShift
    selectedStartDate
    selectedEndDate
    opContinue = NO
    lvPostVorne = NO
    .
  RUN postVorne.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPost C-Win
ON CHOOSE OF btnPost IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    selectedShift
    selectedStartDate
    selectedEndDate
    opContinue = YES
    lvPostVorne = YES
    .
  RUN postVorne.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME
DO:
  RUN getVorneDatValues.
  DISPLAY {&vorneDatValues} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&vorneDatValues}.
  RUN saveVorneDatValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedShift C-Win
ON VALUE-CHANGED OF selectedShift IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    {&SELF-NAME}
    selectedEndDate = TODAY
    .
  CASE {&SELF-NAME}:
    WHEN 'First' OR WHEN 'Second' THEN
    selectedStartDate = TODAY.
    WHEN 'Third' THEN
    selectedStartDate = TODAY - 1.
  END CASE.
  DISPLAY selectedStartDate selectedEndDate WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
DISABLE TRIGGERS FOR LOAD OF machemp.
DISABLE TRIGGERS FOR LOAD OF machtran.
DISABLE TRIGGERS FOR LOAD OF emplogin.
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  
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
  RUN getVorneDatValues.
  ASSIGN
    selectedStartDate = TODAY
    selectedEndDate = TODAY
    .
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcQtyPerTrans C-Win 
PROCEDURE calcQtyPerTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* calculate qty values for each transaction */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneSeq:
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
      ASSIGN
        lvVorneRunQty = 0
        lvVorneRejectQty = 0
        .
      FOR EACH machtran NO-LOCK
          WHERE machtran.company EQ job-mch.company
            AND machtran.machine EQ ttblVorne.vorneResource
            AND machtran.job_number EQ job-mch.job-no
            AND machtran.job_sub EQ job-mch.job-no2
            AND machtran.form_number EQ job-mch.frm
            AND machtran.blank_number EQ job-mch.blank-no
            AND machtran.pass_sequence EQ job-mch.pass
            AND machtran.jobseq EQ 0:
        ASSIGN
          lvVorneRunQty = lvVorneRunQty + machtran.run_qty
          lvVorneRejectQty = lvVorneRejectQty + machtran.waste_qty
          .
      END. /* each machtran */
    END. /* first-of job */

    /* IF lvVorneRunQty LE ttblVorne.vorneRunQty THEN */
    ASSIGN
      ttblVorne.vorneTranRunQty = ttblVorne.vorneRunQty - lvVorneRunQty
      ttblVorne.vorneTranRejectQty = ttblVorne.vorneRejectQty - lvVorneRejectQty
      .
    ASSIGN
      lvVorneRunQty = ttblVorne.vorneRunQty
      lvVorneRejectQty = ttblVorne.vorneRejectQty
      .
  END. /* each ttblvorne */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completeMR C-Win 
PROCEDURE completeMR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND LAST machtran EXCLUSIVE-LOCK
       WHERE machtran.company EQ job-mch.company
         AND machtran.machine EQ ttblVorne.vorneResource
         AND machtran.job_number EQ job-mch.job-no
         AND machtran.job_sub EQ job-mch.job-no2
         AND machtran.form_number EQ job-mch.frm
         AND machtran.blank_number EQ job-mch.blank-no
         AND machtran.pass_sequence EQ job-mch.pass
         AND machtran.charge_code EQ 'MR'
       NO-ERROR.
  IF AVAILABLE machtran THEN
  machtran.completed = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createEmpLogin C-Win 
PROCEDURE createEmpLogin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER ttblVorne FOR ttblVorne.
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE OUTPUT PARAMETER opEmpLoginRowID AS ROWID NO-UNDO.

  IF ttblVorne.vorneEmployee EQ '' OR
     NUM-ENTRIES(ttblVorne.vorneEmployee,' ') LT 2 THEN RETURN.
  ASSIGN
    lvVorneFirstName = ENTRY(1,ttblVorne.vorneEmployee,' ')
    lvVorneLastName = ENTRY(2,ttblVorne.vorneEmployee,' ')
    .
  FIND FIRST employee NO-LOCK
       WHERE employee.last_name EQ lvVorneLastName
         AND employee.first_name EQ lvVorneFirstName
       NO-ERROR.
  IF AVAILABLE employee THEN DO:
    IF CAN-FIND(FIRST emplogin
                WHERE emplogin.company EQ employee.company
                  AND emplogin.employee EQ employee.employee
                  AND emplogin.start_date EQ machtran.start_date
                  AND emplogin.start_time EQ machtran.start_time
                  AND emplogin.machine EQ ttblVorne.vorneResource) THEN
    ASSIGN
      machtran.start_time = machtran.start_time + 1
      machtran.total_time = machtran.total_time + 1
      .
    CREATE empLogin.
    ASSIGN
      emplogin.company = employee.company
      emplogin.employee = employee.employee
      emplogin.machine = ttblVorne.vorneResource
      emplogin.start_date = machtran.start_date
      emplogin.start_time = machtran.start_time
      emplogin.end_date = machtran.end_date
      emplogin.end_time = machtran.end_time
      emplogin.total_time = machtran.total_time
      emplogin.shift = ttblVorne.vorneShift
      opEmpLoginRowID = ROWID(emplogin)
      .
  END. /* avail employee */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createMachEmp C-Win 
PROCEDURE createMachEmp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE INPUT PARAMETER ipEmpLoginRowID AS ROWID NO-UNDO.

  FIND FIRST emplogin NO-LOCK
       WHERE ROWID(emplogin) EQ ipEmpLoginRowID NO-ERROR.
  IF NOT AVAILABLE emplogin THEN RETURN.

  FIND FIRST employee NO-LOCK
       WHERE employee.company EQ emplogin.company
         AND employee.employee EQ emplogin.employee
       NO-ERROR.
  IF NOT AVAILABLE employee THEN RETURN.

  CREATE machemp.
  ASSIGN
    machemp.table_rec_key = machtran.rec_key
    machemp.employee = emplogin.employee
    machemp.start_date = IF machtran.start_date GT emplogin.start_date THEN machtran.start_date ELSE emplogin.start_date
    machemp.start_time = IF machtran.start_date GT emplogin.start_date THEN machtran.start_time ELSE emplogin.start_time
    machemp.shift = machtran.shift
    machemp.ratetype = 'Standard' 
    machemp.rate_usage = employee.rate_usage
    machemp.end_date = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_date ELSE emplogin.end_date
    machemp.end_time = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_time ELSE emplogin.end_time
    .
  RUN employeeRate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                   machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
  IF machemp.start_date EQ machemp.end_date THEN
  machemp.total_time = machemp.end_time - machemp.start_time.
  ELSE
  machemp.total_time = (86400 - machemp.start_time)
                     + (machemp.end_date - machemp.start_date - 1) * 86400
                     +  machemp.end_time.
  /*if end_date is blank, set total_time to 0*/
  IF machemp.total_time LT 0 OR machemp.total_time EQ ? THEN
     machemp.total_time = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtblVorne C-Win 
PROCEDURE createTtblVorne :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipVorneFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.

  EMPTY TEMP-TABLE ttblVorne.
  INPUT STREAM sVorne FROM VALUE(ipVorneFile).
  IF lvPostVorne THEN DO:
    OUTPUT STREAM sHold TO VALUE(lvHoldFile).
    OUTPUT STREAM sProcessed TO VALUE(lvProcessed).
  END. /* if lvpostvorne */
  REPEAT:
    IMPORT STREAM sVorne UNFORMATTED lvVorneData.
    IF lvResourceList NE '' AND NOT CAN-DO(lvResourceList,ENTRY(1,lvVorneData,'|')) THEN NEXT.
    ASSIGN
      lvVorneStart = ENTRY(5,lvVorneData,'|')
      lvVorneStart = SUBSTR(lvVorneStart,1,10)
      lvStartDate = DATE(INT(ENTRY(2,lvVorneStart,'-')),
                         INT(ENTRY(3,lvVorneStart,'-')),
                         INT(ENTRY(1,lvVorneStart,'-')))
      lvVorneEnd = ENTRY(6,lvVorneData,'|')
      lvVorneEnd = SUBSTR(lvVorneEnd,1,10)
      lvEndDate = DATE(INT(ENTRY(2,lvVorneEnd,'-')),
                       INT(ENTRY(3,lvVorneEnd,'-')),
                       INT(ENTRY(1,lvVorneEnd,'-')))
      .
    IF lvPostVorne AND
      ((selectedShift NE 'All' AND
        selectedShift NE ENTRY(13,lvVorneData,'|')) OR
        lvStartDate LT selectedStartDate OR
        lvEndDate GT selectedEndDate) THEN DO:
      PUT STREAM sHold UNFORMATTED lvVorneData SKIP.
      NEXT.
    END. /* if vorneshift ne */
    ASSIGN
      lvVorneResource = ENTRY(1,lvVorneData,'|')
      lvVorneJob = LEFT-TRIM(ENTRY(2,lvVorneData,'|'))
      lvVorneEmployee = IF ENTRY(12,lvVorneData,'|') EQ '' THEN lvVorneBlankEmployee
                        ELSE ENTRY(12,lvVorneData,'|')
      .
    /* scan failed and created null transactions, save them and try to fix later */
    IF lvVorneJob NE 'null' THEN
    ASSIGN
      lvVorneRun = ENTRY(2,lvVorneJob,'-')
      lvVorneForm = ENTRY(3,lvVorneJob,'-')
      lvVorneJob = ENTRY(1,lvVorneJob,'-')
      lvVorneJob = lvVorneJob + '-'
                 + STRING(INT(lvVorneRun)) + '.'
                 + STRING(INT(lvVorneForm))
      .
    IF lvVorneJob NE 'null' AND
       NOT CAN-FIND(FIRST ttblJob
                    WHERE ttblJob.resource EQ lvVorneResource
                      AND ttblJob.job EQ lvVorneJob) AND
       NOT CAN-FIND(FIRST pendingJob
                    WHERE pendingJob.resource EQ lvVorneResource
                      AND pendingJob.job EQ lvVorneJob) THEN DO:
      PUT STREAM sError UNFORMATTED lvVorneData SKIP.
      NEXT.
    END. /* cannot find job in SB */
    CREATE ttblVorne.
    ASSIGN
      ttblVorne.vorneResource = lvVorneResource
      ttblVorne.vorneJob = lvVorneJob
      ttblVorne.vorneItem = ENTRY(3,lvVorneData,'|')
      ttblVorne.vorneSeq = INT(ENTRY(4,lvVorneData,'|'))
      lvVorneStart = ENTRY(5,lvVorneData,'|')
      lvVorneStart = SUBSTR(lvVorneStart,1,10)
      ttblVorne.vorneStartDate = DATE(INT(ENTRY(2,lvVorneStart,'-')),
                                      INT(ENTRY(3,lvVorneStart,'-')),
                                      INT(ENTRY(1,lvVorneStart,'-')))
      lvVorneStart = ENTRY(5,lvVorneData,'|')
      lvVorneStart = SUBSTR(lvVorneStart,12,8)
      ttblVorne.vorneStartTime = INT(ENTRY(1,lvVorneStart,':')) * 3600
                               + INT(ENTRY(2,lvVorneStart,':')) * 60
                               + INT(ENTRY(3,lvVorneStart,':'))
      lvVorneEnd = ENTRY(6,lvVorneData,'|')
      lvVorneEnd = SUBSTR(lvVorneEnd,1,10)
      ttblVorne.vorneEndDate = DATE(INT(ENTRY(2,lvVorneEnd,'-')),
                                    INT(ENTRY(3,lvVorneEnd,'-')),
                                    INT(ENTRY(1,lvVorneEnd,'-')))
      lvVorneEnd = ENTRY(6,lvVorneData,'|')
      lvVorneEnd = SUBSTR(lvVorneEnd,12,8)
      ttblVorne.vorneEndTime = INT(ENTRY(1,lvVorneEnd,':')) * 3600
                             + INT(ENTRY(2,lvVorneEnd,':')) * 60
                             + INT(ENTRY(3,lvVorneEnd,':'))
      ttblVorne.vorneDuration = DEC(ENTRY(7,lvVorneData,'|'))
      ttblVorne.vorneReason = ENTRY(8,lvVorneData,'|')
      lvVorneState = ENTRY(9,lvVorneData,'|')
      lvVorneState = REPLACE(lvVorneState,'_enum','')
      ttblVorne.vorneState# = IF CAN-DO('run,down',lvVorneState) THEN 2
                         ELSE IF lvVorneState EQ 'setup' THEN 1
                         ELSE 3
      ttblVorne.vorneState = IF CAN-DO('run,down',lvVorneState) THEN 'RUN'
                        ELSE IF lvVorneState EQ 'setup' THEN 'MR'
                        ELSE 'NC'
      ttblVorne.vorneEmployee = lvVorneEmployee
      ttblVorne.vorneShift = ENTRY(1,ENTRY(13,lvVorneData,'|'),' ')
      ttblVorne.vorneShift = STRING(LOOKUP(ttblVorne.vorneShift,lvShifts))
      .
    IF lvVorneJob NE 'null' THEN
    ASSIGN
      ttblVorne.vorneRunQty = INT(ENTRY(10,lvVorneData,'|'))
      ttblVorne.vorneRejectQty = INT(ENTRY(11,lvVorneData,'|'))
      .
    /* cannot have a midnight ending time */
    IF ttblVorne.vorneEndTime EQ 0 THEN
    ttblVorne.vorneEndTime = 86340.
    /* scan failed and created null transactions, check if they belong to this one */
    FOR EACH buffVorne
        WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
          AND buffVorne.vorneJob EQ 'null'
          AND buffVorne.vorneSeq LT ttblVorne.vorneSeq
        :
      ASSIGN
        buffVorne.vorneJob = ttblVorne.vorneJob
        buffVorne.vorneItem = ttblVorne.vorneItem
        buffVorne.vorneRunQty = ttblVorne.vorneRunQty
        buffVorne.vorneRejectQty = ttblVorne.vorneRejectQty
        .
    END. /* each buffvorne */
    IF lvPostVorne THEN
    PUT STREAM sProcessed UNFORMATTED lvVorneData SKIP.
  END. /* repeat */
  IF lvPostVorne THEN DO:
    OUTPUT STREAM sHold CLOSE.
    OUTPUT STREAM sProcessed CLOSE.
  END. /* if lvpostvorne */
  INPUT STREAM sVorne CLOSE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE employeeRate C-Win 
PROCEDURE employeeRate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEmployee AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipShift AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRate_usage AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRatetype AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opRate AS DECIMAL NO-UNDO.

  DEFINE BUFFER bRate FOR rate.

  IF ipRate_usage THEN
  ipMachine = ''.
  FIND bRate NO-LOCK
       WHERE bRate.company EQ ipCompany
         AND bRate.employee EQ ipEmployee
         AND bRate.shift EQ ipShift
         AND bRate.machine EQ ipMachine
         AND bRate.ratetype EQ 'Standard'
       NO-ERROR.
  IF NOT AVAILABLE bRate THEN
  RETURN.
  opRate = bRate.rate.
  FIND bRate NO-LOCK
       WHERE bRate.company = ipCompany
         AND bRate.employee = ipEmployee
         AND bRate.shift = ipShift
         AND bRate.machine = ipMachine
         AND bRate.ratetype = ipRatetype
       NO-ERROR.
  IF AVAILABLE bRate THEN
  CASE bRate.factortype:
    WHEN 'Straight' THEN
    opRate = bRate.rate.
    WHEN 'Additional' THEN
    opRate = opRate + bRate.rate.
    WHEN 'Multiply' THEN
    opRate = opRate * bRate.rate.
    OTHERWISE
    opRate = 0.
  END CASE.

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
  DISPLAY selectedShift selectedStartDate selectedEndDate lvVorneDir lvVorneType 
          lvEmpLogin lvVorneBlankEmployee lvRunComplete lvResourceList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 selectedShift selectedStartDate selectedEndDate btnPost 
         btnNonPost btnCancel lvVorneDir lvVorneType lvEmpLogin 
         lvVorneBlankEmployee lvRunComplete lvResourceList btnSave btnReset 
         btnCancel-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVorneDatValues C-Win 
PROCEDURE getVorneDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(lvVorneDat) NO-ECHO.
  IMPORT UNFORMATTED lvVorneDir.           /* location of vorne trans file   */
  IMPORT UNFORMATTED lvVorneType.          /* Summary or Detail              */
  IMPORT UNFORMATTED lvEmpLogin.           /* Vorne or TS                    */
  IMPORT UNFORMATTED lvVorneBlankEmployee. /* default employee if blank      */
  IMPORT UNFORMATTED lvRunComplete.        /* value to indicate run complete */
  IMPORT UNFORMATTED lvResourceList.       /* comma delimited list, or blank */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postVorne C-Win 
PROCEDURE postVorne :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM OS-DIR(lvVorneDir) NO-ECHO.
  REPEAT:
    SET lvVorneFile ^ lvAttrList.
    IF lvAttrList NE 'f' OR INDEX(lvVorneFile,'.psv') EQ 0 THEN NEXT.
    ASSIGN
      lvFile = lvVorneDir + '/' + lvVorneFile
      lvTemp = REPLACE(lvFile,'.psv','.tmp')
      lvHoldFile = REPLACE(lvFile,'.psv','.hold')
      lvProcessed = lvVorneDir + '/processed/'
                  + REPLACE(lvVorneFile,'.psv','.'
                  + STRING(YEAR(TODAY),'9999')
                  + STRING(MONTH(TODAY),'99')
                  + STRING(DAY(TODAY),'99')
                  + '.' + STRING(TIME,'99999')
                  + '.psv')
      lvArchive = REPLACE(lvProcessed,'processed','archive')
      lvErrorFile = REPLACE(lvProcessed,'processed','errors')
      .

    OUTPUT STREAM sError TO VALUE(lvErrorFile).
    PUT STREAM sError UNFORMATTED
      'Parameters - Shift: ' selectedShift
      ' - Start Date: ' selectedStartDate
      ' - End Date: ' selectedEndDate
      SKIP
      .

    /* move transactions to tmp file */
    IF lvPostVorne THEN DO:
      OS-COPY VALUE(lvFile) VALUE(lvArchive).
      OS-RENAME VALUE(lvFile) VALUE(lvTemp).
    END.
    ELSE OS-APPEND VALUE(lvFile) VALUE(lvTemp).

    /* append hold records */
    OS-APPEND VALUE(lvHoldFile) VALUE(lvTemp).
    
    /* create temp-table vorne records */
    RUN createTtblVorne (lvTemp).
    
    /* post to create machtran, or simply to reflect onto SB */
    IF lvPostVorne THEN DO:
      /* calculate qty values for each transaction */
      RUN calcQtyPerTrans.
      /* run Detail or Summary */
      RUN VALUE('vorne' + lvVorneType).
    END. /* if lvportvorne */
    /* not posting, simply reflect trans on SB jobs */
    ELSE RUN setSBJobs.

    /* remove tmp file */
    OS-DELETE VALUE(lvTemp).

    OUTPUT STREAM sError CLOSE.
  END. /* repeat */
  INPUT CLOSE.

  /* be sure voren file exists in case another import run */
  /* without file, the hold file is not processed         */
  IF SEARCH(lvFile) EQ ? THEN DO:
    OUTPUT TO VALUE(lvFile).
    OUTPUT CLOSE.
  END. /* if search ? */

  RELEASE machtran.

  OS-COMMAND NO-WAIT notepad.exe VALUE(lvErrorFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveVorneDatValues C-Win 
PROCEDURE saveVorneDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(lvVorneDat).
  PUT UNFORMATTED lvVorneDir SKIP.           /* location of vorne trans file   */
  PUT UNFORMATTED lvVorneType SKIP.          /* Summary or Detail              */
  PUT UNFORMATTED lvEmpLogin SKIP.           /* Vorne or TS                    */
  PUT UNFORMATTED lvVorneBlankEmployee SKIP. /* default employee if blank      */
  PUT UNFORMATTED lvRunComplete SKIP.        /* value to indicate run complete */
  PUT UNFORMATTED lvResourceList SKIP.       /* comma delimited list, or blank */
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRecKey C-Win 
PROCEDURE setRecKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  {custom/rec_key.i "machtran"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSBJobs C-Win 
PROCEDURE setSBJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneSeq
      :
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN DO:
        IF ttblJob.jobLocked THEN NEXT.
      END. /* avail ttbljob */
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        CREATE ttblJob.
        BUFFER-COPY pendingJob TO ttblJob.
        DELETE pendingJob.
      END. /* not avail ttbljob */
      ASSIGN
        ttblJob.startDate = ttblVorne.vorneStartDate
        ttblJob.startTime = ttblVorne.vorneStartTime
        .
      RUN calcEnd (ttblJob.startDate,ttblJob.startTime,0,ttblJob.timeSpan,
                   OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
    END. /* first-of job */
  END. /* each ttblvorne */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vorneDetail C-Win 
PROCEDURE vorneDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* not implemented ...
  
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneSeq:
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */
    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblVorne.vorneResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblVorne.vorneState
      machtran.completed = ttblVorne.vorneReason EQ lvRunComplete
      machtran.start_date = ttblVorne.vorneStartDate
      machtran.start_time = ttblVorne.vorneStartTime
      machtran.end_date = ttblVorne.vorneEndDate
      machtran.end_time = ttblVorne.vorneEndTime
      machtran.run_qty = ttblVorne.vorneTranRunQty
      machtran.waste_qty = ttblVorne.vorneTranRejectQty
      machtran.shift = ttblVorne.vorneShift
      .
    IF machtran.start_date = machtran.end_date THEN
    machtran.total_time = machtran.end_time - machtran.start_time.
    ELSE
    machtran.total_time = (86400 - machtran.start_time) +
                          (machtran.end_date - machtran.start_date - 1) * 86400 +
                           machtran.end_time.
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.
    RUN setRecKey.
    RUN createMachEmp (BUFFER machtran).
    IF ttblVorne.vorneReason EQ lvRunComplete THEN
    RUN completeMR.
  END. /* each ttblVorne */
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vorneSummary C-Win 
PROCEDURE vorneSummary :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE empLoginRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE lvState AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvTime AS INTEGER NO-UNDO.

  /* Pass 1: consolidate vorne transactions */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF FIRST-OF(ttblVorne.vorneShift) OR
       FIRST-OF(ttblVorne.vorneStartDate) OR
       lvState NE ttblVorne.vorneState THEN DO:
      FIND buffVorne WHERE ROWID(buffVorne) EQ ROWID(ttblVorne).
      lvState = ttblVorne.vorneState.
    END. /* first-of */
    IF AVAILABLE buffVorne AND (ROWID(buffVorne) NE ROWID(ttblVorne)) THEN DO:
      ASSIGN
        buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
        buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
        buffVorne.vorneDuration = buffVorne.vorneDuration + ttblVorne.vorneDuration
        buffVorne.vorneEndDate = ttblVorne.vorneEndDate
        buffVorne.vorneEndTime = ttblVorne.vorneEndTime
        ttblVorne.deleteFlag = YES
        .
    END. /* avail buffvorne */
  END. /* each ttblVorne */

  /* Pass 2: move non-RUN qty values to RUN transaction */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState NE 'RUN'
       AND  ttblVorne.deleteFlag EQ NO
       AND (ttblVorne.vorneTranRunQty NE 0
        OR  ttblVorne.vorneTranRejectQty NE 0)
      :
    FIND FIRST buffVorne
         WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
           AND buffVorne.vorneJob EQ ttblVorne.vorneJob
           AND buffVorne.vorneItem EQ ttblVorne.vorneItem
           AND buffVorne.vorneShift EQ ttblVorne.vorneShift
           AND buffVorne.vorneEmployee EQ ttblVorne.vorneEmployee
           AND buffVorne.vorneState EQ 'RUN'
           AND ttblVorne.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffVorne THEN
    ASSIGN
      buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
      buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
      ttblVorne.vorneTranRunQty = 0
      ttblVorne.vorneTranRejectQty = 0
      .
  END. /* each ttblVorne */

  /* Pass 3: flag records as run completed if necessary */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState EQ 'RUN'
        AND ttblVorne.vorneReason EQ lvRunComplete
        AND ttblVorne.deleteFlag EQ YES
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    FIND LAST buffVorne
         WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
           AND buffVorne.vorneJob EQ ttblVorne.vorneJob
           AND buffVorne.vorneItem EQ ttblVorne.vorneItem
           AND buffVorne.vorneEmployee EQ ttblVorne.vorneEmployee
           AND buffVorne.vorneShift EQ ttblVorne.vorneShift
           AND buffVorne.vorneState EQ 'RUN'
           AND buffVorne.vorneReason NE lvRunComplete
           AND buffVorne.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffVorne THEN
    buffVorne.vorneReason = lvRunComplete.
  END. /* each ttblvorne */
  
  /* Pass 4: remove deleted flag records */
  FOR EACH ttblVorne:
    IF ttblVorne.deleteFlag EQ YES OR
      (ttblVorne.vorneState EQ 'NC' AND
       ttblVorne.vorneDuration LE 60 AND
       ttblVorne.vorneTranRunQty EQ 0 AND
       ttblVorne.vorneTranRejectQty EQ 0 AND
       ttblVorne.vorneReason NE lvRunComplete) OR
       ttblVorne.vorneJob EQ 'null' THEN
    DELETE ttblVorne.
  END. /* each ttblvorne */

  /* Pass 5: split records that span midnight */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF ttblVorne.deleteFlag THEN NEXT.
    IF ttblVorne.vorneStartDate NE ttblVorne.vorneEndDate THEN DO:
      CREATE buffVorne.
      BUFFER-COPY ttblVorne TO buffVorne.
      ASSIGN
        buffVorne.vorneStartDate = ttblVorne.vorneEndDate
        buffVorne.vorneStartTime = 0
        buffVorne.vorneDuration = buffVorne.vorneEndTime - buffVorne.vorneStartTime
        ttblVorne.vorneEndDate = ttblVorne.vorneStartDate
        ttblVorne.vorneEndTime = 86340
        ttblVorne.vorneDuration = ttblVorne.vorneEndTime - ttblVorne.vorneStartTime
        ttblVorne.vorneTranRunQty = 0
        ttblVorne.vorneTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblVorne */

  /* Pass 6: consolidate vorne transactions again */
  RELEASE buffVorne.
  lvState = ''.
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF FIRST-OF(ttblVorne.vorneShift) OR
       FIRST-OF(ttblVorne.vorneStartDate) OR
       lvState NE ttblVorne.vorneState THEN DO:
      FIND buffVorne WHERE ROWID(buffVorne) EQ ROWID(ttblVorne).
      lvState = ttblVorne.vorneState.
    END. /* first-of */
    IF AVAILABLE buffVorne AND (ROWID(buffVorne) NE ROWID(ttblVorne)) THEN DO:
      ASSIGN
        buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
        buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
        buffVorne.vorneDuration = buffVorne.vorneDuration + ttblVorne.vorneDuration
        buffVorne.vorneEndDate = ttblVorne.vorneEndDate
        buffVorne.vorneEndTime = ttblVorne.vorneEndTime
        buffVorne.vorneReason = ttblVorne.vorneReason
        ttblVorne.deleteFlag = YES
        .
    END. /* avail buffvorne */
  END. /* each ttblVorne */
  
  /* Pass 7: move RUN qty values to last RUN transaction */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState EQ 'RUN'
       AND  ttblVorne.deleteFlag EQ NO
       AND (ttblVorne.vorneTranRunQty NE 0
        OR  ttblVorne.vorneTranRejectQty NE 0)
      :
    FIND LAST buffVorne
         WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
           AND buffVorne.vorneJob EQ ttblVorne.vorneJob
           AND buffVorne.vorneItem EQ ttblVorne.vorneItem
           AND buffVorne.vorneShift EQ ttblVorne.vorneShift
           AND buffVorne.vorneEmployee EQ ttblVorne.vorneEmployee
           AND buffVorne.vorneState EQ 'RUN'
           AND buffVorne.deleteFlag EQ NO
           AND buffVorne.vorneSeq GT ttblVorne.vorneSeq
           AND ROWID(buffVorne) NE ROWID(ttblVorne)
         NO-ERROR.
    IF AVAILABLE buffVorne THEN
    ASSIGN
      buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
      buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
      ttblVorne.vorneTranRunQty = 0
      ttblVorne.vorneTranRejectQty = 0
      .
  END. /* each ttblVorne */
  
  /* Pass 8: create machtran records */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF ttblVorne.deleteFlag THEN NEXT.

    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      lvTime = ?.
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */

    IF lvTime EQ ? THEN
    lvTime = ttblVorne.vorneStartTime.
    /*
    ASSIGN
      ttblVorne.vorneDuration = ttblVorne.vorneEndTime - ttblVorne.vorneStartTime
      ttblVorne.vorneStartTime = lvTime
      ttblVorne.vorneEndTime = ttblVorne.vorneStartTime + ttblVorne.vorneDuration
      lvTime = ttblVorne.vorneEndTime
      .
    */
    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblVorne.vorneResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblVorne.vorneState
      machtran.completed = ttblVorne.vorneReason EQ lvRunComplete
      machtran.start_date = ttblVorne.vorneStartDate
      machtran.start_time = ttblVorne.vorneStartTime
      machtran.end_date = ttblVorne.vorneEndDate
      machtran.end_time = ttblVorne.vorneEndTime
      machtran.run_qty = ttblVorne.vorneTranRunQty
      machtran.waste_qty = ttblVorne.vorneTranRejectQty
      machtran.shift = ttblVorne.vorneShift
      machtran.total_time = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.
    
    IF lvEmpLogin EQ 'Vorne' THEN
    RUN createEmpLogin (BUFFER ttblVorne,BUFFER machtran,OUTPUT empLoginRowID).
    RUN setRecKey (BUFFER machtran).
    RUN createMachEmp (BUFFER machtran, INPUT empLoginRowID).
  END. /* each ttblVorne */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

