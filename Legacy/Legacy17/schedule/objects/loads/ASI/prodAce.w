&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : prodAce.w
    Purpose     : capture prodAce transactions into machtran

    Syntax      : run prodAce.w (input prodAce data file)

    Description : ProdAce Interface

    Author(s)   : Ron Stark
    Created     :  7.14.2017 copied from vorne.w
    ReWritten   : 
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER lvProdAceDat AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
 
{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/{&Board}/calcEnd.i}

DEFINE VARIABLE lvProdAceFile AS CHARACTER NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE lvProdAceData AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceRun AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceStartDate AS DATE NO-UNDO.
DEFINE VARIABLE lvProdAceStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceEnd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceEndDate AS DATE NO-UNDO.
DEFINE VARIABLE lvProdAceEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceDuration AS DECIMAL NO-UNDO.
DEFINE VARIABLE lvProdAceMRRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceState AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceTranRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceTranRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvProdAceEmployee AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceLastName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceFirstName AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lvPostProdAce AS LOGICAL NO-UNDO.
DEFINE VARIABLE lvHoldFile AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttblProdAce NO-UNDO
 FIELD prodAceResource AS CHAR
 FIELD prodAceJob AS CHAR
 FIELD prodAceItem AS CHAR
 FIELD prodAceSeq AS INT
 FIELD prodAceStartDate AS DATE
 FIELD prodAceStartTime AS INT
 FIELD prodAceEndDate AS DATE
 FIELD prodAceEndTime AS INT
 FIELD prodAceDuration AS DEC
 FIELD prodAceReason AS CHAR
 FIELD prodAceState# AS INT
 FIELD prodAceState AS CHAR
 FIELD prodAceRunQty AS INT
 FIELD prodAceRejectQty AS INT
 FIELD prodAceEmployee AS CHAR
 FIELD prodAceShift AS CHAR
 FIELD prodAceTranRunQty AS INT
 FIELD prodAceTranRejectQty AS INT
 FIELD deleteFlag AS LOG
   INDEX ttblProdAceDetail IS PRIMARY
         prodAceResource
         prodAceJob
         prodAceItem
         prodAceSeq
   INDEX ttblProdAceSummary
         prodAceResource
         prodAceJob
         prodAceItem
         prodAceState#
         prodAceShift
         prodAceSeq
   INDEX prodAceSeq
         prodAceSeq
   INDEX empLogin
         prodAceResource
         prodAceShift
         prodAceEmployee
         prodAceStartDate
         prodAceStartTime
         prodAceEndDate
         prodAceEndTime
         .

DEFINE BUFFER buffProdAce FOR ttblProdAce.

DEFINE STREAM sProdAce.
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
&Scoped-Define ENABLED-OBJECTS btnExport btnImport selectedShift ~
selectedStartDate selectedEndDate btnPost btnNonPost btnCancel lvProdAceDir ~
lvProdAceBlankEmployee lvRunComplete lvResourceList btnSave btnReset ~
btnCancel-2 
&Scoped-Define DISPLAYED-OBJECTS selectedShift selectedStartDate ~
selectedEndDate lvProdAceDir lvProdAceType lvEmpLogin ~
lvProdAceBlankEmployee lvRunComplete lvResourceList 

/* Custom List Definitions                                              */
/* ProdAceDatValues,List-2,List-3,List-4,List-5,List-6                  */
&Scoped-define ProdAceDatValues lvProdAceDir lvProdAceType lvEmpLogin ~
lvProdAceBlankEmployee lvRunComplete lvResourceList 

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

DEFINE BUTTON btnExport 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 7.2 BY 1.71 TOOLTIP "Export to Procduction Ace".

DEFINE BUTTON btnImport 
     IMAGE-UP FILE "Graphics/32x32/import.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Import" 
     SIZE 7.2 BY 1.71 TOOLTIP "Import from Procduction Ace".

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

DEFINE VARIABLE lvProdAceBlankEmployee AS CHARACTER FORMAT "X(256)":U 
     LABEL "Blank Employee" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvProdAceDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "MRP Path" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

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

DEFINE VARIABLE selectedEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lvEmpLogin AS CHARACTER INITIAL "ProdAce" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Prod Ace", "ProdAce",
"Touch Screen", "Touch Screen"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE lvProdAceType AS CHARACTER INITIAL "Summary" 
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

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.8 BY 2.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExport AT ROW 6.24 COL 35 HELP
          "Export to Production Ace" WIDGET-ID 4
     btnImport AT ROW 6.24 COL 42 HELP
          "Import from Production Ace" WIDGET-ID 6
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
     lvProdAceDir AT ROW 8.86 COL 7
     lvProdAceType AT ROW 10.05 COL 19 NO-LABEL
     lvEmpLogin AT ROW 11.24 COL 19 NO-LABEL
     lvProdAceBlankEmployee AT ROW 12.43 COL 16 COLON-ALIGNED
     lvRunComplete AT ROW 13.62 COL 16 COLON-ALIGNED
     lvResourceList AT ROW 14.81 COL 16 COLON-ALIGNED
     btnSave AT ROW 16.24 COL 33 HELP
          "Click to Save"
     btnReset AT ROW 16.24 COL 39 HELP
          "Click to Reset Values"
     btnCancel-2 AT ROW 16.24 COL 45 HELP
          "Click to Cancel and Exit"
     "Type:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 10.05 COL 12
     "Select Shift to Post ... Enter Date Range" VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 1.24 COL 2
          FONT 6
     "Employee Login:" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 11.24 COL 2
     RECT-1 AT ROW 8.38 COL 1
     RECT-2 AT ROW 6 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.8 BY 16.67.


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
         TITLE              = "Production Ace DMI"
         HEIGHT             = 16.67
         WIDTH              = 50.8
         MAX-HEIGHT         = 25.76
         MAX-WIDTH          = 82.2
         VIRTUAL-HEIGHT     = 25.76
         VIRTUAL-WIDTH      = 82.2
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
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lvProdAceBlankEmployee IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvProdAceDir IN FRAME DEFAULT-FRAME
   ALIGN-L 1                                                            */
/* SETTINGS FOR RADIO-SET lvProdAceType IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lvResourceList IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvRunComplete IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Ace DMI */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production Ace DMI */
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
    lvPostProdAce = NO
    .
  RUN postProdAce.
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
    lvPostProdAce = YES
    .
  RUN postProdAce.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME
DO:
  RUN getProdAceDatValues.
  DISPLAY {&prodAceDatValues} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&prodAceDatValues}.
  RUN saveProdAceDatValues.
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
  RUN getProdAceDatValues.
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
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceSeq:
    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
      ASSIGN
        lvProdAceRunQty = 0
        lvProdAceRejectQty = 0
        .
      FOR EACH machtran NO-LOCK
          WHERE machtran.company EQ job-mch.company
            AND machtran.machine EQ ttblProdAce.prodAceResource
            AND machtran.job_number EQ job-mch.job-no
            AND machtran.job_sub EQ job-mch.job-no2
            AND machtran.form_number EQ job-mch.frm
            AND machtran.blank_number EQ job-mch.blank-no
            AND machtran.pass_sequence EQ job-mch.pass
            AND machtran.jobseq EQ 0:
        ASSIGN
          lvProdAceRunQty = lvProdAceRunQty + machtran.run_qty
          lvProdAceRejectQty = lvProdAceRejectQty + machtran.waste_qty
          .
      END. /* each machtran */
    END. /* first-of job */

    /* IF lvProdAceRunQty LE ttblProdAce.prodAceRunQty THEN */
    ASSIGN
      ttblProdAce.prodAceTranRunQty = ttblProdAce.prodAceRunQty - lvProdAceRunQty
      ttblProdAce.prodAceTranRejectQty = ttblProdAce.prodAceRejectQty - lvProdAceRejectQty
      .
    ASSIGN
      lvProdAceRunQty = ttblProdAce.prodAceRunQty
      lvProdAceRejectQty = ttblProdAce.prodAceRejectQty
      .
  END. /* each ttblprodAce */

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
         AND machtran.machine EQ ttblProdAce.prodAceResource
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
  DEFINE PARAMETER BUFFER ttblProdAce FOR ttblProdAce.
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE OUTPUT PARAMETER opEmpLoginRowID AS ROWID NO-UNDO.

  IF ttblProdAce.prodAceEmployee EQ '' OR
     NUM-ENTRIES(ttblProdAce.prodAceEmployee,' ') LT 2 THEN RETURN.
  ASSIGN
    lvProdAceFirstName = ENTRY(1,ttblProdAce.prodAceEmployee,' ')
    lvProdAceLastName = ENTRY(2,ttblProdAce.prodAceEmployee,' ')
    .
  FIND FIRST employee NO-LOCK
       WHERE employee.last_name EQ lvProdAceLastName
         AND employee.first_name EQ lvProdAceFirstName
       NO-ERROR.
  IF AVAILABLE employee THEN DO:
    IF CAN-FIND(FIRST emplogin
                WHERE emplogin.company EQ employee.company
                  AND emplogin.employee EQ employee.employee
                  AND emplogin.start_date EQ machtran.start_date
                  AND emplogin.start_time EQ machtran.start_time
                  AND emplogin.machine EQ ttblProdAce.prodAceResource) THEN
    ASSIGN
      machtran.start_time = machtran.start_time + 1
      machtran.total_time = machtran.total_time + 1
      .
    CREATE empLogin.
    ASSIGN
      emplogin.company = employee.company
      emplogin.employee = employee.employee
      emplogin.machine = ttblProdAce.prodAceResource
      emplogin.start_date = machtran.start_date
      emplogin.start_time = machtran.start_time
      emplogin.end_date = machtran.end_date
      emplogin.end_time = machtran.end_time
      emplogin.total_time = machtran.total_time
      emplogin.shift = ttblProdAce.prodAceShift
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtblProdAce C-Win 
PROCEDURE createTtblProdAce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipProdAceFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.

  EMPTY TEMP-TABLE ttblProdAce.
  INPUT STREAM sProdAce FROM VALUE(ipProdAceFile).
  IF lvPostProdAce THEN DO:
    OUTPUT STREAM sHold TO VALUE(lvHoldFile).
    OUTPUT STREAM sProcessed TO VALUE(lvProcessed).
  END. /* if lvpostprodAce */
  REPEAT:
    IMPORT STREAM sProdAce UNFORMATTED lvProdAceData.
    IF lvResourceList NE '' AND NOT CAN-DO(lvResourceList,ENTRY(1,lvProdAceData,'|')) THEN NEXT.
    ASSIGN
      lvProdAceStart = ENTRY(5,lvProdAceData,'|')
      lvProdAceStart = SUBSTR(lvProdAceStart,1,10)
      lvStartDate = DATE(INT(ENTRY(2,lvProdAceStart,'-')),
                         INT(ENTRY(3,lvProdAceStart,'-')),
                         INT(ENTRY(1,lvProdAceStart,'-')))
      lvProdAceEnd = ENTRY(6,lvProdAceData,'|')
      lvProdAceEnd = SUBSTR(lvProdAceEnd,1,10)
      lvEndDate = DATE(INT(ENTRY(2,lvProdAceEnd,'-')),
                       INT(ENTRY(3,lvProdAceEnd,'-')),
                       INT(ENTRY(1,lvProdAceEnd,'-')))
      .
    IF lvPostProdAce AND
      ((selectedShift NE 'All' AND
        selectedShift NE ENTRY(13,lvProdAceData,'|')) OR
        lvStartDate LT selectedStartDate OR
        lvEndDate GT selectedEndDate) THEN DO:
      PUT STREAM sHold UNFORMATTED lvProdAceData SKIP.
      NEXT.
    END. /* if prodAceshift ne */
    ASSIGN
      lvProdAceResource = ENTRY(1,lvProdAceData,'|')
      lvProdAceJob = LEFT-TRIM(ENTRY(2,lvProdAceData,'|'))
      lvProdAceEmployee = IF ENTRY(12,lvProdAceData,'|') EQ '' THEN lvProdAceBlankEmployee
                        ELSE ENTRY(12,lvProdAceData,'|')
      .
    /* scan failed and created null transactions, save them and try to fix later */
    IF lvProdAceJob NE 'null' THEN
    ASSIGN
      lvProdAceRun = ENTRY(2,lvProdAceJob,'-')
      lvProdAceForm = ENTRY(3,lvProdAceJob,'-')
      lvProdAceJob = ENTRY(1,lvProdAceJob,'-')
      lvProdAceJob = lvProdAceJob + '-'
                 + STRING(INT(lvProdAceRun)) + '.'
                 + STRING(INT(lvProdAceForm))
      .
    IF lvProdAceJob NE 'null' AND
       NOT CAN-FIND(FIRST ttblJob
                    WHERE ttblJob.resource EQ lvProdAceResource
                      AND ttblJob.job EQ lvProdAceJob) AND
       NOT CAN-FIND(FIRST pendingJob
                    WHERE pendingJob.resource EQ lvProdAceResource
                      AND pendingJob.job EQ lvProdAceJob) THEN DO:
      PUT STREAM sError UNFORMATTED lvProdAceData SKIP.
      NEXT.
    END. /* cannot find job in SB */
    CREATE ttblProdAce.
    ASSIGN
      ttblProdAce.prodAceResource = lvProdAceResource
      ttblProdAce.prodAceJob = lvProdAceJob
      ttblProdAce.prodAceItem = ENTRY(3,lvProdAceData,'|')
      ttblProdAce.prodAceSeq = INT(ENTRY(4,lvProdAceData,'|'))
      lvProdAceStart = ENTRY(5,lvProdAceData,'|')
      lvProdAceStart = SUBSTR(lvProdAceStart,1,10)
      ttblProdAce.prodAceStartDate = DATE(INT(ENTRY(2,lvProdAceStart,'-')),
                                      INT(ENTRY(3,lvProdAceStart,'-')),
                                      INT(ENTRY(1,lvProdAceStart,'-')))
      lvProdAceStart = ENTRY(5,lvProdAceData,'|')
      lvProdAceStart = SUBSTR(lvProdAceStart,12,8)
      ttblProdAce.prodAceStartTime = INT(ENTRY(1,lvProdAceStart,':')) * 3600
                               + INT(ENTRY(2,lvProdAceStart,':')) * 60
                               + INT(ENTRY(3,lvProdAceStart,':'))
      lvProdAceEnd = ENTRY(6,lvProdAceData,'|')
      lvProdAceEnd = SUBSTR(lvProdAceEnd,1,10)
      ttblProdAce.prodAceEndDate = DATE(INT(ENTRY(2,lvProdAceEnd,'-')),
                                    INT(ENTRY(3,lvProdAceEnd,'-')),
                                    INT(ENTRY(1,lvProdAceEnd,'-')))
      lvProdAceEnd = ENTRY(6,lvProdAceData,'|')
      lvProdAceEnd = SUBSTR(lvProdAceEnd,12,8)
      ttblProdAce.prodAceEndTime = INT(ENTRY(1,lvProdAceEnd,':')) * 3600
                             + INT(ENTRY(2,lvProdAceEnd,':')) * 60
                             + INT(ENTRY(3,lvProdAceEnd,':'))
      ttblProdAce.prodAceDuration = DEC(ENTRY(7,lvProdAceData,'|'))
      ttblProdAce.prodAceReason = ENTRY(8,lvProdAceData,'|')
      lvProdAceState = ENTRY(9,lvProdAceData,'|')
      lvProdAceState = REPLACE(lvProdAceState,'_enum','')
      ttblProdAce.prodAceState# = IF CAN-DO('run,down',lvProdAceState) THEN 2
                         ELSE IF lvProdAceState EQ 'setup' THEN 1
                         ELSE 3
      ttblProdAce.prodAceState = IF CAN-DO('run,down',lvProdAceState) THEN 'RUN'
                        ELSE IF lvProdAceState EQ 'setup' THEN 'MR'
                        ELSE 'NC'
      ttblProdAce.prodAceEmployee = lvProdAceEmployee
      ttblProdAce.prodAceShift = ENTRY(1,ENTRY(13,lvProdAceData,'|'),' ')
      ttblProdAce.prodAceShift = STRING(LOOKUP(ttblProdAce.prodAceShift,lvShifts))
      .
    IF lvProdAceJob NE 'null' THEN
    ASSIGN
      ttblProdAce.prodAceRunQty = INT(ENTRY(10,lvProdAceData,'|'))
      ttblProdAce.prodAceRejectQty = INT(ENTRY(11,lvProdAceData,'|'))
      .
    /* cannot have a midnight ending time */
    IF ttblProdAce.prodAceEndTime EQ 0 THEN
    ttblProdAce.prodAceEndTime = 86340.
    /* scan failed and created null transactions, check if they belong to this one */
    FOR EACH buffProdAce
        WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
          AND buffProdAce.prodAceJob EQ 'null'
          AND buffProdAce.prodAceSeq LT ttblProdAce.prodAceSeq
        :
      ASSIGN
        buffProdAce.prodAceJob = ttblProdAce.prodAceJob
        buffProdAce.prodAceItem = ttblProdAce.prodAceItem
        buffProdAce.prodAceRunQty = ttblProdAce.prodAceRunQty
        buffProdAce.prodAceRejectQty = ttblProdAce.prodAceRejectQty
        .
    END. /* each buffprodAce */
    IF lvPostProdAce THEN
    PUT STREAM sProcessed UNFORMATTED lvProdAceData SKIP.
  END. /* repeat */
  IF lvPostProdAce THEN DO:
    OUTPUT STREAM sHold CLOSE.
    OUTPUT STREAM sProcessed CLOSE.
  END. /* if lvpostprodAce */
  INPUT STREAM sProdAce CLOSE.

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
  DISPLAY selectedShift selectedStartDate selectedEndDate lvProdAceDir 
          lvProdAceType lvEmpLogin lvProdAceBlankEmployee lvRunComplete 
          lvResourceList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExport btnImport selectedShift selectedStartDate selectedEndDate 
         btnPost btnNonPost btnCancel lvProdAceDir lvProdAceBlankEmployee 
         lvRunComplete lvResourceList btnSave btnReset btnCancel-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getProdAceDatValues C-Win 
PROCEDURE getProdAceDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(lvProdAceDat) NO-ECHO.
  IMPORT UNFORMATTED lvProdAceDir.           /* location of prodAce trans file   */
  IMPORT UNFORMATTED lvProdAceType.          /* Summary or Detail              */
  IMPORT UNFORMATTED lvEmpLogin.             /* ProdAce or TS                    */
  IMPORT UNFORMATTED lvProdAceBlankEmployee. /* default employee if blank      */
  IMPORT UNFORMATTED lvRunComplete.          /* value to indicate run complete */
  IMPORT UNFORMATTED lvResourceList.         /* comma delimited list, or blank */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postProdAce C-Win 
PROCEDURE postProdAce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM OS-DIR(lvProdAceDir) NO-ECHO.
  REPEAT:
    SET lvProdAceFile ^ lvAttrList.
    IF lvAttrList NE 'f' OR INDEX(lvProdAceFile,'.psv') EQ 0 THEN NEXT.
    ASSIGN
      lvFile = lvProdAceDir + '/' + lvProdAceFile
      lvTemp = REPLACE(lvFile,'.psv','.tmp')
      lvHoldFile = REPLACE(lvFile,'.psv','.hold')
      lvProcessed = lvProdAceDir + '/processed/'
                  + REPLACE(lvProdAceFile,'.psv','.'
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
    IF lvPostProdAce THEN DO:
      OS-COPY VALUE(lvFile) VALUE(lvArchive).
      OS-RENAME VALUE(lvFile) VALUE(lvTemp).
    END.
    ELSE OS-APPEND VALUE(lvFile) VALUE(lvTemp).

    /* append hold records */
    OS-APPEND VALUE(lvHoldFile) VALUE(lvTemp).
    
    /* create temp-table prodAce records */
    RUN createTtblProdAce (lvTemp).
    
    /* post to create machtran, or simply to reflect onto SB */
    IF lvPostProdAce THEN DO:
      /* calculate qty values for each transaction */
      RUN calcQtyPerTrans.
      /* run Detail or Summary */
      RUN VALUE('prodAce' + lvProdAceType).
    END. /* if lvportprodAce */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prodAceDetail C-Win 
PROCEDURE prodAceDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* not implemented ...
  
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceSeq:
    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */
    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblProdAce.prodAceResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblProdAce.prodAceState
      machtran.completed = ttblProdAce.prodAceReason EQ lvRunComplete
      machtran.start_date = ttblProdAce.prodAceStartDate
      machtran.start_time = ttblProdAce.prodAceStartTime
      machtran.end_date = ttblProdAce.prodAceEndDate
      machtran.end_time = ttblProdAce.prodAceEndTime
      machtran.run_qty = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty = ttblProdAce.prodAceTranRejectQty
      machtran.shift = ttblProdAce.prodAceShift
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
    IF ttblProdAce.prodAceReason EQ lvRunComplete THEN
    RUN completeMR.
  END. /* each ttblProdAce */
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prodAceSummary C-Win 
PROCEDURE prodAceSummary :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE empLoginRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE lvState AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvTime AS INTEGER NO-UNDO.

  /* Pass 1: consolidate prodAce transactions */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceEmployee
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift) OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
        buffProdAce.prodAceEndDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceEndTime = ttblProdAce.prodAceEndTime
        ttblProdAce.deleteFlag = YES
        .
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */

  /* Pass 2: move non-RUN qty values to RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState NE 'RUN'
       AND  ttblProdAce.deleteFlag EQ NO
       AND (ttblProdAce.prodAceTranRunQty NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND FIRST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceEmployee EQ ttblProdAce.prodAceEmployee
           AND buffProdAce.prodAceState EQ 'RUN'
           AND ttblProdAce.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */

  /* Pass 3: flag records as run completed if necessary */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState EQ 'RUN'
        AND ttblProdAce.prodAceReason EQ lvRunComplete
        AND ttblProdAce.deleteFlag EQ YES
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceEmployee
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceEmployee EQ ttblProdAce.prodAceEmployee
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState EQ 'RUN'
           AND buffProdAce.prodAceReason NE lvRunComplete
           AND buffProdAce.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    buffProdAce.prodAceReason = lvRunComplete.
  END. /* each ttblprodAce */
  
  /* Pass 4: remove deleted flag records */
  FOR EACH ttblProdAce:
    IF ttblProdAce.deleteFlag EQ YES OR
      (ttblProdAce.prodAceState EQ 'NC' AND
       ttblProdAce.prodAceDuration LE 60 AND
       ttblProdAce.prodAceTranRunQty EQ 0 AND
       ttblProdAce.prodAceTranRejectQty EQ 0 AND
       ttblProdAce.prodAceReason NE lvRunComplete) OR
       ttblProdAce.prodAceJob EQ 'null' THEN
    DELETE ttblProdAce.
  END. /* each ttblprodAce */

  /* Pass 5: split records that span midnight */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceEmployee
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.
    IF ttblProdAce.prodAceStartDate NE ttblProdAce.prodAceEndDate THEN DO:
      CREATE buffProdAce.
      BUFFER-COPY ttblProdAce TO buffProdAce.
      ASSIGN
        buffProdAce.prodAceStartDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceStartTime = 0
        buffProdAce.prodAceDuration = buffProdAce.prodAceEndTime - buffProdAce.prodAceStartTime
        ttblProdAce.prodAceEndDate = ttblProdAce.prodAceStartDate
        ttblProdAce.prodAceEndTime = 86340
        ttblProdAce.prodAceDuration = ttblProdAce.prodAceEndTime - ttblProdAce.prodAceStartTime
        ttblProdAce.prodAceTranRunQty = 0
        ttblProdAce.prodAceTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblProdAce */

  /* Pass 6: consolidate prodAce transactions again */
  RELEASE buffProdAce.
  lvState = ''.
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceEmployee
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift) OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
        buffProdAce.prodAceEndDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceEndTime = ttblProdAce.prodAceEndTime
        buffProdAce.prodAceReason = ttblProdAce.prodAceReason
        ttblProdAce.deleteFlag = YES
        .
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */
  
  /* Pass 7: move RUN qty values to last RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState EQ 'RUN'
       AND  ttblProdAce.deleteFlag EQ NO
       AND (ttblProdAce.prodAceTranRunQty NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceEmployee EQ ttblProdAce.prodAceEmployee
           AND buffProdAce.prodAceState EQ 'RUN'
           AND buffProdAce.deleteFlag EQ NO
           AND buffProdAce.prodAceSeq GT ttblProdAce.prodAceSeq
           AND ROWID(buffProdAce) NE ROWID(ttblProdAce)
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */
  
  /* Pass 8: create machtran records */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceEmployee
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.

    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      lvTime = ?.
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */

    IF lvTime EQ ? THEN
    lvTime = ttblProdAce.prodAceStartTime.
    
    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblProdAce.prodAceResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblProdAce.prodAceState
      machtran.completed = ttblProdAce.prodAceReason EQ lvRunComplete
      machtran.start_date = ttblProdAce.prodAceStartDate
      machtran.start_time = ttblProdAce.prodAceStartTime
      machtran.end_date = ttblProdAce.prodAceEndDate
      machtran.end_time = ttblProdAce.prodAceEndTime
      machtran.run_qty = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty = ttblProdAce.prodAceTranRejectQty
      machtran.shift = ttblProdAce.prodAceShift
      machtran.total_time = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.
    
    IF lvEmpLogin EQ 'ProdAce' THEN
    RUN createEmpLogin (BUFFER ttblProdAce,BUFFER machtran,OUTPUT empLoginRowID).
    RUN setRecKey (BUFFER machtran).
    RUN createMachEmp (BUFFER machtran, INPUT empLoginRowID).
  END. /* each ttblProdAce */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveProdAceDatValues C-Win 
PROCEDURE saveProdAceDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(lvProdAceDat).
  PUT UNFORMATTED lvProdAceDir SKIP.           /* location of prodAce trans file */
  PUT UNFORMATTED lvProdAceType SKIP.          /* Summary or Detail              */
  PUT UNFORMATTED lvEmpLogin SKIP.             /* ProdAce or TS                  */
  PUT UNFORMATTED lvProdAceBlankEmployee SKIP. /* default employee if blank      */
  PUT UNFORMATTED lvRunComplete SKIP.          /* value to indicate run complete */
  PUT UNFORMATTED lvResourceList SKIP.         /* comma delimited list, or blank */
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

  CREATE rec_key.
  ASSIGN
    machtran.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
    rec_key.rec_key = machtran.rec_key
    rec_key.table_name = "machtran"
    .

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
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceSeq
      :
    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN DO:
        IF ttblJob.jobLocked THEN NEXT.
      END. /* avail ttbljob */
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        CREATE ttblJob.
        BUFFER-COPY pendingJob TO ttblJob.
        DELETE pendingJob.
      END. /* not avail ttbljob */
      ASSIGN
        ttblJob.startDate = ttblProdAce.prodAceStartDate
        ttblJob.startTime = ttblProdAce.prodAceStartTime
        .
      RUN calcEnd (ttblJob.startDate,ttblJob.startTime,0,ttblJob.timeSpan,
                   OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
    END. /* first-of job */
  END. /* each ttblprodAce */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

