&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME wOperationsTester
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wOperationsTester 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
DEFINE VARIABLE cEstNum AS CHARACTER NO-UNDO.
DEFINE VARIABLE ghSession        AS HANDLE NO-UNDO.
DEFINE VARIABLE ghOperationProcs AS HANDLE NO-UNDO.
RUN est\OperationProcs.p PERSISTENT SET ghOperationProcs.
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghOperationProcs).

{src/adm2/widgetprto.i}

{est\ttOperationAttribute.i}
{est\OperationProcsTT.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME wOperationsTester
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAttribute ttOperation

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttAttribute.attributeID ttAttribute.attributeName ttAttribute.attributeValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 ttAttribute.attributeValue   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 ttAttribute
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 ttAttribute
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttAttribute
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttAttribute
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttAttribute


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttOperation.estimateNo ttOperation.operationID ttOperation.departmentID[1] ttOperation.hoursRun ttOperation.hoursSetup ttOperation.speed ttOperation.quantityIn ttOperation.quantityInSetupWaste ttOperation.quantityInRunWastePercent ttOperation.quantityInRunWaste ttOperation.quantityOut ttOperation.crewSizeRun ttOperation.crewSizeSetup ttOperation.costTotal ttOperation.feedType ttOperation.outputType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttOperation
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttOperation.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttOperation
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttOperation


/* Definitions for DIALOG-BOX wOperationsTester                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-wOperationsTester ~
    ~{&OPEN-QUERY-BROWSE-1}~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS company btnReset btnDon JobNo formNo blankNo ~
Pass operationID btnProcessOps fi_FileOps btnTag tb_FileOps BROWSE-4 btnGo ~
fi_file tb_excel BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS company JobNo formNo blankNo Pass ~
operationID fi_FileOps tb_FileOps fi_file tb_excel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fValidateInputs wOperationsTester 
FUNCTION fValidateInputs RETURNS LOGICAL PRIVATE
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDon AUTO-GO 
     LABEL "Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGo 
     LABEL "Get Attributes" 
     SIZE 46 BY 1.14.

DEFINE BUTTON btnProcessOps 
     LABEL "Process Operations" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btnReset 
     LABEL "Reset" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnTag 
     LABEL "View Tag" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE blankNo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Blank" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE company AS CHARACTER FORMAT "X(6)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\OpsTester_Attribute.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE fi_FileOps AS CHARACTER FORMAT "X(35)" INITIAL "c:~\tmp~\OpsTester_Operations.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE formNo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Form" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE JobNo AS CHARACTER FORMAT "X(9)":U INITIAL "400178" 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE operationID AS CHARACTER FORMAT "X(6)":U INITIAL "ZLM" 
     LABEL "Machine Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE Pass AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Pass" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_FileOps AS LOGICAL INITIAL yes 
     LABEL "Export To File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttAttribute SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      ttOperation SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wOperationsTester _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttAttribute.attributeID COLUMN-LABEL 'ID' WIDTH 12  
    ttAttribute.attributeName COLUMN-LABEL 'Name' FORMAT "x(50)" WIDTH 40
    ttAttribute.attributeValue COLUMN-LABEL 'Value' FORMAT "x(50)" WIDTH 40
    ENABLE 
        ttAttribute.attributeValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 133 BY 7.1 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 wOperationsTester _FREEFORM
  QUERY BROWSE-4 DISPLAY
      ttOperation.estimateNo COLUMN-LABEL 'estimateNo' WIDTH 10  
   ttOperation.operationID COLUMN-LABEL 'Machine Code' WIDTH 10
   ttOperation.departmentID[1] COLUMN-LABEL 'Deptt' WIDTH 10
   ttOperation.hoursRun COLUMN-LABEL 'Hrs Run' WIDTH 10
   ttOperation.hoursSetup COLUMN-LABEL 'MR Hrs' WIDTH 10
   ttOperation.speed COLUMN-LABEL 'Speed' WIDTH 10
   ttOperation.quantityIn COLUMN-LABEL 'Qty In' WIDTH 10
   ttOperation.quantityInSetupWaste COLUMN-LABEL 'Qty SetupWaste ' WIDTH 10
   ttOperation.quantityInRunWastePercent COLUMN-LABEL 'RunWaste %' WIDTH 10
   ttOperation.quantityInRunWaste COLUMN-LABEL 'Qty RunWaste' WIDTH 10
   ttOperation.quantityOut COLUMN-LABEL 'Qty Out' WIDTH 10
   ttOperation.crewSizeRun COLUMN-LABEL 'CrewSize Run' WIDTH 10
   ttOperation.crewSizeSetup COLUMN-LABEL 'Crew SizeSetup' WIDTH 10
   ttOperation.costTotal COLUMN-LABEL 'CostTotal' WIDTH 10
   ttOperation.feedType  COLUMN-LABEL 'FeedType' WIDTH 10
   ttOperation.outputType COLUMN-LABEL 'Output Type' WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 136 BY 7 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME wOperationsTester
     company AT ROW 1.24 COL 24 COLON-ALIGNED HELP
          "Enter Company" WIDGET-ID 36
     btnReset AT ROW 1.52 COL 105.6 WIDGET-ID 64
     btnDon AT ROW 1.52 COL 122
     JobNo AT ROW 2.43 COL 24 COLON-ALIGNED HELP
          "Enter Job#" WIDGET-ID 46
     formNo AT ROW 2.43 COL 51 COLON-ALIGNED HELP
          "Enter Estimate" WIDGET-ID 48
     blankNo AT ROW 2.43 COL 63 COLON-ALIGNED HELP
          "Enter Estimate" WIDGET-ID 50
     Pass AT ROW 2.43 COL 75 COLON-ALIGNED HELP
          "Enter Pass" WIDGET-ID 54
     operationID AT ROW 3.86 COL 24 COLON-ALIGNED HELP
          "Enter Machine#" WIDGET-ID 52
     btnProcessOps AT ROW 5.52 COL 5 WIDGET-ID 58
     fi_FileOps AT ROW 5.52 COL 75.6 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 60
     btnTag AT ROW 5.57 COL 127 WIDGET-ID 66
     tb_FileOps AT ROW 5.67 COL 57 RIGHT-ALIGNED WIDGET-ID 62
     BROWSE-4 AT ROW 7.67 COL 5 WIDGET-ID 300
     btnGo AT ROW 16 COL 6 WIDGET-ID 10
     fi_file AT ROW 16 COL 95.4 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 40
     tb_excel AT ROW 16.14 COL 77 RIGHT-ALIGNED WIDGET-ID 42
     BROWSE-1 AT ROW 17.91 COL 5 WIDGET-ID 200
     SPACE(3.99) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Operations Tester"
         DEFAULT-BUTTON btnGo WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wOperationsTester 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX wOperationsTester
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 tb_FileOps wOperationsTester */
/* BROWSE-TAB BROWSE-1 tb_excel wOperationsTester */
ASSIGN 
       FRAME wOperationsTester:SCROLLABLE       = FALSE
       FRAME wOperationsTester:HIDDEN           = TRUE.

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME wOperationsTester     = 
                "parm".

ASSIGN 
       fi_FileOps:PRIVATE-DATA IN FRAME wOperationsTester     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME wOperationsTester
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME wOperationsTester     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_FileOps IN FRAME wOperationsTester
   ALIGN-R                                                              */
ASSIGN 
       tb_FileOps:PRIVATE-DATA IN FRAME wOperationsTester     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttOperation.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX wOperationsTester
/* Query rebuild information for DIALOG-BOX wOperationsTester
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX wOperationsTester */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wOperationsTester
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wOperationsTester wOperationsTester
ON WINDOW-CLOSE OF FRAME wOperationsTester /* Operations Tester */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME blankNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL blankNo wOperationsTester
ON LEAVE OF blankNo IN FRAME wOperationsTester /* Blank */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo wOperationsTester
ON CHOOSE OF btnGo IN FRAME wOperationsTester /* Get Attributes */
DO:
    IF fValidateInputs() THEN          
        RUN pBuildList (company, JobNo, formNo, blankNo,Pass,operationID).
        
    IF tb_excel THEN
        RUN pRunReport (fi_file).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcessOps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcessOps wOperationsTester
ON CHOOSE OF btnProcessOps IN FRAME wOperationsTester /* Process Operations */
DO:
    IF fValidateInputs() THEN
        RUN pRunOperation (company, JobNo, formNo, blankNo,Pass,operationID, OUTPUT cEstNum).
    
    IF tb_FileOps THEN
        RUN pRunReportOpsLocal (fi_FileOps, company, JobNo, formNo, blankNo,Pass,operationID,cEstNum).
        
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset wOperationsTester
ON CHOOSE OF btnReset IN FRAME wOperationsTester /* Reset */
DO:
    EMPTY TEMP-TABLE ttOperation.
    EMPTY TEMP-TABLE ttAttribute.
    {&CLOSE-QUERY-BROWSE-4}
    {&OPEN-QUERY-BROWSE-4}
    {&CLOSE-QUERY-BROWSE-1}
    {&OPEN-QUERY-BROWSE-1}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTag wOperationsTester
ON CHOOSE OF btnTag IN FRAME wOperationsTester /* View Tag */
DO:
    RUN pGetTagInfo(company, JobNo, formNo, blankNo,Pass,operationID).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company wOperationsTester
ON LEAVE OF company IN FRAME wOperationsTester /* Company */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file wOperationsTester
ON LEAVE OF fi_file IN FRAME wOperationsTester /* If Yes, File Name */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_FileOps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_FileOps wOperationsTester
ON LEAVE OF fi_FileOps IN FRAME wOperationsTester /* If Yes, File Name */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME formNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formNo wOperationsTester
ON LEAVE OF formNo IN FRAME wOperationsTester /* Form */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME JobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL JobNo wOperationsTester
ON LEAVE OF JobNo IN FRAME wOperationsTester /* Job */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME operationID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL operationID wOperationsTester
ON LEAVE OF operationID IN FRAME wOperationsTester /* Machine Code */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pass wOperationsTester
ON LEAVE OF Pass IN FRAME wOperationsTester /* Pass */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel wOperationsTester
ON VALUE-CHANGED OF tb_excel IN FRAME wOperationsTester /* Export To Excel? */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_FileOps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_FileOps wOperationsTester
ON VALUE-CHANGED OF tb_FileOps IN FRAME wOperationsTester /* Export To File? */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wOperationsTester 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

RUN est\OperationProcs.p PERSISTENT SET ghOperationProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wOperationsTester  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wOperationsTester  _DEFAULT-DISABLE
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
  HIDE FRAME wOperationsTester.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wOperationsTester  _DEFAULT-ENABLE
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
  DISPLAY company JobNo formNo blankNo Pass operationID fi_FileOps tb_FileOps 
          fi_file tb_excel 
      WITH FRAME wOperationsTester.
  ENABLE company btnReset btnDon JobNo formNo blankNo Pass operationID 
         btnProcessOps fi_FileOps btnTag tb_FileOps BROWSE-4 btnGo fi_file 
         tb_excel BROWSE-1 
      WITH FRAME wOperationsTester.
  VIEW FRAME wOperationsTester.
  {&OPEN-BROWSERS-IN-QUERY-wOperationsTester}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssessSelection wOperationsTester 
PROCEDURE pAssessSelection PRIVATE :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNum AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dOpMRWaste  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpMRHours  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpeed AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpRunSpoil AS DECIMAL   NO-UNDO.
    
    FIND FIRST job NO-LOCK 
        WHERE job.company EQ ipcCompany
        AND job.job-no EQ ipcJobNum
        NO-ERROR.
        
    IF NOT AVAILable job then
    DO:
        MESSAGE 'Job DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
     END. 
    
    FIND FIRST est NO-LOCK 
        WHERE est.company EQ ipcCompany
        AND est.est-no EQ job.est-no
        NO-ERROR.
    IF NOT AVAILABLE est THEN
    DO:
        MESSAGE 'Estimate DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
     END. 
      
    RUN SetAttributes(TABLE ttAttribute).
    RUN GetOperationStandards (est.company, est.loc, ipcOperationID, 
        OUTPUT dOpMRWaste, OUTPUT dOpMRHours, OUTPUT dOpRunSpeed, OUTPUT dOpRunSpoil, OUTPUT lError, OUTPUT cMessage).
            
    MESSAGE "Machine: " ipcOperationID SKIP 
        "MR Waste: " dOpMRWaste SKIP 
        "MR Hours: " dOpMRHours SKIP 
        "Run Speed: " dOpRunSpeed SKIP 
        "Run Spoil: " dOpRunSpoil SKIP(1) 
        "Error: " lError SKIP(4) 
        "Message:: "cMessage
        VIEW-AS ALERT-BOX.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildList wOperationsTester 
PROCEDURE pBuildList PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN GetAttributes (OUTPUT TABLE ttAttribute).
    STATUS DEFAULT "".
    {&CLOSE-QUERY-BROWSE-1}   
    {&OPEN-QUERY-BROWSE-1}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTagInfo wOperationsTester
PROCEDURE pGetTagInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ ipcCompany
        AND mach.m-code  EQ ipcOperationID
        NO-ERROR.
        
    IF NOT AVAILable mach then
    DO:
        MESSAGE 'MACHINE DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    FIND FIRST job NO-LOCK 
        WHERE job.company EQ ipcCompany
        AND job.job-no EQ ipcJobNo
        AND job.job-no2 EQ 0
        NO-ERROR.
           
    IF NOT AVAILable job then
    DO:
        MESSAGE 'JOB DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
    END.
              
    FIND FIRST job-mch NO-LOCK
        WHERE job-mch.company EQ ipcCompany
        AND job-mch.job  EQ job.job
        AND job-mch.m-code  EQ ipcOperationID
        AND job-mch.frm     EQ ipiFormNo
        AND job-mch.blank-no EQ ipiBlankNo
        AND job-mch.dept    EQ mach.dept[1]
        AND job-mch.pass    EQ ipiPass NO-ERROR.

    IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
            WHERE job-mch.company EQ ipcCompany
            AND job-mch.job  EQ job.job
            AND job-mch.m-code  EQ ipcOperationID
            AND job-mch.frm     EQ ipiFormNo
            AND job-mch.blank-no EQ ipiBlankNo
            AND job-mch.pass    EQ ipiPass NO-ERROR.
            
    IF AVAILABLE job-mch THEN
        RUN system/d-TagViewer.w (
            INPUT job-mch.rec_key,
            INPUT "",
            INPUT ""
            ).   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunOperation wOperationsTester 
PROCEDURE pRunOperation :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcEstNo     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError AS CHARACTER NO-UNDO.

    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ ipcCompany
        AND mach.m-code  EQ ipcOperationID
        NO-ERROR.
        
    IF NOT AVAILable mach then
    DO:
        MESSAGE 'MACHINE DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
    END.
        
    FIND FIRST job NO-LOCK 
        WHERE job.company EQ ipcCompany
        AND job.job-no EQ ipcJobNo
        AND job.job-no2 EQ 0
        NO-ERROR.
            
    IF NOT AVAILABLE job THEN
    DO:
        MESSAGE 'JOB DOES NOT EXIST - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    RUN ProcessOperationChange (job.company,operationID, job.job, ipiFormNo,ipiBlankNo,ipiPass,INPUT mach.dept[1], OUTPUT cAction).
    
    IF cAction = "" THEN
    DO:
        MESSAGE 'No Change:: Job-Mch already exists for ' + operationID + "." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    RUN pGetOperationTT ( OUTPUT TABLE ttOperation).
    
    {&CLOSE-QUERY-BROWSE-4}   
    {&OPEN-QUERY-BROWSE-4}
    
    ASSIGN
        opcEstNo = job.est-no.
        
    /* **************DEV only for Debug************
        IF cError NE  "" THEN
        MESSAGE "ERROR::" skip cError VIEW-AS ALERT-BOX.
        */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunReport wOperationsTester 
PROCEDURE pRunReport PRIVATE :
/*------------------------------------------------------------------------------
                  Purpose:     
                  Parameters:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE hdOutput    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
  
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    ASSIGN 
        hdTempTable = TEMP-TABLE ttAttribute:HANDLE.
    
    RUN Output_TempTableToCSV IN hdOutput (hdTempTable, ipcFile, YES, INPUT TRUE /* Auto increment File name */, OUTPUT lSuccess, OUTPUT cMessage).
  
        
END PROCEDURE.

PROCEDURE pRunReportOpsLocal PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iExt AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFld AS INTEGER NO-UNDO.
    DEFINE VARIABLE lFirst AS LOGICAL NO-UNDO.
    DEFINE VARIABLE hbfTT AS HANDLE NO-UNDO.
  
    OUTPUT to VALUE(ipcFile).
    hbfTT = BUFFER ttOperation:HANDLE.
        
    DO iFld = 1 TO hbfTT:NUM-FIELDS:
                        
        IF hbfTT:BUFFER-FIELD (iFld):EXTENT > 0 THEN
        DO iExt = 1 TO hbfTT:BUFFER-FIELD (iFld):EXTENT:
            PUT UNFORMATTED hbfTT:BUFFER-FIELD (iFld):name + "[" + STRING(iExt) + "]" + ", " .
        END.
        ELSE
            PUT UNFORMATTED  hbfTT:BUFFER-FIELD (iFld):name + ", " .
    END.
    PUT UNFORMATTED SKIP.
    
    PUT UNFORMATTED  "New Record" SKIP.
        
    FOR EACH ttOperation
        BY ttOperation.sequenceOfOperation:
                
        EXPORT DELIMITER "," 
            ttOperation.
    END.
    
    PUT UNFORMATTED SKIP(2).
    PUT UNFORMATTED  "Original Record" SKIP.
    
    FOR EACH estCostOperation
        WHERE estCostOperation.company    = ipcCompany
        AND estCostOperation.estimateNo = ipcEstNo
        AND estCostOperation.formNo = ipiFormNo
        AND estCostOperation.blankNo = ipiBlankNo
        BY estCostOperation.sequenceOfOperation :
        EXPORT DELIMITER "," 
            estCostOperation.
    END.
    OUTPUT CLOSE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateInputs wOperationsTester 
PROCEDURE pValidateInputs PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fValidateInputs wOperationsTester 
FUNCTION fValidateInputs RETURNS LOGICAL PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
        
    lAllValid = YES.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
             company fi_file jobno blankNo formNo pass operationID.
        //estimateID = FILL(" ",8 - LENGTH(TRIM(estimateID))) + TRIM(estimateID). 
        
    END.
    RETURN lAllValid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

