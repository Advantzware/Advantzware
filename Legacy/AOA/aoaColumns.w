&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: AOA/aoaColumns.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author:  Ron Stark
  Created: 12.5.2018

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

DEFINE VARIABLE aoaCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaProgramID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaUserID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle         AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaColumns       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hAppSrv          AS HANDLE    NO-UNDO.
DEFINE VARIABLE char-hdl         AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iHeightPixels    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWidthPixels     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseDefault      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rUserPrintRowID  AS ROWID     NO-UNDO.

DEFINE BUFFER jasperUserPrint FOR user-print.

{AOA/includes/ttColumn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME ttColumn

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttColumn

/* Definitions for BROWSE ttColumn                                      */
&Scoped-define FIELDS-IN-QUERY-ttColumn ttColumn.ttOrder ttColumn.isActive ttColumn.ttLabel ttColumn.isGroup ttColumn.ttGroupLabel ttColumn.ttGroupCalc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttColumn ttColumn.ttOrder   ttColumn.isActive   ttColumn.isGroup   ttColumn.ttGroupLabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttColumn ttColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttColumn ttColumn
&Scoped-define SELF-NAME ttColumn
&Scoped-define QUERY-STRING-ttColumn FOR EACH ttColumn     USE-INDEX ttOrder
&Scoped-define OPEN-QUERY-ttColumn OPEN QUERY {&SELF-NAME} FOR EACH ttColumn     USE-INDEX ttOrder.
&Scoped-define TABLES-IN-QUERY-ttColumn ttColumn
&Scoped-define FIRST-TABLE-IN-QUERY-ttColumn ttColumn


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-ttColumn}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ttColumn 

/* Custom List Definitions                                              */
/* Liat-1,showFields,List-3,List-4,columnObjects,List-6                 */
&Scoped-define showFields svShowAll svShowParameters svShowReportHeader ~
svShowPageHeader svShowGroupHeader svShowReportFooter svShowPageFooter ~
svShowGroupFooter 
&Scoped-define columnObjects btnJasperGroupCalc btnDefault btnMoveDown ~
btnMoveUp 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperFields sObject 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperVariables sObject 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetShowAll sObject 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDefault 
     IMAGE-UP FILE "AOA/images/aoaapply.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&Default" 
     SIZE 4.4 BY 1 TOOLTIP "Reset Selected Columns to Default".

DEFINE BUTTON btnJasperGroupCalc 
     IMAGE-UP FILE "AOA/images/window_dialog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Group:Calculatioins" 
     SIZE 4.4 BY 1 TOOLTIP "Access Group:Calculatioins".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "AOA/images/aoadown.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Down" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "AOA/images/aoaup.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Up" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Up".

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19.4 BY 1.38
     BGCOLOR 15 .

DEFINE VARIABLE svExcelTable AS LOGICAL INITIAL no 
     LABEL "Excel Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL no 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL no 
     LABEL "Group Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL no 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL no 
     LABEL "Page Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL no 
     LABEL "Page Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL no 
     LABEL "Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL no 
     LABEL "Report Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL no 
     LABEL "Report Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttColumn FOR 
      ttColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttColumn sObject _FREEFORM
  QUERY ttColumn DISPLAY
      ttColumn.ttOrder
    ttColumn.isActive VIEW-AS TOGGLE-BOX
    ttColumn.ttLabel
    ttColumn.isGroup  VIEW-AS TOGGLE-BOX
    ttColumn.ttGroupLabel
    ttColumn.ttGroupCalc
ENABLE
    ttColumn.ttOrder
    ttColumn.isActive
    ttColumn.isGroup
    ttColumn.ttGroupLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 148 BY 4.76
         TITLE "Report Columns".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     ttColumn AT ROW 4.33 COL 1 WIDGET-ID 700
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME frameShow
     btnJasperGroupCalc AT ROW 1.57 COL 16.2 HELP
          "Click to Access Group:Calculatioins" WIDGET-ID 80
     svShowAll AT ROW 1.24 COL 23 WIDGET-ID 18
     svShowParameters AT ROW 1.24 COL 39 WIDGET-ID 16
     svShowReportHeader AT ROW 1.24 COL 57 WIDGET-ID 2
     svShowPageHeader AT ROW 1.24 COL 80 WIDGET-ID 6
     svShowGroupHeader AT ROW 1.24 COL 101 WIDGET-ID 10
     svShowReportFooter AT ROW 2.19 COL 57 WIDGET-ID 4
     svShowPageFooter AT ROW 2.19 COL 80 WIDGET-ID 8
     svShowGroupFooter AT ROW 2.19 COL 101 WIDGET-ID 12
     svExcelTable AT ROW 2.19 COL 134 WIDGET-ID 20
     btnDefault AT ROW 1.57 COL 11.8 HELP
          "Reset Selected Columns to Default" WIDGET-ID 76
     btnMoveDown AT ROW 1.57 COL 7.4 HELP
          "Move Selected Column Down" WIDGET-ID 62
     btnMoveUp AT ROW 1.57 COL 3 HELP
          "Move Selected Column Up" WIDGET-ID 66
     RECT-2 AT ROW 1.38 COL 2 WIDGET-ID 78
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 3.19
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "Show/Hide Sections" WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 8.19
         WIDTH              = 149.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frameShow:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Size-to-Fit                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frameShow:MOVE-BEFORE-TAB-ITEM (ttColumn:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB ttColumn frameShow FRAME-A */
ASSIGN 
       FRAME FRAME-A:SCROLLABLE       = FALSE.

ASSIGN 
       ttColumn:NUM-LOCKED-COLUMNS IN FRAME FRAME-A     = 3.

/* SETTINGS FOR FRAME frameShow
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON btnDefault IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnJasperGroupCalc IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME frameShow
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX svShowAll IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupHeader IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageHeader IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowParameters IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportHeader IN FRAME frameShow
   2                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frameShow
/* Query rebuild information for FRAME frameShow
     _Query            is NOT OPENED
*/  /* FRAME frameShow */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttColumn
/* Query rebuild information for BROWSE ttColumn
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttColumn
    USE-INDEX ttOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttColumn */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME frameShow
&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault sObject
ON CHOOSE OF btnDefault IN FRAME frameShow /* Default */
DO:
    ASSIGN
        cSelectedColumns = ""
        lUseDefault      = YES
        .
    RUN pCreateTempTableColumn.
    lUseDefault = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJasperGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJasperGroupCalc sObject
ON CHOOSE OF btnJasperGroupCalc IN FRAME frameShow /* Group:Calculatioins */
DO:
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown sObject
ON CHOOSE OF btnMoveDown IN FRAME frameShow /* Move Down */
DO:
  RUN pMoveColumn (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp sObject
ON CHOOSE OF btnMoveUp IN FRAME frameShow /* Move Up */
DO:
  RUN pMoveColumn (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll sObject
ON VALUE-CHANGED OF svShowAll IN FRAME frameShow /* Show ALL */
DO:
  ASSIGN {&SELF-NAME}
      svShowReportHeader = {&SELF-NAME}
      svShowParameters   = {&SELF-NAME}
      svShowPageHeader   = {&SELF-NAME}
      svShowGroupHeader  = {&SELF-NAME}
      svShowGroupFooter  = {&SELF-NAME}
      svShowPageFooter   = {&SELF-NAME}
      svShowReportFooter = {&SELF-NAME}
      .
  DISPLAY {&showFields} WITH FRAME frameShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter sObject
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME frameShow /* Group Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader sObject
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME frameShow /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter sObject
ON VALUE-CHANGED OF svShowPageFooter IN FRAME frameShow /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader sObject
ON VALUE-CHANGED OF svShowPageHeader IN FRAME frameShow /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters sObject
ON VALUE-CHANGED OF svShowParameters IN FRAME frameShow /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter sObject
ON VALUE-CHANGED OF svShowReportFooter IN FRAME frameShow /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader sObject
ON VALUE-CHANGED OF svShowReportHeader IN FRAME frameShow /* Report Header */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} EQ FALSE THEN
    svShowParameters = {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttColumn
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME ttColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttColumn sObject
ON DEFAULT-ACTION OF ttColumn IN FRAME FRAME-A /* Report Columns */
DO:
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

{AOA/includes/aoaProcedures.i}

ON "ENTRY":U OF ttColumn.isActive
DO:
    IF ttColumn.ttField BEGINS "xx" THEN DO:
        APPLY "TAB":U TO ttColumn.isActive IN BROWSE ttColumn.
        RETURN NO-APPLY.
    END. /* if xx */
END.

ON "ENTRY":U OF ttColumn.ttGroupLabel
DO:
    IF ttColumn.isGroup EQ NO THEN DO:
        APPLY "TAB":U TO ttColumn.ttGroupLabel IN BROWSE ttColumn.
        RETURN NO-APPLY.
    END. /* if not a group */
END.

ON "LEAVE":U OF ttColumn.ttOrder
DO:
    ttColumn.ttOrder = INTEGER(ttColumn.ttOrder:INPUT-VALUE IN BROWSE ttColumn). 
    RUN pSetColumnOrder.
END.

ON "RETURN":U OF ttColumn.ttOrder
DO:
    APPLY "LEAVE":U TO SELF.
END.

ON "VALUE-CHANGED":U OF ttColumn.isActive
DO:
    ttColumn.isActive = NOT ttColumn.isActive.
    RUN pSetColumnOrder.
END.

ON "VALUE-CHANGED":U OF ttColumn.isGroup
DO:
    ttColumn.isGroup = NOT ttColumn.isGroup.
    RUN pSetGroupListItems.
END.

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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
  HIDE FRAME FRAME-A.
  HIDE FRAME frameShow.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sObject 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME frameShow:
      ENABLE {&showFields} svExcelTable {&columnObjects}.
  END. /* with frame */
  {methods/run_link.i "CONTAINER" "pGetContainerSize" "(OUTPUT iHeightPixels, OUTPUT iWidthPixels)"}
  RUN pSetWinSize.
  {methods/run_link.i "CONTAINER" "pGetUserPrintRowID" "(OUTPUT rUserPrintRowID)"}
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT aoaCompany)"}
  {methods/run_link.i "CONTAINER" "pGetProgramID" "(OUTPUT aoaProgramID)"}
  {methods/run_link.i "CONTAINER" "pGetUserID" "(OUTPUT aoaUserID)"}
  {methods/run_link.i "CONTAINER" "pGetTitle" "(OUTPUT aoaTitle)"}
  {methods/run_link.i "CONTAINER" "pGetlColumns" "(OUTPUT aoaColumns)"}
  {methods/run_link.i "CONTAINER" "pGetSelectedColumns" "(OUTPUT cSelectedColumns)"}
  {methods/run_link.i "CONTAINER" "pGethAppSrv" "(OUTPUT hAppSrv)"}
  RUN pCreateTempTableColumn.
  RUN pGetUserPrint (rUserPrintRowID).
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelCSV sObject 
PROCEDURE pExcelCSV :
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to Excel CSV Format
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cDynFunc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    
    {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
    {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}
    
    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        ASSIGN
            cExcelFile = "AOA\excel\.keep"
            FILE-INFO:FILE-NAME = cExcelFile
            cExcelFile = FILE-INFO:FULL-PATHNAME
            cExcelFile = REPLACE(cExcelFile,".keep",aoaTitle + " (")
                       + USERID("ASI") + ").csv"
                       .
        IF SEARCH(cExcelFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(cExcelFile)).        
        OUTPUT TO VALUE(cExcelFile).
        
        /* run dynamic function (business subject) */
        ASSIGN
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("ASI"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.
        
        RUN pGetSelectedColumns.

        /* build header row column labels */
        DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
            fieldName = ENTRY(iColumn,cSelectedColumns).
            /* column label */
            IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
            PUT UNFORMATTED hTable:BUFFER-FIELD(fieldName):LABEL + ",".
        END. /* do iColumn */
        IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
        PUT UNFORMATTED SKIP.

        /* scroll returned temp-table records */
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hTable:HANDLE).
        hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME).
        hQuery:QUERY-OPEN.
        hQueryBuf = hQuery:GET-BUFFER-HANDLE(hTable:NAME).
        REPEAT:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
            DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
                ASSIGN
                    fieldName    = ENTRY(iColumn,cSelectedColumns)
                    cBufferValue = hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE()
                    cBufferValue = REPLACE(cBufferValue,",","")
                    cBufferValue = REPLACE(cBufferValue,CHR(10)," ")
                    .
                PUT UNFORMATTED cBufferValue + ",".
            END. /* do iColumn */
            PUT UNFORMATTED SKIP.
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START excel.exe VALUE("~"" + cExcelFile + "~"").
    END. /* valid happsrv */
    
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelXLS sObject 
PROCEDURE pExcelXLS :
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to Excel
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColumns    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE fieldName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iColumn     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iRow        AS INTEGER    NO-UNDO INITIAL 1.
    DEFINE VARIABLE iStatusRow  AS INTEGER    NO-UNDO INITIAL 3.
    DEFINE VARIABLE hQuery      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hQueryBuf   AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cDynFunc    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cExcelFile  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDataType   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFormat     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE chExcel     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chRangeRow  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chRangeCol  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE errorMsg    AS CHARACTER  NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    
    {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
    {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}

    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        ASSIGN
            cExcelFile = "AOA\excel\.keep"
            FILE-INFO:FILE-NAME = cExcelFile
            cExcelFile = FILE-INFO:FULL-PATHNAME
            cExcelFile = REPLACE(cExcelFile,".keep",aoaTitle + " (")
                       + USERID("ASI") + ").xls"
                       .
        IF SEARCH(cExcelFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(cExcelFile)).

        /* Connect to the running Excel session. */
        CREATE "Excel.Application" chExcel CONNECT NO-ERROR.
        /* Start a new session of Excel. */
        IF NOT VALID-HANDLE(chExcel) THEN
        CREATE "Excel.Application" chExcel NO-ERROR.
        /* Check if Excel got initialized. */
        IF NOT VALID-HANDLE(chExcel) THEN DO:
            MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
        /* Open our Excel Template. */
        /* chWorkbook = chExcel:Workbooks:Open(cExcelFile) NO-ERROR. */
        chExcel:Visible = TRUE.
        /* Do not display Excel error messages. */
        chExcel:DisplayAlerts = FALSE NO-ERROR.
        ASSIGN
            chExcel:SheetsInNewWorkbook = 1
            chWorkbook  = chExcel:Workbooks:Add()
            chWorksheet = chWorkbook:Worksheets(1)
            .
        chWorkbook:Worksheets:Add(,chWorksheet).
        RELEASE OBJECT chWorksheet.

        IF svShowParameters THEN DO:
            chWorkbook:Worksheets(2):Activate.
            ASSIGN
                chWorksheet = chWorkbook:Worksheets(2)
                /* Rename the worksheet */
                chWorkSheet:Name = "Parameters"
                /* Disable screen updating so it will go faster */
                chExcel:ScreenUpdating = TRUE
                .
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chWorkSheet:Cells(iRow,1):Value = "Parameter:"
                chWorkSheet:Cells(iRow,1):Font:Bold = TRUE
                chWorkSheet:Cells(iRow,1):Font:Underline = TRUE
                chWorkSheet:Cells(iRow,1):HorizontalAlignment = -4152
                chWorkSheet:Cells(iRow,2):Value = "Value"
                chWorkSheet:Cells(iRow,2):Font:Bold = TRUE
                chWorkSheet:Cells(iRow,2):Font:Underline = TRUE
                iRow = iRow + 1
                .
            DO iColumn = 1 TO EXTENT(user-print.field-name):
                IF user-print.field-name[iColumn] EQ "" THEN LEAVE.
                IF user-print.field-name[iColumn] EQ "svTitle" THEN LEAVE.
                IF INDEX(user-print.field-name[iColumn],"DateOption") EQ 0 THEN DO:
                    /* align left (-4131) or right (-4152) */
                    ASSIGN
                        chWorkSheet:Cells(iRow,1):Value = (IF user-print.field-label[iColumn] NE ? THEN
                                                              user-print.field-label[iColumn] ELSE
                                                              user-print.field-name[iColumn]) + ":"
                        chWorkSheet:Cells(iRow,1):HorizontalAlignment = -4152
                        chWorkSheet:Cells(iRow,2):Value = user-print.field-value[iColumn]
                        chWorkSheet:Cells(iRow,2):HorizontalAlignment = -4131
                        iRow = iRow + 1
                        .
                END. /* not a date option parameter */
                ELSE
                chWorkSheet:Cells(iRow - 1,1):Value = chWorkSheet:Cells(iRow - 1,1):Value
                                                    + " (" + user-print.field-value[iColumn] + ")"
                                                    .
            END. /* do icolumn */
            ASSIGN
                chRangeRow = chWorkSheet:Cells(1,1)
                chRangeCol = chWorkSheet:Cells(iRow,2)
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            /* auto size the columns */
            chExcel:Selection:Columns:AutoFit.
            chWorksheet:Cells(iRow,1):Select.
        END. /* show parameters */
        ELSE /* remove spare worksheet */
        chWorkbook:WorkSheets(2):DELETE NO-ERROR.
        
        /* Select a worksheet */
        chWorkbook:Worksheets(1):Activate.
        ASSIGN
            chWorksheet = chWorkbook:Worksheets(1)
            /* Rename the worksheet */
            chWorkSheet:Name = aoaTitle
            /* Disable screen updating so it will go faster */
            chExcel:ScreenUpdating = TRUE
            iRow = 1
            .
        RUN pGetSelectedColumns.        
        IF svShowReportHeader THEN DO:
            ASSIGN
                chRangeRow = chWorkSheet:Cells(1,1)
                chRangeCol = chWorkSheet:Cells(1,NUM-ENTRIES(cSelectedColumns))
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chRangeRow = chWorkSheet:Cells(2,1)
                chRangeCol = chWorkSheet:Cells(2,NUM-ENTRIES(cSelectedColumns))
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chWorkSheet:Range("A1"):Value = aoaTitle
                chWorkSheet:Range("A1"):Font:Bold = TRUE
                chWorkSheet:Range("A2"):Value = "Created " + STRING(TODAY,"99/99/9999")
                                              + " @ " + STRING(TIME,"hh:mm:ss am")
                iRow = iRow + 2
                iStatusRow = iStatusRow + 2
                .
        END. /* show report title */

        /* run dynamic function (business subject) */
        ASSIGN
            chWorkSheet:Cells(iStatusRow,2):Value = "Running Query..."
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("ASI"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

        /* build header row column labels */
        DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
            ASSIGN
                chWorkSheet:Cells(iStatusRow,2):Value = "Running Query...Done"
                chWorkSheet:Cells(iStatusRow + 2,2):Value = "Formatting Cells..."
                fieldName = ENTRY(iColumn,cSelectedColumns)
                cDataType = hTable:BUFFER-FIELD(fieldName):DATA-TYPE
                /* align left (-4131) or right (-4152) */
                chWorkSheet:Cells(iRow,iColumn):HorizontalAlignment = IF cDataType EQ "Character" THEN -4131
                                                                                                  ELSE -4152
                chRangeRow = chWorkSheet:Cells(iRow,iColumn)
                chRangeCol = chWorkSheet:Cells(65536,iColumn)
                .
            /* column label */
            IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
            chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):LABEL.
            /* apply column format based on data type */
            CASE cDataType:
                WHEN "Character" THEN
                ASSIGN
                    chWorkSheet:Range(chRangeRow,chRangeCol):HorizontalAlignment = -4131
                    chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "General"
                    .
                WHEN "Date" THEN
                chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "mm/dd/yyyy".
                WHEN "Integer" OR WHEN "Decimal" THEN DO:
                    ASSIGN
                        cFormat = hTable:BUFFER-FIELD(fieldName):FORMAT
                        cFormat = REPLACE(cFormat,">","#")
                        cFormat = REPLACE(cFormat,"<","#")
                        cFormat = REPLACE(cFormat,"9","0")
                        .
                    IF INDEX(cFormat,"-") NE 0 THEN
                    ASSIGN
                        cFormat = REPLACE(cFormat,"-","")
                        cFormat = cFormat + "_);[Red](" + cFormat + ")"
                        .
                    chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = cFormat.
                END. /* integer/decimal */
            END CASE.
        END. /* do iColumn */

        IF svShowPageHeader OR aoaType EQ "Dashboard" THEN DO:
            /* bold and underline header row */
            ASSIGN
                chRangeRow = chWorkSheet:Cells(iRow,1)
                chRangeCol = chWorkSheet:Cells(iRow,NUM-ENTRIES(cSelectedColumns))
                chWorkSheet:Range(chRangeRow,chRangeCol):Font:Bold = TRUE
                chWorkSheet:Range(chRangeRow,chRangeCol):Font:Underline = TRUE
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            /* auto size the columns */
            chExcel:Selection:Columns:AutoFit.
            chWorksheet:Cells(iRow + 1,1):Select.
        END.
        ELSE
        iRow = iRow - 1.
        
        ASSIGN
            chWorkSheet:Cells(iStatusRow + 2,2):Value = "Formatting Cells...Done"
            chWorkSheet:Cells(iStatusRow + 4,2):Value = "Building Worksheet..."
            .
        /* pause to let excel display catch up */
        PAUSE 1 NO-MESSAGE.

        /* turn off display to run faster and clear status messages */
        ASSIGN
            chExcel:ScreenUpdating = FALSE
            chWorkSheet:Cells(iStatusRow,2):Value     = ""
            chWorkSheet:Cells(iStatusRow + 2,2):Value = ""
            chWorkSheet:Cells(iStatusRow + 4,2):Value = ""
            .
        
        /* scroll returned temp-table records */
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hTable:HANDLE).
        hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME).
        hQuery:QUERY-OPEN.
        hQueryBuf = hQuery:GET-BUFFER-HANDLE(hTable:NAME).
        REPEAT:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
            iRow = iRow + 1.
            DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
                fieldName = ENTRY(iColumn,cSelectedColumns).
                chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    errorMsg = "".
                    DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        errorMsg = errorMsg + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
                    END. /* do idx */
                    MESSAGE "Row:" iRow "Column:" iColumn SKIP(1)
                        "Field:" fieldName SKIP
                        "Label:" hTable:BUFFER-FIELD(fieldName):LABEL SKIP
                        "Value:" hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE() SKIP(1)
                        "Error:" errorMsg
                            VIEW-AS ALERT-BOX ERROR TITLE "CTRL-BREAK to End".
                END. /* if error-status:error */
            END. /* do iColumn */
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.

        /* calc header and data */
        IF iRow GT 0 THEN
        ASSIGN
            chRangeRow = chWorkSheet:Cells(iStatusRow - 2,1)
            chRangeCol = chWorkSheet:Cells(iRow,NUM-ENTRIES(cSelectedColumns))
            .
        /* put data into a table */
        IF svExcelTable THEN
        ASSIGN
            chWorkSheet:ListObjects:Add(,chWorkSheet:Range(chRangeRow,chRangeCol),,NOT svShowPageHeader):Name = "TableAOA"
            chWorkSheet:ListObjects("TableAOA"):ShowTotals = TRUE
            .
        /* select header and data */
        chWorkSheet:Range(chRangeRow,chRangeCol):Select.
        /* auto size the columns */
        chExcel:Selection:Columns:AutoFit.
        /* select first none header cell */
        chWorksheet:Cells(iStatusRow - 1,1):Select.
        /* enable screen updating */
        chExcel:ScreenUpdating = TRUE.
        /* auto save excel file */
        chExcel:ActiveSheet:SaveAs(cExcelFile).
        
        /* Release created objects. */
        RELEASE OBJECT chWorkbook  NO-ERROR.
        RELEASE OBJECT chWorkSheet NO-ERROR.
        RELEASE OBJECT chExcel     NO-ERROR.
    END. /* valid happsrv */
    
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetJasperUserPrintRowID sObject 
PROCEDURE pGetJasperUserPrintRowID :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprJasperUserPrintRowID AS ROWID NO-UNDO.
    
    oprJasperUserPrintRowID = ROWID(jasperUserPrint).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSelectedColumns sObject 
PROCEDURE pGetSelectedColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColumn AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    cSelectedColumns = "".
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        cSelectedColumns = cSelectedColumns + ttColumn.ttField + ",".
    END. /* each ttColumn */
    cSelectedColumns = TRIM(cSelectedColumns,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrint sObject 
PROCEDURE pGetUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE hFrame AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.

    IF iprRowID NE ? THEN
    FIND user-print NO-LOCK WHERE ROWID(user-print) EQ iprRowID.
    ELSE DO:
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ aoaUserID
               AND user-print.prgmName   EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ "_default"
               AND user-print.prgmName   EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
    END. /* else */
    IF NOT AVAILABLE user-print THEN RETURN.

    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        IF user-print.field-name[idx] EQ "svSelectedColumns" THEN
        cSelectedColumns = user-print.field-value[idx].
    END. /* do idx */

    ASSIGN
        hChild = FRAME frameShow:HANDLE
        hChild = hChild:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND hChild:SENSITIVE AND
           hChild:TYPE NE "BUTTON" THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF TRIM(user-print.field-name[idx]) EQ hChild:NAME THEN DO:
                    hChild:SCREEN-VALUE = user-print.field-value[idx].
                    LEAVE.
                END. /* found screen object */
            END. /* do idx */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    ASSIGN {&showFields}.
    
    RUN pGetJasperUserPrint.
    
    IF CAN-FIND(FIRST ttColumn) THEN
    BROWSE ttColumn:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupCalc sObject 
PROCEDURE pJasperGroupCalc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSave      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttColumn FOR ttColumn.
    
    IF ttColumn.isActive EQ NO AND
       NOT ttColumn.ttField BEGINS "xx" THEN RETURN.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttColumn.ttField
        :
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.ttGroup + ","
                   + ttGroupCalc.ttCalcType + ","
                   .
    END. /* each ttgroupcalc */
    cGroupCalc = TRIM(cGroupCalc,",").
    RUN AOA/jasperGroupCalc.w (
        ttColumn.ttLabel,
        ttColumn.ttField,
        fJasperGroups(),
        fJasperFields(),
        fJasperVariables(),
        INPUT-OUTPUT cGroupCalc,
        OUTPUT lSave
        ).
    IF lSave THEN DO:
        FOR EACH ttGroupCalc
            WHERE ttGroupCalc.ttField EQ ttColumn.ttField
            :
            DELETE ttGroupCalc.
        END. /* each ttgroupcalc */
        IF cGroupCalc NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(cGroupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.ttField    = ttColumn.ttField
                ttGroupCalc.ttGroup    = ENTRY(idx,cGroupCalc)
                ttGroupCalc.ttCalcType = ENTRY(idx + 1,cGroupCalc)
                .
        END. /* do idx */
        ttColumn.ttGroupCalc = fJasperGroupCalc(ttColumn.ttField).
        BROWSE ttColumn:REFRESH() NO-ERROR.
    END. /* if lsave */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveColumn sObject 
PROCEDURE pMoveColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiChangeOrder AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo  AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID   AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttColumn FOR ttColumn.
    
    /* can only move active column */
    IF ttColumn.isActive EQ NO THEN RETURN.
    /* first column, can't move up */
    IF ttColumn.ttOrder EQ 1 AND ipiChangeOrder EQ -1 THEN RETURN.
    /* check if at bottom, can't move down */
    FIND LAST bttColumn USE-INDEX ttOrder
         WHERE bttColumn.isActive EQ YES
         NO-ERROR.
    IF AVAILABLE bttColumn THEN DO:
        /* check if at bottom, can't move down */
        IF bttColumn.ttOrder EQ ttColumn.ttOrder AND ipiChangeOrder EQ 1 THEN
        RETURN.
    END. /* if avail */
    ELSE RETURN.
    ASSIGN
        iCurrent = ttColumn.ttOrder
        iMoveTo  = ttColumn.ttOrder + ipiChangeOrder
        .
    FIND FIRST bttColumn
         WHERE bttColumn.isActive EQ YES
           AND bttColumn.ttOrder  EQ iMoveTo
         NO-ERROR.
    IF AVAILABLE bttColumn THEN DO:
        ASSIGN
            ttColumn.ttOrder  = 0
            bttColumn.ttOrder = iCurrent
            ttColumn.ttOrder  = iMoveTo
            .
    END. /* if avail */
    cSelectedColumns = "".
    FOR EACH bttColumn
        WHERE bttColumn.isActive EQ YES
           BY bttColumn.ttOrder
        :
        cSelectedColumns = cSelectedColumns + bttColumn.ttField + ",".
    END. /* each bttColumn */
    ASSIGN
        cSelectedColumns = TRIM(cSelectedColumns,",")
        rRowID = ROWID(ttColumn)
        .
    {&OPEN-QUERY-ttColumn}
    REPOSITION ttColumn TO ROWID rRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefreshColumnsPage sObject 
PROCEDURE pRefreshColumnsPage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pCreateTempTableColumn.
    RUN pGetUserPrint (rUserPrintRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveJasperUserPrint sObject 
PROCEDURE pSaveJasperUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplBatch AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID AS ROWID   NO-UNDO.
    
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* updating batch values, no need to update jasper values */
    IF iplBatch EQ ? THEN RETURN.
    
    DO TRANSACTION:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE ROWID(user-print) EQ iprRowID
             .
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN
            LEAVE.
        END. /* do idx */
        idx = idx - 1.
        
        /* reserve 9 for show/hide section parameters */
        ASSIGN
            hChild = FRAME frameShow:HANDLE
            hChild = hChild:FIRST-CHILD
            hChild = hChild:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:TYPE NE "RECTANGLE" AND
               hChild:TYPE NE "BUTTON" THEN
            ASSIGN
                idx = idx + 1
                user-print.field-name[idx]  = hChild:NAME
                user-print.field-label[idx] = hChild:LABEL
                user-print.field-value[idx] = hChild:SCREEN-VALUE
                .
            hChild = hChild:NEXT-SIBLING.
        END. /* do while */

        IF iplBatch THEN DO:
            CREATE jasperUserPrint.
            ASSIGN
                jasperUserPrint.company    = user-print.company
                jasperUserPrint.program-id = user-print.program-id
                jasperUserPrint.user-id    = user-print.user-id
                jasperUserPrint.batch      = "Batch"
                jasperUserPrint.prgmName   = "Jasper"
                jasperUserPrint.last-date  = TODAY
                jasperUserPrint.last-time  = TIME
                .
        END. /* if batch */
        ELSE IF NOT iplBatch THEN DO:
            FIND FIRST jasperUserPrint EXCLUSIVE-LOCK
                 WHERE jasperUserPrint.company    EQ user-print.company
                   AND jasperUserPrint.program-id EQ user-print.program-id
                   AND jasperUserPrint.user-id    EQ user-print.user-id
                   AND jasperUserPrint.batch      EQ ""
                   AND jasperUserPrint.prgmName   EQ "Jasper"
                 NO-ERROR.
            IF NOT AVAILABLE jasperUserPrint THEN DO:
                CREATE jasperUserPrint.
                ASSIGN
                    jasperUserPrint.company    = user-print.company
                    jasperUserPrint.program-id = user-print.program-id
                    jasperUserPrint.user-id    = user-print.user-id
                    jasperUserPrint.prgmName   = "Jasper"
                    jasperUserPrint.last-date  = TODAY
                    jasperUserPrint.last-time  = TIME
                    .
            END. /* not avail */
        END. /* not batch, must be view now request */
        ASSIGN
            jasperUserPrint.next-date   = TODAY
            jasperUserPrint.next-time   = TIME
            jasperUserPrint.field-name  = ""
            jasperUserPrint.field-value = ""
            jasperUserPrint.field-label = ""
            idx = 0
            .
        FOR EACH ttColumn
            WHERE (ttColumn.ttGroupCalc NE ""
               OR ttColumn.isGroup EQ YES)
               BY ttColumn.ttOrder
            :
            ASSIGN
                idx = idx + 1
                jasperUserPrint.field-name[idx]  = ttColumn.ttField
                jasperUserPrint.field-label[idx] = STRING(ttColumn.isGroup)
                jasperUserPrint.field-value[idx] = "Label," + ttColumn.ttGroupLabel
                .
            IF ttColumn.ttGroupCalc NE "" THEN
            jasperUserPrint.field-value[idx] = jasperUserPrint.field-value[idx]
                                             + "," + ttColumn.ttGroupCalc
                                             .
        END. /* each ttColumn */
    END. /* do tran */
    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.  
    IF AVAILABLE jasperUserPrint THEN
    FIND CURRENT jasperUserPrint NO-LOCK.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveUserPrint sObject 
PROCEDURE pSaveUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER iprRowID AS ROWID   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiIdx  AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cColumns AS CHARACTER NO-UNDO.
    
    DO TRANSACTION:
        IF aoaColumns THEN DO:
            FIND FIRST user-print EXCLUSIVE-LOCK
                 WHERE ROWID(user-print) EQ iprRowID
                 .
            FOR EACH ttColumn
                WHERE ttColumn.isActive EQ NO
                   BY ttColumn.ttOrder
                :
                cColumns = cColumns + ttColumn.ttField + ",".
            END. /* each ttColumn */
            ASSIGN
                iopiIdx                         = iopiIdx + 1
                user-print.field-name[iopiIdx]  = "svAvailableColumns"
                user-print.field-label[iopiIdx] = ?
                user-print.field-value[iopiIdx] = TRIM(cColumns,",")
                cSelectedColumns = ""
                .
            FOR EACH ttColumn
                WHERE ttColumn.isActive EQ YES
                   BY ttColumn.ttOrder
                :
                cSelectedColumns = cSelectedColumns + ttColumn.ttField + ",".
            END. /* each ttColumn */
            ASSIGN
                cSelectedColumns                = TRIM(cSelectedColumns,",")
                iopiIdx                         = iopiIdx + 1
                user-print.field-name[iopiIdx]  = "svSelectedColumns"
                user-print.field-label[iopiIdx] = ?
                user-print.field-value[iopiIdx] = TRIM(cSelectedColumns,",")
                .
        END. /* aoacolumns */
    END. /* do trans */
    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetColumnOrder sObject 
PROCEDURE pSetColumnOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    
    FOR EACH ttColumn
        :
        ttColumn.ttPending = YES.
    END. /* each ttColumn */
    cSelectedColumns = "".
    FOR EACH ttColumn
        WHERE ttColumn.isActive  EQ YES
          AND ttColumn.ttPending EQ YES
           BY ttColumn.ttOrder
        :
        ASSIGN
            iOrder             = iOrder + 1
            ttColumn.ttOrder   = iOrder
            ttColumn.ttPending = NO
            cSelectedColumns   = cSelectedColumns + ttColumn.ttField + ",".
            .
    END. /* each ttColumn */
    cSelectedColumns = TRIM(cSelectedColumns,",").
    FOR EACH ttColumn
        WHERE ttColumn.isActive  EQ NO
          AND ttColumn.ttPending EQ YES
        :
        ASSIGN
            iOrder             = iOrder + 1
            ttColumn.ttOrder   = iOrder
            ttColumn.ttPending = NO
            .
        IF NOT ttColumn.ttField BEGINS "xx" THEN DO:
            ttColumn.isGroup = NO.
            FOR EACH ttGroupCalc
                WHERE ttGroupCalc.ttField EQ ttColumn.ttField
                :
                DELETE ttGroupCalc.
            END. /* each ttgroupcalc */
        END. /* if not xx */
    END. /* each ttColumn */
    RUN pSetGroupListItems.
    {&OPEN-QUERY-ttColumn}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWinSize sObject 
PROCEDURE pSetWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF iHeightPixels LT 347 THEN
    iHeightPixels = 347.
    IF iWidthPixels  LT 746 THEN
    iWidthPixels  = 746.
    ASSIGN
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS = iHeightPixels
        FRAME {&FRAME-NAME}:WIDTH-PIXELS  = iWidthPixels
        FRAME frameShow:WIDTH-PIXELS      = iWidthPixels - 2
        BROWSE ttColumn:WIDTH-PIXELS      = iWidthPixels - 2
        BROWSE ttColumn:HEIGHT-PIXELS     = iHeightPixels - BROWSE ttColumn:Y - 2
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperFields sObject 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bttColumn FOR ttColumn.
        
    FOR EACH bttColumn
        :
        cFields = cFields + "$F~{" + bttColumn.ttField + "},".
    END. /* each bttColumn */
    RETURN TRIM(cFields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperVariables sObject 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cVariables  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bttColumn FOR ttColumn.
        
    FOR EACH bttColumn
        WHERE bttColumn.isActive    EQ YES
           OR bttColumn.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ bttColumn.ttField
        :
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.ttGroup,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.ttField + "_"
                        + IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.ttGroup + "Footer" 
            cVariables  = cVariables + "$V~{" + cName + "},".
                        .
    END. /* each bttColumn */
    RETURN TRIM (cVariables,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetShowAll sObject 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME frameShow:
        svShowAll = svShowReportHeader AND
                    svShowParameters   AND
                    svShowPageHeader   AND
                    svShowGroupHeader  AND
                    svShowGroupFooter  AND
                    svShowPageFooter   AND
                    svShowReportFooter
                    .
        DISPLAY {&showFields}.
    END. /* do with */
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

