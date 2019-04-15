&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : dynProcs.i
    Purpose     : shared procedures used in dynamic modules

    Syntax      : AOA/includes/dynProcs.i

    Description : Dynamic Procedures

    Author(s)   : Ron Stark
    Created     : 1.25.2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cBufferValue        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBrowseQuery        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCalcColumn         AS HANDLE    NO-UNDO EXTENT 200.
DEFINE VARIABLE hDynCalcField       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynDescripProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynInitProc        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynValProc         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery              AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx                 AS INTEGER   NO-UNDO.

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).
RUN AOA/spDynDescriptionProc.p PERSISTENT SET hDynDescripProc.
RUN AOA/spDynInitializeProc.p  PERSISTENT SET hDynInitProc.
RUN AOA/spDynValidateProc.p    PERSISTENT SET hDynValProc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 

/* ***************************  Main Block  *************************** */

CREATE BROWSE hQueryBrowse
    ASSIGN
        FRAME = FRAME resultsFrame:HANDLE
        SENSITIVE = TRUE
        SEPARATORS = TRUE
        ROW-MARKERS = FALSE
        COLUMN-RESIZABLE = TRUE
        COLUMN-MOVABLE = TRUE 
        ALLOW-COLUMN-SEARCHING = TRUE
        COL = 1
        ROW = 1
        HEIGHT = FRAME resultsFrame:HEIGHT - .1
        WIDTH = FRAME resultsFrame:WIDTH - .32
        VISIBLE = FALSE
        NO-VALIDATE = TRUE
        .
ON ROW-DISPLAY OF hQueryBrowse DO:
    DO idx = 1 TO EXTENT(hCalcColumn):
        IF VALID-HANDLE(hCalcColumn[idx]) AND
           dynParamValue.isCalcField[idx] THEN DO:
            RUN spDynCalcField IN hDynCalcField (
                hBrowseQuery:HANDLE,
                dynParamValue.calcProc[idx],
                dynParamValue.calcParam[idx],
                dynParamValue.dataType[idx],
                dynParamValue.colFormat[idx],
                OUTPUT cBufferValue
                ).
            hCalcColumn[idx]:SCREEN-VALUE = cBufferValue.
        END. /* if valid handle */
    END. /* do idx */
END. /* row-display */

{AOA/includes/pDynParamProcs.i "{1}"}
{AOA/includes/pRunNow.i}
{AOA/includes/pRunBusinessLogic.i}
{AOA/includes/pSetDynParamValue.i "{1}"}

RUN spSetCompany IN hDynDescripProc (g_company).
RUN spSetCompany IN hDynInitProc (g_company).
RUN spSetCompany IN hDynValProc (g_company).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecipients Include
PROCEDURE pGetRecipients:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRecipients AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO idx = 1 TO EXTENT(dynParamValue.paramName):
        IF dynParamValue.paramName[idx]  EQ "svRecipients" AND
           dynParamValue.paramValue[idx] NE "" THEN DO:
            opcRecipients = dynParamValue.paramValue[idx].
            LEAVE.
        END. /* if */
    END. /* do idx */
    IF opcRecipients NE "" THEN DO:
        MESSAGE
            "Recipients:" opcRecipients SKIP(1)
            "Email Results?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lUseEmail AS LOGICAL.
        IF lUseEmail EQ NO THEN
        opcRecipients = "".
    END. /* if */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsBrowser Include 
PROCEDURE pResultsBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DEFINE BUFFER b{1}SubjectColumn FOR {1}SubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    iphQuery:QUERY-CLOSE.
    DO idx = 1 TO hQueryBrowse:NUM-COLUMNS:
        ASSIGN
            hCalcColumn[idx] = ?
            hColumn = hQueryBrowse:GET-BROWSE-COLUMN(idx)
            .
        DELETE OBJECT hColumn.
    END. /* do idx */
    ASSIGN
        hQueryBrowse:TITLE   = dynSubject.subjectTitle
        hQueryBrowse:QUERY   = iphQuery
        hQueryBrowse:VISIBLE = TRUE
        .
    DO idx = 1 TO EXTENT(dynParamValue.colName):
        IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
        IF dynParamValue.isActive[idx] EQ NO THEN NEXT.
        IF dynParamValue.isCalcField[idx] THEN
        ASSIGN
            hCalcColumn[idx] = hQueryBrowse:ADD-CALC-COLUMN(
                dynParamValue.dataType[idx],
                dynParamValue.colFormat[idx],
                "",
                dynParamValue.colLabel[idx]
                )
            hColumn = hCalcColumn[idx]
            .
        ELSE
        ASSIGN
            hColumn = hQueryBrowse:ADD-LIKE-COLUMN(dynParamValue.colName[idx])
            hColumn:LABEL  = dynParamValue.colLabel[idx]
            .
/*        IF idx MOD 2 EQ 0 THEN hColumn:COLUMN-BGCOLOR = 11.*/
        IF dynParamValue.columnSize[idx] NE 0 THEN
        hColumn:WIDTH-CHARS = dynParamValue.columnSize[idx].
    END. /* do idx */
    hBrowseQuery = iphQuery:HANDLE.
    iphQuery:QUERY-OPEN.
    IF iphQuery:NUM-RESULTS GT 0 THEN
    hQueryBrowse:REFRESH().
    DO WITH FRAME resultsFrame:
        ASSIGN
            btnCloseResults:HIDDEN    = NO
            btnSaveResults:HIDDEN     = NO
            FRAME resultsFrame:HIDDEN = NO
            .
        btnCloseResults:MOVE-TO-TOP().
        btnSaveResults:MOVE-TO-TOP().
    END. /* results frame */
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsJasper Include 
PROCEDURE pResultsJasper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    RUN spJasperQuery IN hJasper (
        ipcType,
        ROWID(dynParamValue),
        dynSubject.subjectTitle,
        ipcUserID,
        hAppSrvBin,
        OUTPUT cJasperFile
        ).
    
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunQuery Include
PROCEDURE pRunQuery:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cError     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectTable FOR {1}SubjectTable.    

    FOR EACH {1}SubjectTable
        WHERE {1}SubjectTable.subjectID EQ dynSubject.subjectID
           BY {1}SubjectTable.sortOrder
        :
        cTableName = cTableName + {1}SubjectTable.tableName + ",".
    END. /* each {1}SubjectTable */
    cTableName = TRIM(cTableName,",").
    IF dynSubject.businessLogic EQ "" THEN
    RUN AOA/dynQuery.p (
        ROWID(dynParamValue),
        queryStr,
        cTableName,
        IF ipcType EQ "Grid" THEN 100 ELSE 0,
        OUTPUT hQuery,
        OUTPUT lOK,
        OUTPUT cError
        ).
    ELSE
    RUN pRunBusinessLogic (
        OUTPUT hQuery,
        OUTPUT lOK,
        OUTPUT cError
        ).
    IF lOK THEN DO:
        IF iplRun THEN DO:
            RUN pSetParamValueDefault.
            CASE ipcType:
                WHEN "Grid" THEN
                RUN pResultsBrowser (hQuery).
                WHEN "Print -d" OR WHEN "View" THEN
                RUN pResultsJasper (ipcType, ipcUserID).
                OTHERWISE
                IF dynParamValue.user-id NE "_default" THEN
                RUN pRunNow (ipcType, dynSubject.subjectTitle, YES).
                ELSE
                RUN pResultsJasper (ipcType, ipcUserID).
            END CASE.
        END. /* if run */
        ELSE
        MESSAGE
            "Query Syntax is Correct."        
        VIEW-AS ALERT-BOX TITLE "Query Syntax Check".
    END. /* if ok */
    ELSE MESSAGE cError VIEW-AS ALERT-BOX ERROR TITLE "Query Syntax Check".

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunSubject Include 
PROCEDURE pRunSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    IF AVAILABLE dynParamValue THEN
    DO TRANSACTION:
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        dynParamValue.outputFormat = ipcType.
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */
    RUN pSetDynParamValue (dynSubject.subjectID, ipcUserID, ipcPrgmName, 0).
    IF iplRun THEN
    RUN pSaveDynParamValues (ipcType).
    RUN pRunQuery (iplRun, ipcType, ipcUserID).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetParamValueDefault Include
PROCEDURE pSetParamValueDefault:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bDynParamValue FOR dynParamValue.

    DO TRANSACTION:
        FIND FIRST bDynParamValue EXCLUSIVE-LOCK
             WHERE bDynParamValue.subjectID    EQ dynParamValue.subjectID
               AND bDynParamValue.user-id      EQ dynParamValue.user-id
               AND bDynParamValue.prgmName     EQ dynParamValue.prgmName
               AND bDynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF NOT AVAILABLE bDynParamValue THEN DO:
            CREATE bDynParamValue.
            bDynParamValue.paramDescription = "User Default".
        END. /* if not avail */
        BUFFER-COPY dynParamValue EXCEPT paramValueID paramDescription TO bDynParamValue.
    END. /* do trans */
    RELEASE bDynParamValue.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */
