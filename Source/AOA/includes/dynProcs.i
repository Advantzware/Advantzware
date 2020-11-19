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

DEFINE VARIABLE cBufferValue    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormula        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBrowseColumn   AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hBrowseQuery    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCalcColumn     AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynDescripProc AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynInitProc    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynValProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFGColor        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNumColumns     AS INTEGER   NO-UNDO.

{sys/ref/CustList.i NEW}

RUN AOA/spDynDescriptionProc.p PERSISTENT SET hDynDescripProc.
RUN AOA/spDynInitializeProc.p  PERSISTENT SET hDynInitProc.
RUN AOA/spDynValidateProc.p    PERSISTENT SET hDynValProc.
RUN AOA/spDynCalcField.p       PERSISTENT SET hDynCalcField.
SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

RUN spSetSessionParam ("Company",  g_company).
RUN spSetSessionParam ("Location", g_loc).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 

/* ***************************  Main Block  *************************** */

&IF "{&program-id}" NE "dynBrowserParam." &THEN
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
    iFGColor = IF iFGColor NE 9 THEN 9 ELSE 1.
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
           BY dynValueColumn.sortOrder
        :
        IF VALID-HANDLE(hCalcColumn[dynValueColumn.sortOrder]) AND
           dynValueColumn.isCalcField EQ YES THEN DO:
            ASSIGN
                cFormula = REPLACE(dynValueColumn.calcFormula,"$F~{","")
                cFormula = REPLACE(cFormula,"}","")
                cFormula = REPLACE(cFormula,"__",".")
                cBufferValue = ""
                .
            IF dynValueColumn.calcProc NE "" THEN
            RUN spDynCalcField IN hDynCalcField (
                hBrowseQuery:HANDLE,
                dynValueColumn.calcProc,
                dynValueColumn.calcParam,
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            ELSE
            IF dynValueColumn.calcFormula NE "" AND
               INDEX(dynValueColumn.calcFormula,"$") EQ 0 THEN
            RUN spDynCalcField IN hDynCalcField (
                hBrowseQuery:HANDLE,
                "Calculator",
                dynValueColumn.calcFormula,
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            ELSE
            IF dynValueColumn.calcFormula NE "" THEN
            cBufferValue = cFormula.
            ASSIGN
                hCalcColumn[dynValueColumn.sortOrder]:SCREEN-VALUE = cBufferValue
                hCalcColumn[dynValueColumn.sortOrder]:FGCOLOR      = iFGColor
                .
        END. /* if valid handle */
        IF VALID-HANDLE(hBrowseColumn[dynValueColumn.sortOrder]) AND
           dynValueColumn.isCalcField EQ NO THEN
        ASSIGN
            hBrowseColumn[dynValueColumn.sortOrder]:FORMAT  = dynValueColumn.colFormat
            hBrowseColumn[dynValueColumn.sortOrder]:FGCOLOR = iFGColor
            .
        IF dynValueColumn.isStatusField AND
           dynValueColumn.textColor NE dynValueColumn.cellColor AND
           DYNAMIC-FUNCTION("fDynStatusField" IN hDynCalcField,
               hBrowseQuery:HANDLE,
               dynValueColumn.colName,
               dynValueColumn.statusCompare,
               dynValueColumn.compareValue) THEN DO:
            IF dynValueColumn.statusAction BEGINS "Row" THEN
            DO idx = 1 TO iNumColumns:
                IF VALID-HANDLE(hBrowseColumn[idx]) THEN
                ASSIGN
                    hBrowseColumn[idx]:FGCOLOR = dynValueColumn.textColor
                    hBrowseColumn[idx]:BGCOLOR = dynValueColumn.cellColor
                    .
            END. /* else */
            ELSE IF dynValueColumn.statusAction BEGINS "Cell" THEN
            ASSIGN
                hBrowseColumn[dynValueColumn.sortOrder]:FGCOLOR = dynValueColumn.textColor
                hBrowseColumn[dynValueColumn.sortOrder]:BGCOLOR = dynValueColumn.cellColor
                .
        END. /* if begins */
    END. /* each dynvaluecolumn */
END. /* row-display */
&ENDIF

{AOA/includes/pDynParamProcs.i "{1}"}
{AOA/includes/pRunNow.i}
{AOA/includes/pRunBusinessLogic.i}
{AOA/includes/pSetDynParamValue.i "{1}"}

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
    
    FIND FIRST dynValueParam NO-LOCK
         WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
           AND dynValueParam.user-id      EQ dynParamValue.user-id
           AND dynValueParam.prgmName     EQ dynParamValue.prgmName
           AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
           AND dynValueParam.paramName    EQ "svRecipients"
         NO-ERROR.
    IF AVAILABLE dynValueParam THEN
    opcRecipients = dynValueParam.paramValue.
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
    
&IF "{&program-id}" NE "dynBrowserParam." &THEN
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DEFINE BUFFER b{1}SubjectColumn FOR {1}SubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    iphQuery:QUERY-CLOSE.
    DO idx = 1 TO hQueryBrowse:NUM-COLUMNS:
        ASSIGN
            hBrowseColumn[idx] = ?
            hCalcColumn[idx] = ?
            hColumn = hQueryBrowse:GET-BROWSE-COLUMN(idx)
            .
        DELETE OBJECT hColumn.
    END. /* do idx */
    ASSIGN
        hQueryBrowse:TITLE   = dynSubject.subjectTitle
        hQueryBrowse:QUERY   = iphQuery
        hQueryBrowse:VISIBLE = TRUE
        iNumColumns          = 0
        .
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
          AND dynValueColumn.isActive     EQ YES
           BY dynValueColumn.sortOrder
        :
        hColumn = ?.
        IF dynValueColumn.isCalcField THEN DO:
            IF dynValueColumn.calcProc NE "" THEN
            ASSIGN
                hCalcColumn[dynValueColumn.sortOrder] = hQueryBrowse:ADD-CALC-COLUMN(
                    dynValueColumn.dataType,
                    dynValueColumn.colFormat,
                    "",
                    dynValueColumn.colLabel
                    )
                hColumn = hCalcColumn[dynValueColumn.sortOrder]
                .
            ELSE
            IF dynValueColumn.calcFormula NE "" THEN
            ASSIGN
                cFormula = REPLACE(dynValueColumn.calcFormula,"$F~{","")
                cFormula = REPLACE(cFormula,"}","")
                cFormula = REPLACE(cFormula,"__",".")
                hCalcColumn[dynValueColumn.sortOrder] = hQueryBrowse:ADD-CALC-COLUMN(
                    IF INDEX(dynValueColumn.calcFormula,"|") NE 0 THEN dynValueColumn.dataType ELSE "Character",
                    IF INDEX(dynValueColumn.calcFormula,"|") NE 0 THEN dynValueColumn.colFormat ELSE "x(" + STRING(LENGTH(cFormula)) + ")",
                    "",
                    dynValueColumn.colLabel + "[Calc]"
                    )
                hColumn = hCalcColumn[dynValueColumn.sortOrder]
                .
        END. /* if calc field */
        ELSE
        ASSIGN
            hColumn = hQueryBrowse:ADD-LIKE-COLUMN(dynValueColumn.colName)
            hColumn:LABEL = dynValueColumn.colLabel
            hBrowseColumn[dynValueColumn.sortOrder] = hColumn
            .
        IF NOT VALID-HANDLE(hColumn) THEN NEXT.
/*        IF idx MOD 2 EQ 0 THEN hColumn:COLUMN-BGCOLOR = 11.*/
        IF dynValueColumn.columnSize NE 0 THEN
        hColumn:WIDTH-CHARS = dynValueColumn.columnSize.
        iNumColumns = iNumColumns + 1.
    END. /* each dynvaluecolumn */
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
&ENDIF

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
    DEFINE INPUT PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    RUN spJasperQuery IN hJasper (
        ipcType,
        ROWID(dynParamValue),
        dynSubject.subjectTitle,
        ipcUserID,
        hAppSrvBin,
        ipcTaskRecKey,
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
    DEFINE INPUT PARAMETER iplRun        AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cError     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectTable FOR {1}SubjectTable.    

    FOR EACH {1}SubjectTable NO-LOCK
        WHERE {1}SubjectTable.subjectID EQ dynSubject.subjectID
           BY {1}SubjectTable.sortOrder
        :
        cTableName = cTableName + {1}SubjectTable.tableName + ",".
    END. /* each {1}SubjectTable */
    ASSIGN
        cTableName = TRIM(cTableName,",")
        lOK        = TRUE
        .
    IF ipcType EQ "Grid" THEN DO:
        IF dynSubject.businessLogic EQ "" THEN
        RUN AOA/dynQuery.p (
            ROWID(dynParamValue),
            queryStr,
            cTableName,
            IF ipcType EQ "Grid" THEN 2500 ELSE dynParamValue.recordLimit,
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
    END. /* if not grid,print,view */
    IF lOK THEN DO:
        IF iplRun THEN DO:
            RUN pSetParamValueDefault.
            CASE ipcType:
                WHEN "Grid" THEN
                RUN pResultsBrowser (hQuery).
                WHEN "LocalCSV" OR WHEN "Print -d" OR WHEN "View" THEN
                RUN pResultsJasper (ipcType, ipcUserID, ipcTaskRecKey).
                OTHERWISE DO:
                    IF dynParamValue.user-id NE "_default" THEN
                    RUN pRunNow (ipcType, dynSubject.subjectTitle, YES).
                    ELSE
                    RUN pResultsJasper (ipcType, ipcUserID, ipcTaskRecKey).
                END. /* otherwise */
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
    
    DEFINE VARIABLE cTaskRecKey AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    IF AVAILABLE dynParamValue THEN
    DO TRANSACTION:
        FIND CURRENT dynSubject EXCLUSIVE-LOCK.
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
            dynParamValue.pageFormat      = dynSubject.pageFormat
            dynParamValue.pageOrientation = dynSubject.pageOrientation
            dynParamValue.pageWidth       = dynSubject.pageWidth
            dynParamValue.pageHeight      = dynSubject.pageHeight
            dynParamValue.outputFormat    = ipcType
            dynParamValue.lastRunDateTime = NOW
            dynSubject.lastRunDateTime    = NOW
            .
        FIND CURRENT dynParamValue NO-LOCK.
        FIND CURRENT dynSubject NO-LOCK.
    END. /* do trans */
    RUN pSetDynParamValue (dynSubject.subjectID, ipcUserID, ipcPrgmName, 0).
    IF iplRun THEN
    RUN pSaveDynParamValues (ipcType).
    cTaskRecKey = IF AVAILABLE Task THEN Task.rec_key ELSE "NoTask".
    RUN pRunQuery (iplRun, ipcType, ipcUserID, cTaskRecKey).
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
    DEFINE VARIABLE cUserID AS CHARACTER NO-UNDO.

    DEFINE BUFFER bDynParamValue    FOR dynParamValue.
    DEFINE BUFFER bDynValueColumn   FOR dynValueColumn.
    DEFINE BUFFER bDynValueParam    FOR dynValueParam.
    DEFINE BUFFER bDynValueParamSet FOR dynValueParamSet.

    cUserID = dynParamValue.user-id.
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
        BUFFER-COPY dynParamValue
             EXCEPT paramValueID paramDescription
                 TO bDynParamValue.

/*        FOR EACH dynValueColumn EXCLUSIVE-LOCK                          */
/*            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID*/
/*              AND dynValueColumn.user-id      EQ dynParamValue.user-id  */
/*              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName */
/*              AND dynValueColumn.paramValueID EQ 0                      */
/*            :                                                           */
/*            DELETE dynValueColumn.                                      */
/*        END. /* each dynvaluecolumn */                                  */
        FOR EACH dynValueColumn NO-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
               BY dynValueColumn.sortOrder
            :
            FIND FIRST bDynValueColumn EXCLUSIVE-LOCK
                 WHERE bDynValueColumn.subjectID    EQ dynValueColumn.subjectID
                   AND bDynValueColumn.user-id      EQ dynValueColumn.user-id
                   AND bDynValueColumn.prgmName     EQ dynValueColumn.prgmName
                   AND bDynValueColumn.paramValueID EQ 0
                   AND bDynValueColumn.sortOrder    EQ dynValueColumn.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueColumn THEN
            CREATE bDynValueColumn.
            BUFFER-COPY dynValueColumn
                 EXCEPT paramValueID
                     TO bDynValueColumn.
        END. /* each dynvaluecolumn */
/*        FOR EACH dynValueParam EXCLUSIVE-LOCK                          */
/*            WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID*/
/*              AND dynValueParam.user-id      EQ dynParamValue.user-id  */
/*              AND dynValueParam.prgmName     EQ dynParamValue.prgmName */
/*              AND dynValueParam.paramValueID EQ 0                      */
/*            :                                                          */
/*            DELETE dynValueParam.                                      */
/*        END. /* each dynValueParam */                                  */
        FOR EACH dynValueParam NO-LOCK
            WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
              AND dynValueParam.user-id      EQ dynParamValue.user-id
              AND dynValueParam.prgmName     EQ dynParamValue.prgmName
              AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
               BY dynValueParam.sortOrder
            :
            FIND FIRST bDynValueParam EXCLUSIVE-LOCK
                 WHERE bDynValueParam.subjectID    EQ dynValueParam.subjectID
                   AND bDynValueParam.user-id      EQ dynValueParam.user-id
                   AND bDynValueParam.prgmName     EQ dynValueParam.prgmName
                   AND bDynValueParam.paramValueID EQ 0
                   AND bDynValueParam.sortOrder    EQ dynValueParam.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueParam THEN
            CREATE bDynValueParam.
            BUFFER-COPY dynValueParam
                 EXCEPT paramValueID
                     TO bDynValueParam.
        END. /* each dynValueParam */
/*        FOR EACH dynValueParamSet EXCLUSIVE-LOCK                          */
/*            WHERE dynValueParamSet.subjectID    EQ dynParamValue.subjectID*/
/*              AND dynValueParamSet.user-id      EQ dynParamValue.user-id  */
/*              AND dynValueParamSet.prgmName     EQ dynParamValue.prgmName */
/*              AND dynValueParamSet.paramValueID EQ 0                      */
/*            :                                                             */
/*            DELETE dynValueParamSet.                                      */
/*        END. /* each dynValueParamSet */                                  */
        FOR EACH dynValueParamSet NO-LOCK
            WHERE dynValueParamSet.subjectID    EQ dynParamValue.subjectID
              AND dynValueParamSet.user-id      EQ dynParamValue.user-id
              AND dynValueParamSet.prgmName     EQ dynParamValue.prgmName
              AND dynValueParamSet.paramValueID EQ dynParamValue.paramValueID
               BY dynValueParamSet.sortOrder
            :
            FIND FIRST bDynValueParamSet EXCLUSIVE-LOCK
                 WHERE bDynValueParamSet.subjectID    EQ dynValueParamSet.subjectID
                   AND bDynValueParamSet.user-id      EQ dynValueParamSet.user-id
                   AND bDynValueParamSet.prgmName     EQ dynValueParamSet.prgmName
                   AND bDynValueParamSet.paramValueID EQ 0
                   AND bDynValueParamSet.sortOrder    EQ dynValueParamSet.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueParamSet THEN
            CREATE bDynValueParamSet.
            BUFFER-COPY dynValueParamSet
                 EXCEPT paramValueID
                    TO bDynValueParamSet.
        END. /* each dynValueParamSet */
        RELEASE bDynParamValue.
        RELEASE bDynValueColumn.
        RELEASE bDynValueParam.
        RELEASE bDynValueParamSet.
    END. /* do trans */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */
