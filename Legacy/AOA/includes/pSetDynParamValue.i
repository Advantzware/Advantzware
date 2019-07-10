/* pSetDynParamValue.i - rstark - 2.12.2019 */

/* {1} = "tt" when used in dynSubjct.w, "dyn" in all other instances */

PROCEDURE pSetDynParamValue:
    DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO. 
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER dynSubject         FOR dynSubject.
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    DEFINE BUFFER {1}SubjectColumn   FOR {1}SubjectColumn.
    
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ipiSubjectID
           AND dynParamValue.user-id      EQ ipcUserID
           AND dynParamValue.prgmName     EQ ipcPrgmName
           AND dynParamValue.paramValueID EQ ipiParamValueID
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN DO TRANSACTION:
        FIND FIRST dynSubject NO-LOCK
             WHERE dynSubject.subjectID EQ ipiSubjectID
             NO-ERROR.
        IF NOT AVAILABLE dynSubject THEN RETURN.
        CREATE dynParamValue.
        ASSIGN
            dynParamValue.subjectID        = ipiSubjectID
            dynParamValue.user-id          = ipcUserID
            dynParamValue.prgmName         = ipcPrgmName
            dynParamValue.paramValueID     = ipiParamValueID
            dynParamValue.paramTitle       = dynSubject.subjectTitle
            dynParamValue.module           = dynSubject.module
            dynParamValue.outputFormat     = dynSubject.outputFormat
            dynParamValue.paramDescription = IF ipcUserID EQ "{&defaultUser}" THEN "System Default"
                                             ELSE "User Default"
            dynParamValue.externalForm     = dynSubject.externalForm
            dynParamValue.securityLevel    = dynSubject.securityLevel
            dynParamValue.recordLimit      = dynSubject.recordLimit
            .
        FOR EACH {1}SubjectParamSet NO-LOCK
            WHERE {1}SubjectParamSet.subjectID EQ ipiSubjectID,
            EACH dynParamSetDtl NO-LOCK
            WHERE dynParamSetDtl.paramSetID EQ {1}SubjectParamSet.paramSetID,
            FIRST dynParam NO-LOCK
            WHERE dynParam.paramID EQ dynParamSetDtl.paramID
            :
            ASSIGN
                idx                              = idx + 1
                dynParamValue.paramName[idx]     = dynParamSetDtl.paramName
                dynParamValue.paramLabel[idx]    = dynParamSetDtl.paramLabel
                dynParamValue.paramValue[idx]    = dynParamSetDtl.initialValue
                dynParamValue.paramDataType[idx] = dynParam.dataType
                dynParamValue.paramFormat[idx]   = dynParam.paramFormat
                .
        END. /* each dynsubjectparamset */
        idx = 0.
        FOR EACH {1}SubjectColumn NO-LOCK
            WHERE {1}SubjectColumn.subjectID EQ ipiSubjectID
               BY {1}SubjectColumn.sortOrder
            :
            ASSIGN
                idx = idx + 1
                dynParamValue.isActive[idx]       = {1}SubjectColumn.isActive
                dynParamValue.colName[idx]        = {1}SubjectColumn.fieldName
                dynParamValue.colLabel[idx]       = {1}SubjectColumn.fieldLabel
                dynParamValue.colFormat[idx]      = {1}SubjectColumn.fieldFormat
                dynParamValue.columnSize[idx]     = {1}SubjectColumn.columnSize
                dynParamValue.dataType[idx]       = {1}SubjectColumn.dataType
                dynParamValue.sortCol[idx]        = {1}SubjectColumn.sortCol
                dynParamValue.sortDescending[idx] = {1}SubjectColumn.sortDescending
                dynParamValue.isGroup[idx]        = {1}SubjectColumn.isGroup
                dynParamValue.groupLabel[idx]     = {1}SubjectColumn.groupLabel
                dynParamValue.groupCalc[idx]      = {1}SubjectColumn.groupCalc
                dynParamValue.isCalcField[idx]    = {1}SubjectColumn.isCalcField
                dynParamValue.calcProc[idx]       = {1}SubjectColumn.calcProc
                dynParamValue.calcParam[idx]      = {1}SubjectColumn.calcParam
                .
        END. /* each {1}SubjectColumn */
        FOR EACH {1}SubjectParamSet NO-LOCK
            WHERE {1}SubjectParamSet.subjectID EQ ipiSubjectID
               BY {1}SubjectParamSet.sortOrder
            :
            ASSIGN
                dynParamValue.paramSetID[{1}SubjectParamSet.sortOrder] = {1}SubjectParamSet.paramSetID
                dynParamValue.isVisible[{1}SubjectParamSet.sortOrder]  = {1}SubjectParamSet.isVisible
                .
        END. /* each {1}SubjectParamSet */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* not avail */

END PROCEDURE.
