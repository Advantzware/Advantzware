/* pSetDynParamValue.i - rstark - 2.12.2019 */

/* {1} = "tt" when used in dynSubjct.w, "dyn" in all other instances */

PROCEDURE pSetDynParamValue:
    DEFINE INPUT PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO. 
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iSortOrder AS INTEGER NO-UNDO.
    
    DEFINE BUFFER dynSubject         FOR dynSubject.
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    DEFINE BUFFER {1}SubjectColumn   FOR {1}SubjectColumn.
    
    IF ipcUserID   EQ "" THEN ipcUserID   = "_default".
    IF ipcPrgmName EQ "" THEN ipcPrgmName = "dynSubjct.".
    FOR EACH dynValueParam EXCLUSIVE-LOCK
        WHERE dynValueParam.subjectID    EQ ipiSubjectID
          AND dynValueParam.user-id      EQ ipcUserID
          AND dynValueParam.prgmName     EQ ipcPrgmName
          AND dynValueParam.paramValueID EQ ipiParamValueID
        :
        DELETE dynValueParam.
    END. /* each dynvalueparam */
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
            dynParamValue.isLookup         = dynSubject.isLookup
            dynParamValue.runSync          = dynSubject.runSync
            dynParamValue.custListID       = dynSubject.custListID
            dynParamValue.useCustList      = dynSubject.useCustList
            .
        FOR EACH dynValueColumn EXCLUSIVE-LOCK
            WHERE dynValueColumn.subjectID    EQ ipiSubjectID
              AND dynValueColumn.user-id      EQ ipcUserID
              AND dynValueColumn.prgmName     EQ ipcPrgmName
              AND dynValueColumn.paramValueID EQ ipiParamValueID
            :
            DELETE dynValueColumn.
        END. /* each dynvaluecolumn */
        FOR EACH {1}SubjectColumn NO-LOCK
            WHERE {1}SubjectColumn.subjectID EQ ipiSubjectID
               BY {1}SubjectColumn.sortOrder
            :
            CREATE dynValueColumn.
            ASSIGN
                dynValueColumn.subjectID      = ipiSubjectID
                dynValueColumn.user-id        = ipcUserID
                dynValueColumn.prgmName       = ipcPrgmName
                dynValueColumn.paramValueID   = ipiParamValueID
                dynValueColumn.sortOrder      = {1}SubjectColumn.sortOrder
                dynValueColumn.calcFormula    = {1}SubjectColumn.calcFormula
                dynValueColumn.calcParam      = {1}SubjectColumn.calcParam
                dynValueColumn.calcProc       = {1}SubjectColumn.calcProc
                dynValueColumn.cellColor      = {1}SubjectColumn.cellColor
                dynValueColumn.colName        = {1}SubjectColumn.fieldName
                dynValueColumn.colLabel       = {1}SubjectColumn.fieldLabel
                dynValueColumn.colFormat      = {1}SubjectColumn.fieldFormat
                dynValueColumn.columnSize     = {1}SubjectColumn.columnSize
                dynValueColumn.compareValue   = {1}SubjectColumn.compareValue
                dynValueColumn.dataType       = {1}SubjectColumn.dataType
                dynValueColumn.groupCalc      = {1}SubjectColumn.groupCalc
                dynValueColumn.groupLabel     = {1}SubjectColumn.groupLabel
                dynValueColumn.isActive       = {1}SubjectColumn.isActive
                dynValueColumn.isCalcField    = {1}SubjectColumn.isCalcField
                dynValueColumn.isGroup        = {1}SubjectColumn.isGroup
                dynValueColumn.isReturnValue  = {1}SubjectColumn.isReturnValue
                dynValueColumn.isSearchable   = {1}SubjectColumn.isSearchable
                dynValueColumn.isSortable     = {1}SubjectColumn.isSortable
                dynValueColumn.isStatusField  = {1}SubjectColumn.isStatusField
                dynValueColumn.statusAction   = {1}SubjectColumn.statusAction
                dynValueColumn.statusCompare  = {1}SubjectColumn.statusCompare
                dynValueColumn.sortCol        = {1}SubjectColumn.sortCol
                dynValueColumn.sortDescending = {1}SubjectColumn.sortDescending
                dynValueColumn.textColor      = {1}SubjectColumn.textColor
                dynValueColumn.custListField  = {1}SubjectColumn.custListField
                .
        END. /* each {1}SubjectColumn */
        RELEASE dynValueColumn.
        FOR EACH dynValueParamSet EXCLUSIVE-LOCK
            WHERE dynValueParamSet.subjectID    EQ ipiSubjectID
              AND dynValueParamSet.user-id      EQ ipcUserID
              AND dynValueParamSet.prgmName     EQ ipcPrgmName
              AND dynValueParamSet.paramValueID EQ ipiParamValueID
            :
            DELETE dynValueParamSet.
        END. /* each dynvalueparamset */
        FOR EACH {1}SubjectParamSet NO-LOCK
            WHERE {1}SubjectParamSet.subjectID EQ ipiSubjectID
               BY {1}SubjectParamSet.sortOrder
            :
            CREATE dynValueParamSet.
            ASSIGN
                dynValueParamSet.subjectID    = ipiSubjectID
                dynValueParamSet.user-id      = ipcUserID
                dynValueParamSet.prgmName     = ipcPrgmName
                dynValueParamSet.paramValueID = ipiParamValueID
                dynValueParamSet.sortOrder    = {1}SubjectParamSet.sortOrder
                dynValueParamSet.paramSetID   = {1}SubjectParamSet.paramSetID
                dynValueParamSet.isVisible    = {1}SubjectParamSet.isVisible
                .
        END. /* each {1}SubjectParamSet */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* not avail */
    FOR EACH {1}SubjectParamSet NO-LOCK
        WHERE {1}SubjectParamSet.subjectID EQ ipiSubjectID,
        EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID EQ {1}SubjectParamSet.paramSetID,
        FIRST dynParam NO-LOCK
        WHERE dynParam.paramID EQ dynParamSetDtl.paramID
           BY {1}SubjectParamSet.setRow
           BY {1}SubjectParamSet.setCol
           BY dynParamSetDtl.paramRow
           BY dynParamSetDtl.paramCol
        :
        CREATE dynValueParam.
        ASSIGN
            iSortOrder                 = iSortOrder + 1
            dynValueParam.subjectID    = ipiSubjectID
            dynValueParam.user-id      = ipcUserID
            dynValueParam.prgmName     = ipcPrgmName
            dynValueParam.paramValueID = ipiParamValueID
            dynValueParam.sortOrder    = iSortOrder
            dynValueParam.paramName    = dynParamSetDtl.paramName
            dynValueParam.paramLabel   = dynParamSetDtl.paramLabel
            dynValueParam.paramValue   = dynParamSetDtl.initialValue
            dynValueParam.dataType     = dynParam.dataType
            dynValueParam.paramFormat  = dynParam.paramFormat
            .
    END. /* each dynsubjectparamset */
END PROCEDURE.
