/* dynUpgrade.p */

DEFINE VARIABLE hSession AS HANDLE  NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER NO-UNDO.

FOR EACH dynValueParam EXCLUSIVE-LOCK:
    DELETE dynValueParam.
END. /* each dynValueParam */

FOR EACH dynValueParamSet EXCLUSIVE-LOCK:
    DELETE dynValueParamSet.
END. /* each dynValueParam */

FOR EACH dynValueColumn EXCLUSIVE-LOCK:
    DELETE dynValueColumn.
END. /* each dynValueParam */

FOR EACH dynParamValue NO-LOCK:
    DO idx = 1 TO EXTENT(dynParamValue.paramSetID):
        IF dynParamValue.paramSetID[idx] EQ 0 THEN LEAVE.
        CREATE dynValueParamSet.
        ASSIGN
            dynValueParamSet.subjectID    = dynParamValue.subjectID
            dynValueParamSet.user-id      = dynParamValue.user-id
            dynValueParamSet.prgmName     = dynParamValue.prgmName
            dynValueParamSet.paramValueID = dynParamValue.paramValueID
            dynValueParamSet.sortOrder    = idx
            dynValueParamSet.paramSetID   = dynParamValue.paramSetID[idx]
            dynValueParamSet.isVisible    = dynParamValue.isVisible[idx]
            .
    END. /* do idx */
    DO idx = 1 TO EXTENT(dynParamValue.paramName):
        IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
        CREATE dynValueParam.
        ASSIGN
            dynValueParam.subjectID    = dynParamValue.subjectID
            dynValueParam.user-id      = dynParamValue.user-id
            dynValueParam.prgmName     = dynParamValue.prgmName
            dynValueParam.paramValueID = dynParamValue.paramValueID
            dynValueParam.sortOrder    = idx
            dynValueParam.paramName    = dynParamValue.paramName[idx]
            dynValueParam.paramLabel   = dynParamValue.paramLabel[idx]
            dynValueParam.paramValue   = dynParamValue.paramValue[idx]
            dynValueParam.dataType     = dynParamValue.paramDataType[idx]
            dynValueParam.paramFormat  = dynParamValue.paramFormat[idx]
            .
    END. /* do idx */
    DO idx = 1 TO EXTENT(dynParamValue.colName):
        IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
        CREATE dynValueColumn.
        ASSIGN
            dynValueColumn.subjectID      = dynParamValue.subjectID
            dynValueColumn.user-id        = dynParamValue.user-id
            dynValueColumn.prgmName       = dynParamValue.prgmName
            dynValueColumn.paramValueID   = dynParamValue.paramValueID
            dynValueColumn.sortOrder      = idx
            dynValueColumn.isActive       = dynParamValue.isActive[idx]
            dynValueColumn.colName        = dynParamValue.colName[idx]
            dynValueColumn.colLabel       = dynParamValue.colLabel[idx]
            dynValueColumn.colFormat      = dynParamValue.colFormat[idx]
            dynValueColumn.columnSize     = dynParamValue.columnSize[idx]
            dynValueColumn.dataType       = dynParamValue.dataType[idx]
            dynValueColumn.sortCol        = dynParamValue.sortCol[idx]
            dynValueColumn.sortDescending = dynParamValue.sortDescending[idx]
            dynValueColumn.isGroup        = dynParamValue.isGroup[idx]
            dynValueColumn.isReturnValue  = dynParamValue.isReturnValue[idx]
            dynValueColumn.isSearchable   = dynParamValue.isSearchable[idx]
            dynValueColumn.isSortable     = dynParamValue.isSortable[idx]
            dynValueColumn.groupLabel     = dynParamValue.groupLabel[idx]
            dynValueColumn.groupCalc      = dynParamValue.groupCalc[idx]
            dynValueColumn.isCalcField    = dynParamValue.isCalcField[idx]
            dynValueColumn.calcProc       = dynParamValue.calcProc[idx]
            dynValueColumn.calcParam      = dynParamValue.calcParam[idx]
            dynValueColumn.calcFormula    = dynParamValue.calcFormula[idx]
            .
    END. /* do idx */
END. /* each dynParamValue */

OUTPUT TO c:\tmp\dynParamValue.save.d.
FOR EACH dynParamValue NO-LOCK:
    EXPORT dynParamValue.
END. /* each dynparamvalue */
OUTPUT CLOSE.

RELEASE dynValueParamSet.
RELEASE dynValueParam.
RELEASE dynValueColumn.
RELEASE dynParamValue.
