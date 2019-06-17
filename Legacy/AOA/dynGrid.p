/* dynGrid.p - rstark - 4.30.2019 */

DEFINE INPUT PARAMETER ipiSubjectID  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcParamValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.

RUN spGetDynParamValue (ipiSubjectID, OUTPUT rRowID, OUTPUT cErrorMsg).
IF rRowID NE ? THEN DO:
    DO TRANSACTION:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE ROWID(dynParamValue) EQ rRowID.
        IF AVAILABLE dynParamValue THEN
        DO jdx = 1 TO NUM-ENTRIES(ipcParamValue,"|"):
            ASSIGN
                cParamName  = ENTRY(1,ENTRY(jdx,ipcParamValue,"|"),"^")
                cParamValue = ENTRY(2,ENTRY(jdx,ipcParamValue,"|"),"^")
                .
            DO idx = 1 TO EXTENT(dynParamValue.paramName):
                IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
                IF dynParamValue.paramName[idx] NE cParamName THEN NEXT.
                dynParamValue.paramValue[idx] = cParamValue.
                LEAVE.
            END. /* do idx */
        END. /* do jdx */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */
    RUN AOA/Jasper.p (
        dynParamValue.subjectID,
        dynParamValue.user-id,
        dynParamValue.prgmName,
        dynParamValue.paramValueID,
        NO /* show parameters */
        ).
END. /* if rrowid */
ELSE
MESSAGE
    cErrorMsg
VIEW-AS ALERT-BOX ERROR.
