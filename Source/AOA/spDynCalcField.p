/* spDynCalcField.p - rstark - 1.28.2019 */

/* add dynamic calc field procedures in alphabetical order   */
/* dynamics will always call procedure spDynCalcField which  */
/* contains a case statement.  place your procedure name and */
/* break ipcCalcParam apart which is pipe "|" delimited  and */
/* include output parameter opcCalcValue                     */

FUNCTION fCalcTime RETURNS INTEGER (ipcTime AS CHARACTER):
    DEFINE VARIABLE iHours   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMinutes AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTime    AS INTEGER NO-UNDO.

    ASSIGN
        iHours   = INTEGER(SUBSTR(ipcTime,1,2))
        iMinutes = INTEGER(SUBSTR(ipcTime,4,2))
        iTime    = iHours * 3600 + iMinutes * 60
        .
    RETURN iTime.
END FUNCTION.

PROCEDURE calcShiftEndTime:
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplUseTime    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStartShift AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEndShift   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTime       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iShiftEndTime AS INTEGER NO-UNDO.
    
    IF iplUseTime THEN
    iShiftEndTime = fCalcTime(ipcTime).
    ELSE DO:
        FIND FIRST shifts NO-LOCK
             WHERE shifts.company EQ ipcCompany
               AND shifts.shift   EQ ipcStartShift
             NO-ERROR.
        IF AVAILABLE shifts THEN
        iShiftEndTime = shifts.end_time.
    
        IF ipcStartShift NE ipcEndShift THEN DO:
            FIND FIRST shifts NO-LOCK
                 WHERE shifts.company EQ ipcCompany
                   AND shifts.shift   EQ ipcEndShift
                 NO-ERROR.
            IF AVAILABLE shifts THEN
            iShiftEndTime = shifts.end_time.
        END. /* different shifts */
    END. /* else */

    ASSIGN
        iShiftEndTime = iShiftEndTime * 1000
        opcCalcValue  = STRING(iShiftEndTime)
        .
END PROCEDURE.
    
PROCEDURE calcShiftStartTime:
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplUseTime    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStartShift AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTime       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iShiftStartTime AS INTEGER NO-UNDO.
    
    IF iplUseTime THEN
    iShiftStartTime = fCalcTime(ipcTime).
    ELSE DO:
        FIND FIRST shifts NO-LOCK
             WHERE shifts.company EQ ipcCompany
               AND shifts.shift   EQ ipcStartShift
             NO-ERROR.
        IF AVAILABLE shifts THEN
        iShiftStartTime = shifts.start_time.
    END. /* else */
    
    ASSIGN
        iShiftStartTime = iShiftStartTime * 1000
        opcCalcValue    = STRING(iShiftStartTime)
        .
END PROCEDURE.
    
PROCEDURE calcStringDateTime:
    DEFINE INPUT  PARAMETER ipdtDate     AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipdtDate,"99/99/9999") + " "
                 + STRING(ipiTime,"hh:mm:ss am")
                 .
END PROCEDURE.
	
PROCEDURE calcStringTime:
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipiTime,"hh:mm:ss am").
END PROCEDURE.
	
PROCEDURE calcTimeString:
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipiTime,"hh:mm:ss").
END PROCEDURE.

PROCEDURE calcAPIType:
    DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue     AS CHARACTER NO-UNDO.
    
    opcCalcValue = IF ipiAPIOutboundID GT 5000 THEN
                       "Custom"
                   ELSE
                       "System".
END PROCEDURE.

PROCEDURE spDynCalcField:
    DEFINE INPUT  PARAMETER iphQuery     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcProc  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcParam AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStr    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hField  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable  AS HANDLE    NO-UNDO.
    
    /* parse parameter string, replace fields with actual values */
    DO idx = 1 TO NUM-ENTRIES(ipcCalcParam,"|"):
        cParam = ENTRY(idx,ipcCalcParam,"|").
        IF INDEX(cParam,".") NE 0 THEN DO:
            ASSIGN
                cTable  = ENTRY(1,cParam,".")
                cField  = ENTRY(2,cParam,".")
                iExtent = 0
                .
            IF INDEX(cField,"[") NE 0 THEN
            ASSIGN
                cStr    = SUBSTRING(cField,INDEX(cField,"[") + 1)
                cStr    = REPLACE(cStr,"]","")
                iExtent = INTEGER(cStr)
                cField  = SUBSTRING(cField,1,INDEX(cField,"[") - 1)
                .
            ASSIGN 
                hTable = iphQuery:GET-BUFFER-HANDLE(cTable)
                cValue = hTable:BUFFER-FIELD(cField):BUFFER-VALUE(iExtent)
                ENTRY(idx,ipcCalcParam,"|") = TRIM(cValue)
                .
        END. /* if database field */
    END. /* do idx */
    /* list case when values alphabetically */
    CASE ipcCalcProc:
        WHEN "calcShiftEndTime" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|") EQ "yes",
            ENTRY(3,ipcCalcParam,"|"),
            ENTRY(4,ipcCalcParam,"|"),
            ENTRY(5,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).
        WHEN "calcShiftStartTime" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|") EQ "yes",
            ENTRY(3,ipcCalcParam,"|"),
            ENTRY(4,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).
        WHEN "calcStringDateTime" THEN
        RUN VALUE(ipcCalcProc) (
            DATE(ENTRY(1,ipcCalcParam,"|")),
            INTEGER(ENTRY(2,ipcCalcParam,"|")),
            OUTPUT opcCalcValue).
        WHEN "calcStringTime" THEN
        RUN VALUE(ipcCalcProc) (
            INTEGER(ipcCalcParam),
            OUTPUT opcCalcValue).
        WHEN "calcTimeString" THEN
        RUN VALUE(ipcCalcProc) (
            INTEGER(ipcCalcParam),
            OUTPUT opcCalcValue).
        WHEN "calcAPIType" THEN
        RUN VALUE(ipcCalcProc) (
            INTEGER(ipcCalcParam),
            OUTPUT opcCalcValue).
    END CASE.
END PROCEDURE.
