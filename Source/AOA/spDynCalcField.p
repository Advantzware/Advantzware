/* spDynCalcField.p - rstark - 1.28.2019 */

/* add dynamic calc field procedures in alphabetical order   */
/* dynamics will always call procedure spDynCalcField which  */
/* contains a case statement.  place your procedure name and */
/* break ipcCalcParam apart which is pipe "|" delimited  and */
/* include output parameter opcCalcValue                     */

/* **********************  Internal Functions  ************************ */

/* ************************  Function Implementations ***************** */
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

FUNCTION fDynStatusField RETURNS LOGICAL
    (iphQuery         AS HANDLE,
     ipcFieldName     AS CHARACTER,
     ipcStatusCompare AS CHARACTER,
     ipcCompareValue  AS CHARACTER):

    DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStr    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hTable  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO.

    ASSIGN
        cTable  = ENTRY(1,ipcFieldName,".")
        cField  = ENTRY(2,ipcFieldName,".")
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
        .
    CASE ipcStatusCompare:
        WHEN "EQ" THEN
        RETURN cValue EQ ipcCompareValue.
        WHEN "NE" THEN
        RETURN cValue NE ipcCompareValue.
        WHEN "LT" THEN
        RETURN cValue LT ipcCompareValue.
        WHEN "LE" THEN
        RETURN cValue LE ipcCompareValue.
        WHEN "GT" THEN
        RETURN cValue GT ipcCompareValue.
        WHEN "GE" THEN
        RETURN cValue GE ipcCompareValue.
        WHEN "BEGINS" THEN
        RETURN cValue BEGINS ipcCompareValue.
    END CASE.
END FUNCTION.

FUNCTION fMathOperation RETURNS DECIMAL
	(ipdNumber1 AS DECIMAL, ipdNumber2 AS DECIMAL, ipcOperator AS CHARACTER):
    DEFINE VARIABLE opdResult AS DECIMAL NO-UNDO.
    
    CASE ipcOperator:
        WHEN "+" THEN
        opdResult = ipdNumber1 + ipdNumber2.
        WHEN "-" THEN
        opdResult = ipdNumber1 - ipdNumber2.
        WHEN "*" THEN
        opdResult = ipdNumber1 * ipdNumber2.
        WHEN "/" THEN
        IF ipdNumber2 NE 0 THEN
        opdResult = ipdNumber1 / ipdNumber2.
    END CASE.	

	RETURN opdResult.
		
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE calcAPIType:
    DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue     AS CHARACTER NO-UNDO.
    
    opcCalcValue = IF ipiAPIOutboundID GT 5000 THEN "Custom"
                   ELSE "System".
END PROCEDURE.

PROCEDURE calcDropShipment:
    DEFINE INPUT  PARAMETER ipcPOType    AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcPOCust    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
   IF ipcPOType EQ "D" THEN
   opcCalcValue = IF ipcPOCust NE "" THEN "Customer" ELSE "Vendor".   
END PROCEDURE.

PROCEDURE calcGetChgMethod:
    DEFINE INPUT  PARAMETER ipcChgMethod AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = IF ipcChgMethod EQ "M" THEN "MSF" 
              ELSE IF ipcChgMethod EQ "P" THEN "Pallet"
              ELSE "Weight".
END PROCEDURE.

PROCEDURE calcGetCarrierInActive:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCarrier   AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    FIND FIRST carrier NO-LOCK
         WHERE carrier.company EQ ipcCompany
         AND carrier.carrier EQ  ipcCarrier NO-ERROR.
    IF AVAIL carrier THEN
    
    IF AVAILABLE carrier AND
       NOT DYNAMIC-FUNCTION("IsActive", carrier.rec_key) THEN
    opcCalcValue = "Yes".
    ELSE opcCalcValue = "No".
END PROCEDURE.

PROCEDURE calcGetQtyOnHandFromFgitem:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcFGItem    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
   FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany 
        AND itemfg.i-no EQ ipcFGItem NO-ERROR.
   IF AVAIL itemfg THEN
      opcCalcValue = string(itemfg.q-onh).
END PROCEDURE.

PROCEDURE calcGetPartDscr1FromFgitem:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcFGItem    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
   FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany 
        AND itemfg.i-no EQ ipcFGItem NO-ERROR.
   IF AVAIL itemfg THEN
      opcCalcValue = itemfg.part-dscr1.
END PROCEDURE.

PROCEDURE calcGetPartDscr2FromFgitem:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcFGItem    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
   FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany 
        AND itemfg.i-no EQ ipcFGItem NO-ERROR.
   IF AVAIL itemfg THEN
      opcCalcValue = itemfg.part-dscr2.
END PROCEDURE.

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
    
    opcCalcValue = IF ipiTime NE ? THEN STRING(ipiTime,"hh:mm:ss am")
                   ELSE "".
END PROCEDURE.
	
PROCEDURE calcTimeString:
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipiTime,"hh:mm:ss").
END PROCEDURE.

PROCEDURE Calculator : /* shunting yard algorithm */
    DEFINE INPUT  PARAMETER ipcCalcParam AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cOperator AS CHARACTER NO-UNDO EXTENT 200 INITIAL ?.
    DEFINE VARIABLE cStack    AS CHARACTER NO-UNDO EXTENT 200 INITIAL ?.
    DEFINE VARIABLE cToken    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE odx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sdx       AS INTEGER   NO-UNDO.

    DO idx = 1 TO NUM-ENTRIES(ipcCalcParam,"|"):
        cToken = ENTRY(idx,ipcCalcParam,"|").
        IF LOOKUP(cToken,"(,*,/,+,-,)") EQ 0 THEN
        /* add number to stack */
        ASSIGN
            sdx = sdx + 1
            cStack[sdx] = cToken
            .
        ELSE /* check operator precedence and add operator */
        IF LOOKUP(cToken,"*,/,+,-") NE 0 THEN DO:
            DO jdx = odx TO 1 BY -1:
                IF cOperator[jdx] EQ "(" THEN LEAVE.
                IF (LOOKUP(cOperator[jdx],"*,/") NE 0 AND LOOKUP(cToken,"+,-") NE 0)  OR
                  ((LOOKUP(cOperator[jdx],"*,/") NE 0 AND LOOKUP(cToken,"*,/") NE 0)  OR
                   (LOOKUP(cOperator[jdx],"+,-") NE 0 AND LOOKUP(cToken,"+,-") NE 0)) THEN DO:
                    /* pop off operator, add to stack */
                    ASSIGN
                        sdx = sdx + 1
                        cStack[sdx] = cOperator[jdx]
                        cOperator[jdx] = ?
                        odx = odx - 1
                        .
                END. /* if check precedence */
            END. /* do jdx */
            /* add operator */
            ASSIGN
                odx = odx + 1
                cOperator[odx] = cToken
                .
        END. /* if operator */
        ELSE
        IF cToken EQ "(" THEN
        /* add left parentheses to operators */
        ASSIGN
            odx = odx + 1
            cOperator[odx] = cToken
            .
        ELSE
        IF cToken EQ ")" THEN
        DO jdx = odx TO 1 BY -1:
            IF cOperator[jdx] EQ "(" THEN DO:
                /* clear left parentheses */
                ASSIGN
                    cOperator[jdx] = ?
                    odx = odx - 1
                    .
                LEAVE.
            END.
            /* pop off operator, add to stack */
            ASSIGN
                sdx = sdx + 1
                cStack[sdx] = cOperator[jdx]
                cOperator[jdx] = ?
                odx = odx - 1
                .
        END. /* do jdx */
    END. /* do idx */
    /* clear remaining operators to the stack */
    DO idx = odx TO 1 BY -1:
        ASSIGN
            sdx = sdx + 1
            cStack[sdx] = cOperator[idx]
            cOperator[idx] = ?
            .
    END. /* do idx */
    /* cStack now contains the shunting yard result */
    /* process stack */
    DO idx = 1 TO sdx:
        IF LOOKUP(cStack[idx],"*,/,+,-") EQ 0 THEN NEXT.
        ASSIGN
            cStack[idx] = STRING(fMathOperation(DECIMAL(cStack[idx - 2]),DECIMAL(cStack[idx - 1]),cStack[idx]))
            cStack[idx - 1] = ""
            cStack[idx - 2] = ""
            .        
        DO jdx = 2 TO sdx:
            IF cStack[jdx] EQ "" THEN
            ASSIGN
                cStack[jdx] = cStack[jdx - 1]
                cStack[jdx - 1] = ""
                .
        END. /* do jdx */
    END. /* do idx */
    opcCalcValue = STRING(DECIMAL(cStack[sdx]),ipcFormat).

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
    DEFINE VARIABLE dNumber AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hField  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable  AS HANDLE    NO-UNDO.
    
    /* parse parameter string, replace fields with actual values */
    ipcCalcParam = TRIM(ipcCalcParam,"|").
    DO idx = 1 TO NUM-ENTRIES(ipcCalcParam,"|"):
        cParam = ENTRY(idx,ipcCalcParam,"|").
        dNumber = DECIMAL(cParam) NO-ERROR.
        IF INDEX(cParam,".") NE 0 AND ERROR-STATUS:ERROR THEN DO:
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
        WHEN "calcAPIType" THEN
        RUN VALUE(ipcCalcProc) (
            INTEGER(ipcCalcParam),
            OUTPUT opcCalcValue).
        WHEN "calcDropShipment" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|"),
            OUTPUT opcCalcValue). 
        WHEN "calcGetChgMethod" THEN
        RUN VALUE(ipcCalcProc) (
            ipcCalcParam,            
            OUTPUT opcCalcValue).     
        WHEN "calcGetCarrierInActive" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).   
        WHEN "calcGetQtyOnHandFromFgitem" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|"),
            OUTPUT opcCalcValue). 
        WHEN "calcGetPartDscr1FromFgitem" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).
        WHEN "calcGetPartDscr2FromFgitem" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|"),
            ENTRY(2,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).    
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
        WHEN "Calculator" THEN
        RUN VALUE(ipcCalcProc) (
            ipcCalcParam,
            ipcFormat,
            OUTPUT opcCalcValue).
    END CASE.
END PROCEDURE.
