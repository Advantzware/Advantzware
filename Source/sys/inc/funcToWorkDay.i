/* funcToWorkDay.i */

FUNCTION get-date RETURNS DATE
    (ipdDate AS DATE, ipiDays AS INTEGER, ipcPlusMinus AS CHARACTER):

    DEFINE VARIABLE dtCalcDate   AS DATE    NO-UNDO.
    DEFINE VARIABLE dtBizDate    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtTotBizDate AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtFirstDate  AS DATE    NO-UNDO.
    DEFINE VARIABLE dtLastDate   AS DATE    NO-UNDO.
    DEFINE VARIABLE ctr          AS INTEGER NO-UNDO.

    ASSIGN
        dtFirstDate = ipdDate                                                                                          
        dtCalcDate  = ipdDate
        ctr         = IF ipcPlusMinus EQ "+" THEN 1 ELSE -1                                                        
        dtBizDate   = 0
        .
    REPEAT:
        ASSIGN
            dtLastDate = dtFirstDate + ctr
            ctr = ctr + IF ipcPlusMinus EQ "+" THEN 1 ELSE -1
            .
        IF WEEKDAY(dtLastDate) EQ 1 OR WEEKDAY(dtLastDate) EQ 7 THEN NEXT.
        IF CAN-FIND(FIRST reftable
                    WHERE reftable.reftable  EQ "Holiday"
                      AND DATE(reftable.loc) EQ dtLastDate) THEN
        NEXT.
        ASSIGN
            dtTotBizDate = dtTotBizDate + 1
            dtCalcDate   = dtLastDate
            .
        IF ABSOLUTE(dtTotBizDate) GE ipiDays THEN
        LEAVE.
    END. // repeat
    RETURN dtCalcDate.

END FUNCTION.
