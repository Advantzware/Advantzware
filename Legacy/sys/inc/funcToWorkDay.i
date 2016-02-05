FUNCTION get-date RETURNS DATE
    ( INPUT ipdDate AS DATE, INPUT ipiDays AS INT, INPUT ipcPlusMinus as char ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF    VAR      dCalcDate    AS DATE.       

    DEFINE VAR      search-date  AS DATE    NO-UNDO.
    DEFINE VAR      biz-days     AS INTEGER NO-UNDO.
    DEFINE VAR      tot-biz-days AS INTEGER NO-UNDO.


    DEFINE VARIABLE first-date   AS DATE    NO-UNDO.
    DEFINE VARIABLE last-date    AS DATE    NO-UNDO.
    DEFINE VARIABLE ctr          AS INTEGER NO-UNDO.
    first-date = ipdDate.   
    dCalcDate = ipdDate.

    IF ipiDays NE 0 THEN DO:
      
      ASSIGN 
          ctr      = (if ipcPlusMinus = "+" then 1 else -1)
          biz-days = 0.
  
      REPEAT:
          last-date = first-date + ctr.
          IF ipcPlusMinus = "+" THEN
            ctr = ctr + 1.
          ELSE
            ctr = ctr - 1.       
          
          IF WEEKDAY(last-date) EQ 1 OR WEEKDAY(last-date) EQ 7 THEN NEXT.
          IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ "Holiday" AND DATE(reftable.loc) EQ last-date) THEN
              NEXT.
              
          tot-biz-days = tot-biz-days + 1.
          IF last-date LE search-date THEN biz-days = biz-days + 1.
  
          dCalcDate = last-date.
          IF abs(tot-biz-days) GE ipiDays THEN LEAVE.
      END.
    END.

    RETURN dCalcDate.   /* Function return value. */
END FUNCTION.