DEFINE INPUT  PARAMETER ipcCustNo AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdDueDate AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER ipdPromDate AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateChanged AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER opdDueDate AS DATE        NO-UNDO.
DEFINE OUTPUT PARAMETER opdPromDate AS DATE        NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i SHARED}
{sys/inc/varasgn.i}
DEF VAR lcNk1Value AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.
DEF VAR oeDateAuto-log AS LOG NO-UNDO.
DEFINE VARIABLE oeDateAuto-chr AS CHARACTER   NO-UNDO.

/* ************************  Function Prototypes ********************** */



FUNCTION get-date RETURNS DATE
  ( INPUT ipdDate AS DATE, INPUT ipiDays AS INT, INPUT ipcPlusMinus as char)  FORWARD.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
IF NOT AVAIL oe-ordl  THEN
  RETURN.
FIND cust
   WHERE cust.company EQ oe-ordl.company
     AND cust.cust-no EQ oe-ordl.cust-no
   NO-LOCK NO-ERROR.
ASSIGN
  opdDueDate = ipdDueDate
  opdPromDate = ipdPromDate.

RUN sys/ref/nk1look.p (INPUT oe-ordl.company, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT lcNk1Value, OUTPUT llRecFound).
IF llRecFound THEN
oeDateAuto-log = LOGICAL(lcNk1Value) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT oe-ordl.company, "OEDATEAUTO", "C" /* Char */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT lcNk1Value, OUTPUT llRecFound).
IF llRecFound THEN
oeDateAuto-chr = lcNk1Value NO-ERROR.

IF oeDateAuto-log 
     AND oeDateAuto-chr EQ "Colonial"
     AND  (oe-ordl.prom-date NE ? OR oe-ordl.req-date NE ?)  THEN DO:

  FIND FIRST oe-rel
    WHERE oe-rel.company EQ oe-ordl.company
      AND oe-rel.ord-no  EQ oe-ordl.ord-no
      AND oe-rel.i-no    EQ oe-ordl.i-no
    NO-LOCK NO-ERROR.
  IF AVAIL oe-rel THEN 
    FIND shipto
      WHERE shipto.company EQ oe-rel.company
        AND shipto.cust-no EQ oe-rel.cust-no
        AND shipto.ship-id EQ oe-rel.ship-id
      NO-LOCK NO-ERROR.
  IF NOT AVAIL shipto THEN
    FIND FIRST shipto 
      WHERE shipto.company EQ cust.company
        AND shipto.cust-no EQ cust.cust-no
        AND shipto.ship-id EQ cust.cust-no
      NO-LOCK NO-ERROR.
    
  IF AVAIL shipto THEN DO:
    IF ipcDateChanged EQ "PromiseDate" THEN
      opdDueDate = get-date(ipdPromDate, INT(shipto.del-time), "+").
    ELSE
      opdPromDate = get-date(ipdDueDate, INT(shipto.del-time), "-").
  END. /* avail shipto */
 
END.

DEF TEMP-TABLE holidays
FIELD holiday AS DATE.

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



