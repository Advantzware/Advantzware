/* dueDateCalc.p */

DEFINE INPUT  PARAMETER ipcCustNo      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdDueDate     AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER ipdPromDate    AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateChanged AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iprRowID       AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER opdDueDate     AS DATE      NO-UNDO.
DEFINE OUTPUT PARAMETER opdPromDate    AS DATE      NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i SHARED}
{sys/inc/varasgn.i}

DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustNo        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNk1Value     AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE oeDateAuto-log AS LOGICAL   NO-UNDO.
DEFINE VARIABLE oeDateAuto-chr AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE holidays
    FIELD holiday AS DATE
    .
                                                                                                                   
/* ************************  Function Prototypes ********************** */

FUNCTION get-date RETURNS DATE
  (ipdDate AS DATE, ipiDays AS INTEGER, ipcPlusMinus AS CHARACTER) FORWARD.

ASSIGN
  opdDueDate  = ipdDueDate
  opdPromDate = ipdPromDate
  .

CASE ipcDateChanged:
    WHEN "DueDate" OR WHEN "PromiseDate" THEN DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE ROWID(oe-ordl) EQ iprRowID
             NO-ERROR.
        IF NOT AVAILABLE oe-ordl THEN RETURN.
        IF (ipcDateChanged EQ "PromiseDate" AND oe-ordl.prom-date EQ ?)  OR
           (ipcDateChanged EQ "DueDate"     AND oe-ordl.req-date  EQ ?) THEN
        RETURN.
        ASSIGN
            cCompany = oe-ordl.company
            cCustNo  = oe-ordl.cust-no
            .
        FIND FIRST oe-rel NO-LOCK
             WHERE oe-rel.company EQ oe-ordl.company
               AND oe-rel.ord-no  EQ oe-ordl.ord-no
               AND oe-rel.i-no    EQ oe-ordl.i-no
             NO-ERROR.
    END.
    WHEN "RelDate" THEN DO:
        FIND FIRST oe-rel NO-LOCK
             WHERE ROWID(oe-rel) EQ iprRowID
             NO-ERROR.
        IF NOT AVAILABLE oe-rel THEN RETURN.
        ASSIGN
            cCompany = oe-rel.company
            cCustNo  = oe-rel.cust-no
            .
    END.
END CASE.

FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cCompany
       AND cust.cust-no EQ cCustNo
     NO-ERROR.
IF NOT AVAILABLE cust THEN RETURN.

RUN sys/ref/nk1look.p (
    cCompany,
    "OEDATEAUTO",
    "L" /* Logical */,
    NO  /* check by cust */,
    YES /* use cust not vendor */,
    ""  /* cust */,
    ""  /* ship-to*/,
    OUTPUT lcNk1Value,
    OUTPUT llRecFound
    ).
IF llRecFound THEN
oeDateAuto-log = LOGICAL(lcNk1Value) NO-ERROR.

IF NOT oeDateAuto-log THEN RETURN.

RUN sys/ref/nk1look.p (
    cCompany,
    "OEDATEAUTO",
    "C" /* Char */,
    NO  /* check by cust */,
    YES /* use cust not vendor */,
    ""  /* cust */,
    ""  /* ship-to*/,
    OUTPUT lcNk1Value,
    OUTPUT llRecFound
    ).
IF llRecFound THEN
oeDateAuto-chr = lcNk1Value NO-ERROR.

IF oeDateAuto-log AND oeDateAuto-chr EQ "Colonial" THEN DO:
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ oe-rel.company
           AND shipto.cust-no EQ oe-rel.cust-no
           AND shipto.ship-id EQ oe-rel.ship-id
         NO-ERROR.
    IF NOT AVAILABLE shipto THEN
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ cust.company
           AND shipto.cust-no EQ cust.cust-no
           AND shipto.ship-id EQ cust.cust-no
         NO-ERROR.
    IF AVAILABLE shipto THEN
    CASE ipcDateChanged:
        WHEN "DueDate" THEN
        opdPromDate = get-date (ipdDueDate, INTEGER(shipto.del-time), "-").
        WHEN "PromiseDate" THEN
        opdDueDate = get-date (ipdPromDate, INTEGER(shipto.del-time), "+").
        WHEN "RelDate" THEN
        opdDueDate = get-date (ipdPromDate, 0, "-").
    END CASE.
END.

{sys/inc/funcToWorkDay.i}
