
{sys/inc/var.i SHARED}

DEF INPUT  PARAM cust_rec AS RECID NO-UNDO.
DEF OUTPUT PARAM hold_invoice_id AS RECID INIT ? NO-UNDO.


FIND cust WHERE RECID(cust) EQ cust_rec NO-LOCK NO-ERROR.

IF AVAIL cust THEN
FOR EACH ar-inv NO-LOCK
    WHERE ar-inv.company  EQ cust.company
      AND ar-inv.posted   EQ YES
      AND ar-inv.due      GT 0
      AND ar-inv.cust-no  EQ cust.cust-no
      AND ar-inv.due-date LT (TODAY - cust.cr-hold-invdays)
    USE-INDEX posted-due
    BREAK BY ar-inv.company:

  ACCUM ar-inv.due (TOTAL).

  IF LAST(ar-inv.company)                            AND
     (ACCUM TOTAL ar-inv.due) GT cust.cr-hold-invdue THEN
    hold_invoice_id = RECID(ar-inv).
END.

RETURN.
