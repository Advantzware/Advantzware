/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\asi\sonotest.p
**       By: Chris Heins for ASI / Sonoco
** Descript: Testing driver.
**
*****************************************************************************
\***************************************************************************/
{sys/inc/var.i}
OS-DELETE "sono_inv.txt".
OS-DELETE "sono_cst.txt".
DEF var maxrecs AS int NO-UNDO initial 50.
DEF var exprecs AS int NO-UNDO extent 3.
MESSAGE "Maximum records for each file?" UPDATE maxrecs.
IF maxrecs = 0 THEN
RETURN.
FOR EACH company:
  cocode = company.company.
  FOR EACH cust NO-LOCK:
    FIND FIRST inv-head OF cust NO-LOCK NO-ERROR.
    FIND FIRST ar-inv OF cust NO-LOCK NO-ERROR.
    IF NOT AVAIL inv-head AND NOT AVAIL ar-inv THEN
    NEXT.
    STATUS DEFAULT "Processing cust: " + cocode + '/' + cust.cust-no.
    RUN edi/ed/asi/expsono.p ("write", RECID(cust)).
    exprecs[1] = exprecs[1] + 1.
    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company = cocode
        AND inv-head.cust-no = cust.cust-no
        BY inv-head.inv-date BY inv-head.inv-no:
      IF exprecs[2] > maxrecs THEN
      LEAVE.
      STATUS DEFAULT "Processing inv-head: " + cocode + string(inv-head.r-no).
      RUN edi/ed/asi/exparinv.p ("inv-head", RECID(inv-head)).
      exprecs[2] = exprecs[2] + 1.
    END.
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company = cocode
        AND ar-inv.cust-no = cust.cust-no
        BY ar-inv.inv-date BY ar-inv.inv-no:
      IF exprecs[3] > maxrecs THEN
      LEAVE.
      STATUS DEFAULT "Processing ar-inv: " + cocode + string(ar-inv.x-no).
      RUN edi/ed/asi/exparinv.p ("ar-inv", RECID(ar-inv)).
      exprecs[3] = exprecs[3] + 1.
    END.
    PAUSE 0.
    IF exprecs[1] > maxrecs AND exprecs[2] > maxrecs AND exprecs[3] > maxrecs
      THEN
    LEAVE.
  END.  /* each cust */
  PAUSE 0.
END.
STATUS DEFAULT.
