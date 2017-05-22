/* calcEnd.i - used in procedure calcEnd in loadPro.p & viewers/ASI/viewers/cust.w */

PROCEDURE calcEnd:
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMR AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRun AS DECIMAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.

  DEFINE VARIABLE totalTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE days AS INTEGER NO-UNDO.

  IF ipTime EQ ? THEN ipTime = 0.
  IF ipMR EQ ? THEN ipMR = 0.
  IF ipRun EQ ? THEN ipRun = 0.
  ASSIGN
    totalTime = ipTime + ipMR * 3600 + ipRun * 3600
    days = TRUNCATE(totalTime / 86400,0)
    opDate = ipDate + days
    opTime = totalTime - days * 86400
    .
  IF opDate EQ ? THEN opDate = ipDate.
  IF opTime EQ ? THEN opTime = ipTime.
END PROCEDURE.
