/* addTime.i */

PROCEDURE addTime:
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime1 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipTime2 AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER opDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
  ASSIGN
    opDate = ipDate
    opTime = ipTime1
    ipTime2 = ipTime2 * 60. /* change from minutes to seconds */
  
  IF ipTime2 NE 0 THEN
  ASSIGN
    ipTime1 = ipTime1 + ipTime2
    days = TRUNCATE(ipTime1 / 86400,0)
    opDate = ipDate + days
    opTime = ipTime1 - days * 86400.
END PROCEDURE.
