/* emailConnect.p - rstark 7.15.2005 */

DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipPrgmName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opEmailList AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE pfLine AS CHARACTER NO-UNDO.

IF CONNECTED('JOBS') THEN DO:
  RUN pfLine (OUTPUT pfLine).
  CONNECT VALUE(pfLine) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR
        TITLE 'Database Connect Error (custom/emailConnect.p)'.
  END.
  IF CONNECTED('asinos') THEN DO:
    RUN custom/emailListAddon.p (ipRecKey,ipPrgmName,OUTPUT opEmailList).
    DISCONNECT asinos.
  END. /* if connect asinos */
END. /* if connected jobs */
ELSE
RUN custom/emailList.p (ipRecKey,ipPrgmName,OUTPUT opEmailList).

PROCEDURE pfLine:
  DEFINE OUTPUT PARAMETER opPFLine AS CHARACTER NO-UNDO.

  DEFINE VARIABLE pfText AS CHARACTER NO-UNDO.
  
  INPUT FROM VALUE(SEARCH('nosweat.pf')) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED pfText.
    IF pfText BEGINS '#' THEN NEXT.
    IF INDEX(pfText,'-db') NE 0 OR
       INDEX(pfText,'-H') NE 0 OR
       INDEX(pfText,'-S') NE 0 THEN
    opPFLine = opPFLine + ' ' + pfText.
  END.
  INPUT CLOSE.
  opPFLine = opPFLine + ' -N TCP -ld asinos -h 7'.
END PROCEDURE.
