/* purgeLO.p - purge obsolete list objects */

DEFINE VARIABLE purgeList AS CHARACTER NO-UNDO.

RUN purgeListObjs (NO,OUTPUT purgeList).
IF purgeList NE '' THEN DO:
  MESSAGE 'The Following Invalid Objects Exist:' SKIP(1) purgeList SKIP(1)
    'Remove The Above Invalid Objects?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE purgeObjs AS LOGICAL.
  IF purgeObjs THEN DO:
    RUN purgeListObjs (YES,OUTPUT purgeList).
    MESSAGE 'Purge Invalid List Objects Complete!' SKIP(1)
      'Invalid Objects Removed:' SKIP(1) purgeList VIEW-AS ALERT-BOX.
  END.
END.
ELSE
MESSAGE 'Purge Invalid List Objects Complete!' SKIP(1)
  'No Invalid Objects Exist.' VIEW-AS ALERT-BOX.

PROCEDURE purgeListObjs:
  DEFINE INPUT PARAMETER ipPurge AS LOGICAL NO-UNDO.

  DEFINE OUTPUT PARAMETER opPurgeList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE searchDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE prgmName AS CHARACTER FORMAT "X(16)" NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE validObjs AS CHARACTER NO-UNDO.

  ASSIGN
    validObjs = 'account_.r,ar-inv_.r,inv-hea_.r,job_.r,oe-boll_.r,oe-relh_.r,po-ordl_.r,style_.r'
    searchDir = "./listobjs".
  INPUT FROM OS-DIR(searchDir) NO-ECHO.
  REPEAT:
    SET prgmName ^ attrList.
    IF attrList NE "f" OR CAN-DO(validObjs,prgmName) THEN NEXT.
    IF ipPurge THEN
    OS-DELETE VALUE(searchDir + '\' + prgmName).
    opPurgeList = opPurgeList + prgmName + CHR(10).
  END.
  INPUT CLOSE.
END PROCEDURE.
