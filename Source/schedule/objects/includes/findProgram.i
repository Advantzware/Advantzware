/* findProgram.i */

FUNCTION findProgram RETURNS CHARACTER
  (ipDir AS CHARACTER,ipID AS CHARACTER,ipProgram AS CHARACTER):
  DEFINE VARIABLE findProgram AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  IF INDEX(PROPATH,'schedule') NE 0 AND
     INDEX(PROPATH,':') EQ 0 THEN ipDir = '../' + ipDir.
  findProgram = ipDir + ipID + ipProgram.
  DO WHILE SEARCH(findProgram) EQ ? AND
           SEARCH(REPLACE(findProgram,'.p','.r')) EQ ? AND
           SEARCH(REPLACE(findProgram,'.w','.r')) EQ ?:
    ASSIGN
      ipID = IF INDEX(ipID,'/') EQ 0 THEN ''
             ELSE SUBSTR(ipID,1,R-INDEX(ipID,'/') - 1)
      findProgram = ipDir + ipID + ipProgram
      findProgram = REPLACE(findProgram,'//','/')
      i = i + 1.
    IF i GT 5 THEN RETURN ?.
  END.
  RETURN IF INDEX(findProgram,'.dat') NE 0 THEN SEARCH(findProgram)
         ELSE findProgram.
END FUNCTION.
