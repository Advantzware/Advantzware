DEF VAR cPfName AS CHAR NO-UNDO.
DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cDB AS CHAR.
DEF VAR cDBOnly AS CHAR.
DEF VAR cHost AS CHAR.
DEF VAR cServer AS CHAR.
DEF VAR cTab AS CHAR INIT "~011".
DEF VAR cTmpLine AS CHAR NO-UNDO.
DEF VAR cChar AS CHAR NO-UNDO.
DEF VAR iAscCode AS INT NO-UNDO.
DEF VAR i AS INT.

cPfName = "p:\asi10test\rco1010\nosweat.pf".

IF SESSION:PARAMETER GT "" THEN
  cPfName = SESSION:PARAMETER.

INPUT FROM VALUE(cPfName).

REPEAT :
  cLine = "".
  IMPORT DELIMITER "|" cLine.
  cLine = TRIM(cLine).
  IF cLine BEGINS "#" THEN 
    NEXT.

  cChar = "".
  iAscCode = 0.
  cTmpLine = "".
  DO i = 1 TO LENGTH(cLine):
    cChar = SUBSTRING(cLine, i, 1).
    iAscCode = ASC(cChar).

    IF (iAscCode GE 65 AND iAscCode LE 90) OR
       (iAscCode GE 48 AND iAscCode LE 57) OR
       (iAscCode GE 97 AND iAscCode LE 122) OR
       iAscCode EQ 32 OR 
      cChar = "-" OR cChar = ":" OR cChar = "\" OR cChar = "/"
      THEN
      cTmpLine = cTmpLine + cChar.
      ELSE 
        cTmpLine = cTmpLine + " ".
  END.
  cLine = cTmpLine.
  DO i = 1 TO 20:
    IF INDEX(cLine, "  ") EQ 0 THEN
      LEAVE.
    cLine = REPLACE(cLine, "  ", " ").

  END.

  cLine = TRIM(cLine).


  /* cLine = REPLACE(cLine, cTab, " "). */
  /* MESSAGE cLine SKIP
    NUM-ENTRIES(cLine, cTab)
    VIEW-AS ALERT-BOX INFO BUTTONS OK. */

  /*
  MESSAGE "entry 2" ENTRY(2, cLine, " ")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
  IF cLine BEGINS "-S" THEN DO:
    cChar = SUBSTRING(cLine, 2, 1).
    IF ASC(cChar) GE 65 AND ASC(cChar) LE 90 THEN
    ASSIGN cServer = ENTRY(2, cLine, " ").
  END.
    
  IF cLine BEGINS "-db" THEN
    ASSIGN cDB = ENTRY(2, cLine, " ").


  IF cLine BEGINS "-H" THEN DO: 
    cChar = SUBSTRING(cLine, 2, 1).
    IF ASC(cChar) GE 65 AND ASC(cChar) LE 90 THEN
    ASSIGN cHost = ENTRY(2, cLine, " ").
  END.


END.
cDBOnly = cDB.
IF INDEX(cDB, "\") gt 0 or INDEX(cDB, "/") GT 0 THEN DO:
  i = 0.
  i =   INDEX(cDB, "\").
  if i = 0 then 
    i = INDEX(cDB, "/").
  cDBOnly = substring(cDB, i + 1).
END.
OUTPUT TO VALUE(cDbOnly + ".pf").
PUT UNFORMATTED "-DB " + cDb SKIP
    "-H " + cHost SKIP
    "-S " + cServer SKIP.

