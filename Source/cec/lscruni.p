DEFINE INPUT PARAMETER ip-score AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER op-line-1 AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER op-line-2 AS CHAR NO-UNDO.

DEF VAR i AS INT.
DEF VAR new-str AS CHAR.
DEF VAR tmp-str AS CHAR.
DEF VAR array-count AS INT INIT 1 NO-UNDO.
DEF VAR tmp-dec AS DEC NO-UNDO.
DEF VAR lscore-ext AS CHAR EXTENT 30.

ASSIGN ip-score = LEFT-TRIM(ip-score)
       ip-score = RIGHT-TRIM(ip-score).

DO i = 1 TO LENGTH(ip-score):

  IF array-count GE 31 THEN
     LEAVE.

  IF SUBSTRING(ip-score,i,1) NE ' ' THEN
    tmp-str = tmp-str + SUBSTRING(ip-score,i,1).
  ELSE IF tmp-str NE "" THEN
    DO:
      tmp-dec = DEC(tmp-str) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:
        IF tmp-dec NE 0 THEN
          lscore-ext[array-count] = STRING(tmp-dec,"ZZ9.99").
        ELSE
          lscore-ext[array-count] = "      ".
      END.
      ELSE
        lscore-ext[array-count] = tmp-str.

      ASSIGN
        array-count = array-count + 1
        tmp-str = "".
    END.
END.

IF tmp-str NE "" AND array-count LT 31 THEN DO:
  tmp-dec = DEC(tmp-str) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    IF tmp-dec NE 0 THEN
      lscore-ext[array-count] = STRING(tmp-dec,"ZZ9.99").
    ELSE
      lscore-ext[array-count] = "      ".
  END.
END.

DO i = 1 TO 15:
  tmp-dec = DEC(lscore-ext[i]) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND TRIM(lscore-ext[i]) NE "" THEN
    op-line-1 = op-line-1 + STRING(tmp-dec,"ZZ9.99") + " ".
  ELSE
    op-line-1 = op-line-1 + lscore-ext[i] + " ".
END.

DO i = 16 TO 30:
  tmp-dec = DEC(lscore-ext[i]) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND TRIM(lscore-ext[i]) NE "" THEN
    op-line-2 = op-line-2 + STRING(tmp-dec,"ZZ9.99") + " ".
  ELSE
    op-line-2 = op-line-2 + lscore-ext[i] + " ".
END.
