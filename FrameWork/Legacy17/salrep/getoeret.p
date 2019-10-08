
DEF INPUT PARAM  ip-rowid AS ROWID NO-UNDO.
DEF PARAM BUFFER io-ref-tab FOR reftable.
DEF PARAM BUFFER io-oe-retl FOR oe-retl.

DEF VAR lv-r-no LIKE oe-retl.r-no NO-UNDO.


RELEASE io-ref-tab.
RELEASE io-oe-retl.

FIND ar-cashl WHERE ROWID(ar-cashl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ar-cashl THEN DO:
  FIND FIRST io-ref-tab
      WHERE io-ref-tab.reftable EQ "ar-cashl.return"
        AND io-ref-tab.company  EQ ar-cashl.company
        AND io-ref-tab.loc      EQ ""
        AND io-ref-tab.code     EQ STRING(ar-cashl.c-no,"9999999999")
        AND io-ref-tab.code2    EQ STRING(ar-cashl.line,"9999999999")
      NO-LOCK NO-ERROR.

  IF AVAIL io-ref-tab AND io-ref-tab.dscr EQ "ITEMS" THEN
    lv-r-no = io-ref-tab.val[1].

  ELSE
  IF ar-cashl.dscr MATCHES "*oe return*"                                      AND
     SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "ITEMS" THEN
    lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12)) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN lv-r-no = 0.

  IF lv-r-no NE 0 THEN
  FIND FIRST io-oe-retl
      WHERE io-oe-retl.company EQ ar-cashl.company
        AND io-oe-retl.r-no    EQ lv-r-no
        AND io-oe-retl.line    EQ ar-cashl.line
      NO-LOCK NO-ERROR.
END.
