DEF INPUT-OUTPUT PARAM io-date AS DATE NO-UNDO.
DEF INPUT PARAM ip-factor AS INT NO-UNDO.

DEF VAR ld AS DATE INIT ? NO-UNDO.
DEF VAR li AS INT NO-UNDO.


li = DAY(io-date).

DO WHILE li GT 0:
  ld = DATE(MONTH(io-date),li,YEAR(io-date) + ip-factor) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN LEAVE.
  li = li - 1.
END.

io-date = ld.
