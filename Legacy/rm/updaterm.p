
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR ll-fold AS LOG FORMAT "Folding Carton/Corrugated".


FIND ITEM WHERE ROWID(item) EQ ip-rowid NO-ERROR.

IF AVAIL ITEM THEN DO:
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ item.company
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.

  ll-fold = YES.

  IF AVAIL sys-ctrl THEN
    IF sys-ctrl.char-fld EQ "Both" THEN
      MESSAGE "Please enter which industry (Folding Carton/Corrugated):"
          UPDATE ll-fold.
    ELSE
    IF sys-ctrl.char-fld EQ "Corrware" THEN
      ll-fold = NO.

  IF ll-fold THEN DO:
    item.industry = "1".

    /*RUN rm/uitem1.p (ip-rowid).*/
  END.

  ELSE DO:
    item.industry = "2".

    /*RUN cec/uitem1.p (ip-rowid).*/
  END.
END.
