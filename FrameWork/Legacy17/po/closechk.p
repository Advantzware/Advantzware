
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR ld AS DEC DECIMALS 10 NO-UNDO.
DEF VAR ll-close AS LOG NO-UNDO.
  
{sys/inc/var.i NEW SHARED}


FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL po-ordl THEN
    FIND FIRST po-ord WHERE
         po-ord.company EQ po-ordl.company AND
         po-ord.po-no EQ po-ordl.po-no AND
         po-ord.stat NE "C"
         NO-LOCK NO-ERROR.

IF AVAIL po-ord THEN DO:
  ASSIGN
   cocode   = po-ord.company
   ll-close = NO.

  FOR EACH po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no EQ po-ord.po-no
      NO-LOCK:
    RUN po/rec-inv.p (ROWID(po-ordl), OUTPUT ld).

    IF ld LE 0 AND po-ordl.stat EQ "C" THEN ll-close = YES.
    ELSE DO:
      ll-close = NO.
      LEAVE.
    END.
  END.

  IF ll-close THEN RUN po/closepo.p (ROWID(po-ord), NO).
END.
