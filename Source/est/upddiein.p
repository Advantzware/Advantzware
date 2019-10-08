
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF NEW SHARED TEMP-TABLE formule 
                          FIELD formule AS de EXTENT 12.


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-ERROR.

IF AVAIL eb THEN DO:
  cocode = eb.company.

  IF CAN-FIND(FIRST style
              WHERE style.company EQ eb.company
                AND style.style   EQ eb.style) THEN
    IF eb.est-type LE 4 THEN DO:
      RUN est/u2kinc1.p (RECID(eb)).
      RUN est/u2kinc2.p (RECID(eb)).
    END.

    ELSE DO:
      RUN est/u2kinc1c.p (RECID(eb)).
      RUN est/u2kinc2c.p (RECID(eb)).
    END.

  FOR EACH formule:
    eb.die-in = formule[12] * eb.num-up.
    LEAVE.
  END.

  IF eb.die-in EQ ? THEN eb.die-in = 0.
END.
