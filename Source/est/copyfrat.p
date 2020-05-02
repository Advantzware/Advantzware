/* ------------------------------------------------- est/copyfrat.p 10/07 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM iplUpdateOtherEst AS LOGICAL NO-UNDO.
                      
{sys/inc/var.i SHARED}

{est/d-selblk.i NEW}

DEF BUFFER b-eb FOR eb.

DEF VAR li AS INT NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  IF iplUpdateOtherEst THEN 
  RUN est/updest3.p (ROWID(eb), ROWID(eb), 4,NO).

  RUN est/d-selblk.w (ip-rowid, "Copy Freight").

  FOR EACH tt-select WHERE tt-selected,
      FIRST b-eb WHERE ROWID(b-eb) EQ tt-rowid
      BREAK BY b-eb.stock-no:

    {est/copyfrat.i}

    IF LAST-OF(b-eb.stock-no) AND iplUpdateOtherEst THEN
      RUN est/updest3.p (ROWID(b-eb), ROWID(eb), 4,NO).
  END.
END.

