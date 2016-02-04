/* ------------------------------------------------- est/copyunit.p 06/05 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
                      
{sys/inc/var.i SHARED}

{est/d-selblk.i NEW}

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ref FOR reftable.

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  RUN est/updest3.p (ROWID(eb), ROWID(eb), 2).

  RUN est/d-selblk.w (ip-rowid, "Copy Units").

  FOR EACH tt-select WHERE tt-selected,
      FIRST b-eb WHERE ROWID(b-eb) EQ tt-rowid
      BREAK BY b-eb.stock-no:

    {est/copyunit.i}

    IF LAST-OF(b-eb.stock-no) THEN RUN est/updest3.p (ROWID(b-eb), ROWID(eb), 2).
  END.
END.

