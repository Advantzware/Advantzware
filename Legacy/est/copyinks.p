/* ------------------------------------------------- est/copyinks.p 04/02 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
                      
{sys/inc/var.i SHARED}

{est/d-selblk.i NEW}

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-ref FOR reftable.

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR ll AS LOG INIT YES NO-UNDO.
DEF VAR v-side-count AS INT NO-UNDO.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  IF eb.est-type LE 4 THEN
    MESSAGE "Update Units along with Inks?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

  RUN est/updest3.p (ROWID(eb), ROWID(eb), 1 + INT(ll)).

  RUN est/d-selblk.w (ip-rowid, "Copy Inks" + IF ll THEN " & Units" ELSE "").

  FOR EACH tt-select WHERE tt-selected,
      FIRST b-eb WHERE ROWID(b-eb) EQ tt-rowid
      BREAK BY b-eb.stock-no:

    {est/copyinks.i}

    IF LAST-OF(b-eb.stock-no) THEN
      RUN est/updest3.p (ROWID(b-eb), ROWID(eb), 1 + INT(ll)).
  END.
END.

/* end ---------------------------------- copr. 2004  advanced software, inc. */
