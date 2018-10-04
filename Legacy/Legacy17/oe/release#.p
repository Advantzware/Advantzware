
DEF INPUT  PARAM ip-cocode  LIKE oe-relh.company    NO-UNDO.
DEF OUTPUT PARAM op-next#   LIKE oe-relh.release#   NO-UNDO.


op-next# = 0.
/* Replace with code that reserves the number */
/* FIND LAST oe-relh                                      */
/*     WHERE oe-relh.company EQ ip-cocode                 */
/*     USE-INDEX release# NO-LOCK NO-ERROR.               */
/* IF AVAIL oe-relh AND oe-relh.release# GT op-next# THEN */
/*   op-next# = oe-relh.release#.                         */

RUN oe/get-r-no.p (INPUT "release#", OUTPUT op-next#).

FIND LAST oe-bolh
    WHERE oe-bolh.company EQ ip-cocode
    USE-INDEX release# NO-LOCK NO-ERROR.
IF AVAIL oe-bolh AND oe-bolh.release# GT op-next# THEN
  op-next# = oe-bolh.release# + 1.

/* op-next# = op-next# . */
