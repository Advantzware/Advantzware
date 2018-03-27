/*-----------------------------------------------------oe/palcalc.p 03/99 RLL-*/
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

DEF INPUT  PARAM ip-rowid   AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.


DEF VAR v-int AS DEC NO-UNDO.

FIND oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-bolh THEN
FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no:

  RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT oe-boll.tot-pallets).

  op-pallets = op-pallets + oe-boll.tot-pallets.
END.

