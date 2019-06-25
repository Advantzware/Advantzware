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
      AND oe-boll.b-no    EQ oe-bolh.b-no
    NO-LOCK:

  RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).

  op-pallets = op-pallets + v-int.
END.

/*
for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no no-lock,
    first fg-bin
    where fg-bin.company    eq oe-boll.company
      and fg-bin.i-no       eq oe-boll.i-no
      and fg-bin.job-no     eq oe-boll.job-no
      and fg-bin.job-no2    eq oe-boll.job-no2
      and fg-bin.loc        eq oe-boll.loc
      and fg-bin.loc-bin    eq oe-boll.loc-bin
      and fg-bin.tag        eq oe-boll.tag
      and fg-bin.cases-unit ne 0
    no-lock:

  v-dec = (oe-boll.cases + min(oe-boll.partial,1)) / fg-bin.cases-unit.

  {sys/inc/roundup.i v-dec}

  op-pallets = op-pallets + v-dec.
end.
*/
