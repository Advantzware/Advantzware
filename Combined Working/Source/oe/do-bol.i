/* ---------------------------------------------------- oe/do-bol.i 01/98 JLF */
/* order entry - Create actual release and BOL from planned release line      */
/* -------------------------------------------------------------------------- */

{oe/oe-relp.i "{1}"}

DO TRANSACTION:
  /* get first order for release */
  FIND FIRST bf-rell WHERE bf-rell.r-no EQ oe-relh.r-no USE-INDEX r-no NO-LOCK NO-ERROR.
  FIND FIRST xoe-ord WHERE xoe-ord.company EQ oe-relh.company
                       AND xoe-ord.ord-no  EQ bf-rell.ord-no
                     NO-LOCK NO-ERROR.

  {oe/mk-bolls.i}
  
  /* WFK - 10/14/15 - replace with calcBolWeight.p */
  /* {oe/chk-bolwt.i} */
  
  FOR EACH oe-boll EXCLUSIVE-LOCK 
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no:
          
      RUN oe/calcBolWeight.p (INPUT ROWID(oe-boll), OUTPUT oe-boll.weight).
       
  END.
  
  RUN oe/palcal2.p (ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets).

  RUN oe/bolhtots.p (ROWID(oe-bolh)).

  RUN oe/calcBolFrt.p (ROWID(oe-bolh), OUTPUT oe-bolh.freight).

  IF oe-bolh.freight EQ ? THEN oe-bolh.freight = 0.

  IF bf-rell.link-no NE 0 THEN
  FIND FIRST oe-rel
      WHERE oe-rel.r-no EQ bf-rell.link-no
      USE-INDEX seq-no NO-ERROR.

  IF NOT AVAIL oe-rel THEN
  FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-relh.company
        AND oe-rel.ord-no   EQ bf-rell.ord-no
        AND oe-rel.line     EQ bf-rell.line
        AND oe-rel.i-no     EQ bf-rell.i-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.rel-date LE oe-relh.rel-date
        AND oe-rel.rel-no   EQ 0
        AND oe-rel.b-ord-no EQ 0
        AND oe-rel.link-no  EQ 0
      USE-INDEX ord-item NO-ERROR.

  IF NOT AVAIL oe-rel THEN
  FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-relh.company
        AND oe-rel.ord-no   EQ bf-rell.ord-no
        AND oe-rel.line     EQ bf-rell.line
        AND oe-rel.i-no     EQ bf-rell.i-no
        AND oe-rel.rel-date LE oe-relh.rel-date
        AND oe-rel.rel-no   EQ 0
        AND oe-rel.b-ord-no EQ 0
        AND oe-rel.link-no  EQ 0
      USE-INDEX ord-item NO-ERROR.

  IF AVAIL oe-rel THEN
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  FIND CURRENT oe-rel NO-LOCK NO-ERROR.
  /*IF AVAIL xoe-ord AND xoe-ord.t-freight NE 0 THEN
    oe-bolh.freight = xoe-ord.t-freight.*/
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

