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
  
  {oe/chk-bolwt.i}

  RUN oe/palcal2.p (ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets).

  RUN oe/bolhtots.p (ROWID(oe-bolh)).

  RUN oe/bolhfrat.p (ROWID(oe-bolh),
                     oe-bolh.cust-no,
                     oe-bolh.ship-id,
                     oe-bolh.carrier,
                     OUTPUT oe-bolh.freight).

  IF oe-bolh.freight EQ ? THEN oe-bolh.freight = 0.

  

  /*IF AVAIL xoe-ord AND xoe-ord.t-freight NE 0 THEN
    oe-bolh.freight = xoe-ord.t-freight.*/
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

