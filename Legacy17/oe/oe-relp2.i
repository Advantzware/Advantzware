  
    BREAK BY oe-relh.cust-no
          BY oe-relh.ship-id
          BY oe-relh.carrier.

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF oe-ordl.
  DISABLE TRIGGERS FOR LOAD OF oe-relh.
  DISABLE TRIGGERS FOR LOAD OF oe-rell.
  DISABLE TRIGGERS FOR LOAD OF oe-bolh.
  DISABLE TRIGGERS FOR LOAD OF oe-boll.

  RUN oe/relcheck.p (ROWID(oe-relh), OUTPUT ll-exception).
  IF ll-exception THEN NEXT headblok.

  RELEASE oe-bolh.
  IF lv-bol-no NE 0 THEN
  FIND FIRST oe-bolh
      WHERE oe-bolh.company EQ oe-relh.company
        AND oe-bolh.bol-no  EQ lv-bol-no
      NO-ERROR.

  IF NOT AVAIL oe-bolh         OR 
     FIRST-OF(oe-relh.carrier) OR
     relpost-chr EQ "BOL/REL"  THEN
  DO TRANSACTION:
    {oe/oe-bolno.i}
    lv-bol-no = oe-bolh.bol-no.
  END.

  FIND FIRST oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
        AND oe-rell.s-code  EQ "I"
      USE-INDEX r-no NO-ERROR.

  relpost-chr = IF AVAIL oe-rell THEN "Invoice" ELSE v-relpost-hld.                

  {oe/do-bol.i}

  IF LAST-OF(oe-relh.carrier) OR
     relpost-chr EQ "BOL/REL" THEN
  DO TRANSACTION:
    oe-bolh.printed = relpost-chr EQ "Invoice".

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-bolh.company
          AND cust.cust-no EQ oe-bolh.cust-no
        NO-ERROR.

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
        USE-INDEX b-no:
      FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode.
      {oe/seq-bolh.i}
    END.
  END.
