DISABLE TRIGGERS FOR LOAD OF oe-ordl.


FOR EACH company NO-LOCK:

  FOR EACH inv-head NO-LOCK WHERE inv-head.company EQ company.company,
      EACH inv-line NO-LOCK
      WHERE inv-line.r-no   EQ inv-head.r-no
        AND inv-line.ord-no NE 0,
      EACH oe-ordl
      WHERE oe-ordl.company EQ inv-line.company
        AND oe-ordl.ord-no  EQ inv-line.ord-no
        AND oe-ordl.i-no    EQ inv-line.i-no
        AND oe-ordl.line    EQ inv-line.line:

    DISPLAY oe-ordl.company LABEL "Company"
            oe-ordl.ord-no  LABEL "Order#"
            oe-ordl.i-no    FORMAT "x(20)"
                          LABEL "FG Item#"
        WITH DOWN.

    RUN oe/ordlsqty.p (ROWID(oe-ordl),
                       OUTPUT oe-ordl.inv-qty,
                       OUTPUT oe-ordl.ship-qty).

    oe-ordl.t-ship-qty = oe-ordl.ship-qty.
  END.

  FOR EACH ar-inv NO-LOCK
      WHERE ar-inv.company  EQ company.company
        AND ar-inv.inv-date GT 10/01/2005,
      EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no   EQ ar-inv.x-no
        AND ar-invl.ord-no NE 0,
      EACH oe-ordl
      WHERE oe-ordl.company EQ ar-invl.company
        AND oe-ordl.ord-no  EQ ar-invl.ord-no
        AND oe-ordl.i-no    EQ ar-invl.i-no:

    DISPLAY oe-ordl.company LABEL "Company"
            oe-ordl.ord-no  LABEL "Order#"
            oe-ordl.i-no    FORMAT "x(20)"
                          LABEL "FG Item#"
        WITH DOWN.

    RUN oe/ordlsqty.p (ROWID(oe-ordl),
                       OUTPUT oe-ordl.inv-qty,
                       OUTPUT oe-ordl.ship-qty).

    oe-ordl.t-ship-qty = oe-ordl.ship-qty.
  END.
END.

MESSAGE "Procedure complete..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

HIDE ALL NO-PAUSE.
