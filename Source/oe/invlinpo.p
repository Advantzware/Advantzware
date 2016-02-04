DEF PARAM BUFFER io-inv-line FOR inv-line.
DEF INPUT PARAM lv-old-po-no LIKE inv-line.po-no NO-UNDO.

    
FOR EACH oe-boll
    WHERE oe-boll.company EQ io-inv-line.company
      AND oe-boll.b-no    EQ io-inv-line.b-no
      AND oe-boll.ord-no  EQ io-inv-line.ord-no
      AND oe-boll.i-no    EQ io-inv-line.i-no
      AND oe-boll.line    EQ io-inv-line.line
      AND oe-boll.po-no   EQ lv-old-po-no
      AND CAN-FIND(FIRST oe-bolh
                   WHERE oe-bolh.b-no   EQ oe-boll.b-no
                     AND oe-bolh.posted EQ YES)
    BREAK BY oe-boll.r-no
          BY oe-boll.rel-no
          BY oe-boll.b-ord-no:

  oe-boll.po-no = io-inv-line.po-no.
END.
