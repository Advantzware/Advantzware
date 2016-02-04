/* t-order.p  Assign b-ord-no, rel-no to line from header for oe-boll, oe-rell */
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
  
FOR EACH oe-relh NO-LOCK:
    FOR EACH oe-rell WHERE oe-rell.company = oe-relh.company AND
                           oe-rell.r-no = oe-relh.r-no
                     USE-INDEX r-no:
        
        ASSIGN oe-rell.ord-no = oe-relh.ord-no
               oe-rell.rel-no = oe-relh.rel-no
               oe-rell.b-ord-no = oe-relh.b-ord-no.
    END.
    DISPLAY oe-relh.r-no.
    PAUSE 0.
END.


FOR EACH oe-bolh NO-LOCK:
    FOR EACH oe-boll WHERE oe-boll.company = oe-bolh.company AND
                           oe-boll.b-no = oe-bolh.b-no:
       ASSIGN oe-boll.b-ord-no = oe-bolh.b-ord-no
              oe-boll.ord-no = oe-bolh.ord-no
              oe-boll.rel-no = oe-bolh.rel-no.             
        
    END.
    DISP oe-bolh.b-no oe-bolh.bol-no.
    PAUSE 0.
END.


FOR EACH oe-ordl.
    IF oe-ordl.e-num <> 0 THEN oe-ordl.e-num = 0.
END.



