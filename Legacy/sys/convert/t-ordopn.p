  /* t-ordopn.p  update oe-ord.opened and oe-ordl.opend - new fields*/
  
  DISABLE TRIGGERS FOR LOAD OF oe-ordl.
  
  FOR EACH oe-ord:
  
     oe-ord.opened = LOOKUP(oe-ord.stat,"C,D,Z") EQ 0.

  
     FIND FIRST oe-ordl OF oe-ord NO-LOCK NO-ERROR.
     IF NOT AVAIL oe-ordl THEN DO:
        CREATE oe-ordl.
        ASSIGN oe-ordl.company = oe-ord.company
               oe-ordl.ord-no = oe-ord.ord-no
               oe-ordl.LINE = 0.
     END.


     FOR EACH oe-ordl
         WHERE oe-ordl.company EQ oe-ord.company
           AND oe-ordl.ord-no  EQ oe-ord.ord-no:
    
       oe-ordl.opened = oe-ord.opened.
     END.
  END.
