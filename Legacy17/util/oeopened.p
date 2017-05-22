
SESSION:SET-WAIT-STATE ("general").

FOR EACH oe-ord:
  oe-ord.opened = LOOKUP(oe-ord.stat,"C,D,Z") EQ 0.
      
  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no:
    
    oe-ordl.opened = oe-ord.opened.
  END.
END.

SESSION:SET-WAIT-STATE ("").
