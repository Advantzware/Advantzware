SESSION:SET-WAIT-STATE ("general").
    
FOR EACH itemfg WHERE i-code EQ "C":
  prod-uom = "M".

  FOR EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        AND itemfg.i-no    NE "":
    pur-uom = prod-uom.
  END.
END.

SESSION:SET-WAIT-STATE ("").

MESSAGE "Fixing FG's UOM complete..." VIEW-AS ALERT-BOX.
