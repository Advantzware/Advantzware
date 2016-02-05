
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF item.
                                   
                                   
SESSION:SET-WAIT-STATE ("general").

FOR EACH itemfg:
  DISPLAY itemfg.company    LABEL "Company"
          itemfg.i-no       FORMAT "x(20)"
                            LABEL "FG Item#".

  FOR EACH po-ordl
      WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
        AND po-ordl.item-type EQ NO
      BY po-ordl.po-no DESC:
    LEAVE.
  END.
  
  itemfg.pur-uom = IF AVAIL po-ordl THEN po-ordl.pr-qty-uom ELSE "EA".
END.

FOR EACH item:
  DISPLAY item.company    LABEL "Company"
          item.i-no       FORMAT "x(20)"
                          LABEL "RM Item#".

  FOR EACH po-ordl
      WHERE po-ordl.company   EQ item.company
        AND po-ordl.i-no      EQ item.i-no
        AND po-ordl.item-type EQ YES
      BY po-ordl.po-no DESC:
    LEAVE.
  END.
  item.pur-uom = IF AVAIL po-ordl THEN po-ordl.pr-qty-uom ELSE "EA".
END.

HIDE ALL NO-PAUSE.

SESSION:SET-WAIT-STATE ("").
