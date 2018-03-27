SESSION:SET-WAIT-STATE ("general").

FOR EACH itemfg:
  STATUS DEFAULT "Processing FG#: " + TRIM(itemfg.i-no).
  FOR EACH po-ordl
      WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
        AND po-ordl.item-type EQ NO
      BY po-ordl.po-no DESC:
    itemfg.pur-uom = po-ordl.pr-qty-uom.
    LEAVE.
  END.
END.

FOR EACH item:
  STATUS DEFAULT "Processing RM#: " + TRIM(item.i-no).
  FOR EACH po-ordl
      WHERE po-ordl.company   EQ item.company
        AND po-ordl.i-no      EQ item.i-no
        AND po-ordl.item-type EQ YES
      BY po-ordl.po-no DESC:
    item.pur-uom = po-ordl.pr-qty-uom.
    LEAVE.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE ("").
