
FOR EACH e-item:
  DISPLAY e-item.company
          e-item.i-no.

  IF CAN-FIND(FIRST e-item-vend
              WHERE e-item-vend.company   EQ e-item.company
                AND e-item-vend.i-no      EQ e-item.i-no
                AND e-item-vend.item-type EQ NO) AND
     NOT CAN-FIND(FIRST e-itemfg
                  WHERE e-itemfg.company EQ e-item.company
                    AND e-itemfg.i-no    EQ e-item.i-no) THEN DO:

    CREATE e-itemfg.
    BUFFER-COPY e-item EXCEPT rec_key TO e-itemfg.
  END.

  IF e-item.std-uom EQ "M" THEN e-item.std-uom = "MSH".
END.

FOR EACH e-item-vend:
  DISPLAY e-item-vend.company
          e-item-vend.i-no
          e-item-vend.vend-no.

  IF CAN-FIND(FIRST e-itemfg
              WHERE e-itemfg.company   EQ e-item-vend.company
                AND e-itemfg.i-no      EQ e-item-vend.i-no)           AND
     NOT CAN-FIND(FIRST e-itemfg-vend
                  WHERE e-itemfg-vend.company EQ e-item-vend.company
                    AND e-itemfg-vend.i-no    EQ e-item-vend.i-no
                    AND e-itemfg-vend.vend-no EQ e-item-vend.vend-no) THEN DO:
    CREATE e-itemfg-vend.
    BUFFER-COPY e-item-vend EXCEPT rec_key TO e-itemfg-vend
    ASSIGN
     e-itemfg-vend.item-type = NO.
  END.

  IF NOT CAN-FIND(FIRST ITEM
                  WHERE ITEM.company EQ e-item-vend.company
                    AND ITEM.i-no    EQ e-item-vend.i-no) THEN DELETE e-item-vend.

  ELSE e-item-vend.item-type = YES.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Utility Completed..." VIEW-AS ALERT-BOX.

