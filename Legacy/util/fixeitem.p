
FOR EACH company:
  FOR EACH item NO-LOCK WHERE item.company EQ company.company,
      FIRST e-item NO-LOCK OF item:

    DISPLAY "Processing Company/RMItem#: " +
            TRIM(item.company) + "/"       +
            TRIM(item.i-no)                     FORMAT "x(50)"
        WITH FRAME f1 1 DOWN.

    IF NOT CAN-FIND(FIRST e-item-vend OF e-item
                    WHERE e-item-vend.item-type EQ YES
                      AND e-item-vend.vend-no   EQ "") THEN DO:

      CREATE e-item-vend.
      BUFFER-COPY e-item EXCEPT rec_key TO e-item-vend
      ASSIGN
       e-item-vend.item-type = YES
       e-item-vend.vend-no   = "".
    END.
  END.

  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ company.company,
      FIRST e-itemfg NO-LOCK OF itemfg:

    DISPLAY "Processing Company/FGItem#: " +
            TRIM(itemfg.company) + "/"     +
            TRIM(itemfg.i-no)                   FORMAT "x(50)"
        WITH FRAME f2 1 DOWN.

    IF NOT CAN-FIND(FIRST e-itemfg-vend OF e-itemfg
                    WHERE e-itemfg-vend.item-type EQ YES
                      AND e-itemfg-vend.vend-no   EQ "") THEN DO:

      CREATE e-itemfg-vend.
      BUFFER-COPY e-itemfg EXCEPT rec_key TO e-itemfg-vend
      ASSIGN
       e-itemfg-vend.item-type = YES
       e-itemfg-vend.vend-no   = "".
    END.
  END.                   
END.

HIDE FRAME f1 NO-PAUSE.
HIDE FRAME f2 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
