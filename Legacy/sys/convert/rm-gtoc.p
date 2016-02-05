DEF TEMP-TABLE tt-item LIKE ITEM.
DEF TEMP-TABLE tt-e-item LIKE e-item.
DEF TEMP-TABLE tt-e-item-vend LIKE e-item-vend.

INPUT FROM c:/tmp/ITEM.d.

REPEAT:
  CREATE tt-item.
  IMPORT tt-item.
  IF (tt-item.mat-type EQ "I" OR tt-item.mat-type EQ "V") AND
     tt-item.industry EQ "1"                              THEN DO:
    FIND FIRST ITEM
        WHERE ITEM.company EQ tt-item.company
          AND ITEM.i-no    EQ tt-item.i-no
        NO-ERROR.
    IF NOT AVAIL ITEM THEN CREATE ITEM.
    BUFFER-COPY tt-item TO ITEM.
  END.
  ELSE DELETE tt-item.
END.

INPUT FROM c:/tmp/e-ITEM.d.

REPEAT:
  CREATE tt-e-item.
  IMPORT tt-e-item.
  IF CAN-FIND(FIRST tt-item WHERE tt-item.company EQ tt-e-item.company
                              AND tt-item.i-no    EQ tt-e-item.i-no)
  THEN DO:
    FIND FIRST e-ITEM
        WHERE e-ITEM.company EQ tt-e-item.company
          AND e-ITEM.i-no    EQ tt-e-item.i-no
        NO-ERROR.
    IF NOT AVAIL e-ITEM THEN CREATE e-ITEM.
    BUFFER-COPY tt-e-item TO e-ITEM.
  END.
  DELETE tt-e-item.
END.

INPUT FROM c:/tmp/eitmvend.d.

REPEAT:
  CREATE tt-e-item-vend.
  IMPORT tt-e-item-vend.
  IF tt-e-item-vend.vend-no EQ "" AND
     CAN-FIND(FIRST tt-item WHERE tt-item.company EQ tt-e-item-vend.company
                              AND tt-item.i-no    EQ tt-e-item-vend.i-no)
  THEN DO:
    FIND FIRST e-item-vend
        WHERE e-item-vend.company   EQ tt-e-item-vend.company
          AND e-item-vend.item-type EQ YES
          AND e-item-vend.i-no      EQ tt-e-item-vend.i-no
          AND e-item-vend.vend-no   EQ tt-e-item-vend.vend-no
        NO-ERROR.
    IF NOT AVAIL e-item-vend THEN CREATE e-item-vend.
    BUFFER-COPY tt-e-item-vend TO e-item-vend.
  END.
  DELETE tt-e-item-vend.
END.

