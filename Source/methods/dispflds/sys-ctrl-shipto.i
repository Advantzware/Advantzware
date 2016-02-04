/* sys-ctrl-shipto.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.cust-vend-no" THEN DO:
    IF {&FIRST-EXTERNAL-TABLE}.cust-vend:SCREEN-VALUE EQ 'Yes' THEN DO:
      FIND cust NO-LOCK
           WHERE cust.company EQ gcompany
             AND cust.cust-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE
           NO-ERROR.
      type_name = IF NOT AVAILABLE cust THEN "" ELSE cust.name.
    END.
    ELSE DO:
      FIND vend NO-LOCK
           WHERE vend.company EQ gcompany
             AND vend.vend-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE
           NO-ERROR.
      type_name = IF NOT AVAILABLE vend THEN "" ELSE vend.name.
    END.
    DISPLAY type_name.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.ship-id" THEN DO:
    IF {&FIRST-EXTERNAL-TABLE}.cust-vend:SCREEN-VALUE EQ 'Yes' THEN DO:
      FIND shipto NO-LOCK
           WHERE shipto.company EQ gcompany
             AND shipto.cust-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE
             AND shipto.ship-id EQ {&FIRST-EXTERNAL-TABLE}.ship-id:SCREEN-VALUE
           NO-ERROR.
      ship_name = IF NOT AVAILABLE shipto THEN "" ELSE shipto.ship-name.
    END.
    ELSE DO:
      ship_name = ''.
    END.
    DISPLAY ship_name.
  END.
