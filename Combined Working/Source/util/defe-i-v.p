
{custom/globdefs.i}

{sys/inc/var.i new shared}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

FOR EACH e-item-vend
    WHERE e-item-vend.company   EQ cocode
      AND e-item-vend.item-type EQ YES
      AND CAN-FIND(FIRST item 
                   WHERE item.company  EQ e-item-vend.company
                     AND item.i-no     EQ e-item-vend.i-no
                     AND INDEX("1234B",item.mat-type) GT 0):

  ASSIGN
   e-item-vend.roll-w[27] = 0
   e-item-vend.roll-w[28] = 999.9999
   e-item-vend.roll-w[29] = 0
   e-item-vend.roll-w[30] = 999.9999.
END.

SESSION:SET-WAIT-STATE("").
