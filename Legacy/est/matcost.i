
DEF VAR lv-setup-{3} LIKE e-item-vend.setup NO-UNDO.
DEF VAR ll-est-found-{3} AS LOG NO-UNDO.

DEF TEMP-TABLE tt-e-i-v-{3} NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

ASSIGN
 ll-est-found-{3} = NO
 {2}              = 0
 lv-setup-{3}     = 0.

FIND FIRST e-item OF item NO-LOCK NO-ERROR.

IF AVAIL e-item THEN DO:
   RELEASE e-item-vend.
  
   FOR EACH e-item-vend OF e-item NO-LOCK
       WHERE e-item-vend.item-type EQ YES
       BY e-item-vend.vend-no:
     LEAVE.
   END.
  
   CREATE tt-e-i-v-{3}.
   IF AVAIL e-item-vend THEN
   DO:
      DO j = 1 TO 10:
         ASSIGN
            tt-e-i-v-{3}.run-qty[j] = e-item-vend.run-qty[j]
            tt-e-i-v-{3}.run-cost[j] = e-item-vend.run-cost[j]
            tt-e-i-v-{3}.setups[j] = e-item-vend.setups[j].
      END.


      
      IF AVAIL e-item THEN
      DO:

      
         DO j = 1 TO 10:
            ASSIGN
               tt-e-i-v-{3}.run-qty[j + 10] = e-item-vend.runQtyXtra[j]
               tt-e-i-v-{3}.run-cost[j + 10] = e-item-vend.runCostXtra[j]
               tt-e-i-v-{3}.setups[j + 10] = e-item-vend.setupsXtra[j].
         END.
      END. 
   END.
   ELSE
   IF AVAIL e-item THEN
   DO:
      DO j = 1 TO 10:
         ASSIGN
            tt-e-i-v-{3}.run-qty[j] = e-item.run-qty[j]
            tt-e-i-v-{3}.run-cost[j] = e-item.run-cost[j].
      END.
      
            
         DO j = 1 TO 10:
            ASSIGN
               tt-e-i-v-{3}.run-qty[j + 10] = e-item.runQty[j]
               tt-e-i-v-{3}.run-cost[j + 10] = e-item.runCost[j].
         END.
   END.
  
   DO j = 1 TO 20:
     IF tt-e-i-v-{3}.run-qty[j] NE 0   AND
        tt-e-i-v-{3}.run-qty[j] GE {1} THEN DO:
       ASSIGN
        ll-est-found-{3} = YES
        {2}              = tt-e-i-v-{3}.run-cost[j]
        lv-setup-{3}     = tt-e-i-v-{3}.setups[j].
       LEAVE.
     END.
   END.
  
   DELETE tt-e-i-v-{3}.
END.

IF item.i-code EQ "R" AND NOT ll-est-found-{3} THEN
  {2} = IF ce-ctrl.r-cost THEN item.avg-cost ELSE item.last-cost.
