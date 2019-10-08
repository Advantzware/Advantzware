/* item.i */


DO WITH FRAME {&FRAME-NAME}:
  IF AVAILABLE item THEN
  ASSIGN
 /*   item_yield = item.yield */
    u-ptd = item.q-ptd * item.avg-cost
    u-ytd = item.q-ytd * item.avg-cost
    u-lyr = item.q-lyr * item.avg-cost.
 /* IF NOT item_yield:HIDDEN THEN
  DISPLAY item_yield.
 */ 
  DISPLAY u-ptd u-ytd u-lyr.
END.
