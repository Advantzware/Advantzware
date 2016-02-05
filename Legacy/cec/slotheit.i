/* cec/slotheit.i   Slot height
    /*Slot Height = Blank Width / 2 + .125 if Board Caliper less than .10000
      Slot Height = Blank Width / 2 + .25 if Board Caliper is greater than .10000
      */
*/

IF eb.dep = 0 THEN DO:
    FIND FIRST style WHERE style.company EQ eb.company
                     AND style.style EQ eb.style NO-LOCK NO-ERROR.
    IF AVAIL style AND lookup(style.TYPE,'P,R') > 0 THEN
       eb.dep = eb.wid / 2 + IF eb.wid > 4 THEN 0.125 ELSE 0.0625.
END.
