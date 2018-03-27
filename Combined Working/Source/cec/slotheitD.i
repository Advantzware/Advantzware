/* cec/slotheitD.i   Slot height display - screen-value
    /*Slot Height = Blank Width / 2 + .125 if Board Caliper less than .10000
      Slot Height = Blank Width / 2 + .25 if Board Caliper is greater than .10000
      */
*/

DO :  /* screen-field to display */
    DEF VAR dHeight AS DEC NO-UNDO.
    FIND FIRST style WHERE style.company EQ cocode
                     AND style.style EQ eb.style:SCREEN-VALUE {1} NO-LOCK NO-ERROR.
    IF AVAIL style AND lookup(style.TYPE,'P,R') > 0 THEN DO:
      dHeight = {sys/inc/k16bv.i dec(eb.wid:SCREEN-VALUE)}. /* convert to dec */
      eb.dep:SCREEN-VALUE = string(dHeight / 2 + (IF dHeight GT 4 THEN 0.125 ELSE 0.0625)).
    END.
    
END.
