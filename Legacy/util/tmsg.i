/* Usage:
     {util\tmsg.i """this is a test """ 
                  lValue1 
                  "oe-ordl.qty in browse {&browse-name}"
                   }
*/
MESSAGE  "Test Message" SKIP(2)
    "Pgm:" PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
     SKIP(2)
   
      {1} SKIP
     "{2}" {2} SKIP
     "{3}" {3} SKIP
     "{4}" {4} SKIP
     "{5}" {5} SKIP
     "{6}" {6} SKIP
     "{7}" {7} SKIP
     "{8}" {8} SKIP
     "{9}" {9} SKIP
     "{10}" {10} SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
