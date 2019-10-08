/* addon/bol/loadcust.i */
FIND FIRST itemfg WHERE itemfg.company = cocode
                    AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN
   FIND FIRST cust WHERE cust.company = cocode 
                     AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
