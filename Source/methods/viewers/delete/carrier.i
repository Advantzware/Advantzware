/* carrier.i */

    DEFINE BUFFER bf-quotehd FOR quotehd.
    DEFINE BUFFER bf-cust FOR cust.
    DEFINE BUFFER bf-shipto FOR shipto.
  
    IF AVAILABLE carrier THEN
    DO:
         FIND FIRST bf-quotehd NO-LOCK
              WHERE bf-quotehd.company EQ carrier.company
                AND bf-quotehd.carrier EQ carrier.carrier 
                AND bf-quotehd.loc EQ carrier.loc NO-ERROR.
         IF AVAILABLE bf-quotehd THEN
         DO:
             MESSAGE "Carrier is on quotes and cannot be deleted." 
             VIEW-AS ALERT-BOX INFO. 
             RETURN "ADM-ERROR":U.
         END.

         FIND FIRST bf-cust NO-LOCK
              WHERE bf-cust.company EQ carrier.company
                AND bf-cust.carrier EQ carrier.carrier
                AND bf-cust.loc EQ carrier.loc NO-ERROR.
         IF AVAILABLE bf-cust THEN
         DO:
             MESSAGE "Carrier is on customers and cannot be deleted." 
             VIEW-AS ALERT-BOX INFO. 
             RETURN "ADM-ERROR":U.
         END.
         
         FIND FIRST bf-shipto NO-LOCK
              WHERE bf-shipto.company EQ carrier.company
                AND bf-shipto.carrier EQ carrier.carrier
                AND bf-shipto.loc EQ carrier.loc NO-ERROR.
         IF AVAILABLE bf-shipto THEN
         DO:
             MESSAGE "Carrier is on shipto and cannot be deleted." 
             VIEW-AS ALERT-BOX INFO. 
             RETURN "ADM-ERROR":U.
         END.
    END.

