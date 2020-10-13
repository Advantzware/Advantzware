/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       return over run and under run  
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcCompany      AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustomerID   AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcShipToId     AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcFGItem       AS   CHARACTER               NO-UNDO.
DEFINE OUTPUT PARAMETER opdOverPer      AS   DECIMAL                 NO-UNDO.
DEFINE OUTPUT PARAMETER opdUnderPer     AS   DECIMAL                 NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGOversDefault AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (ipcCompany, "FGOversDefault", "C", NO, NO, "", "", 
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cFGOversDefault = STRING(cRtnChar) NO-ERROR. 
   
IF ipcFGItem EQ  "" AND cFGOversDefault EQ "FG Category" THEN
    cFGOversDefault = "ShipToOverride".       
     

CASE cFGOversDefault:
    WHEN "Customer" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipcCompany 
                AND cust.cust-no EQ ipcCustomerID NO-ERROR .
            IF AVAILABLE cust THEN     
                ASSIGN
                    opdOverPer  = cust.over-pct
                    opdUnderPer = cust.under-pct.                    
        END.
    WHEN "Ship To" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipcCompany 
                AND cust.cust-no EQ ipcCustomerID NO-ERROR .
                  
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ipcCompany
                AND shipto.cust-no EQ ipcCustomerID
                AND shipto.ship-id EQ ipcShipToId NO-ERROR . 
                
            IF AVAILABLE shipto THEN
                ASSIGN
                    opdOverPer  = shipto.oversPercent
                    opdUnderPer = shipto.undersPercent .                              
        END.
    WHEN "ShipToOverride" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipcCompany 
                AND cust.cust-no EQ ipcCustomerID NO-ERROR .
                  
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ipcCompany
                AND shipto.cust-no EQ ipcCustomerID
                AND shipto.ship-id EQ ipcShipToId NO-ERROR .   
                  
            IF AVAILABLE cust THEN
            DO:
                IF AVAILABLE shipto AND shipto.oversPercent NE 0 THEN
                    ASSIGN
                        opdOverPer  = shipto.oversPercent
                        opdUnderPer = shipto.undersPercent .
                ELSE
                    ASSIGN
                        opdOverPer  = cust.over-pct
                        opdUnderPer = cust.under-pct.      
            END.           
        END.                     
    WHEN  "FG category" THEN 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ ipcCompany
                AND itemfg.i-no EQ ipcFGItem NO-ERROR .
                 
            IF AVAILABLE itemfg THEN
            DO:
                FIND FIRST fgcat NO-LOCK
                    WHERE fgcat.company EQ ipcCompany
                    AND fgcat.procat EQ itemfg.procat NO-ERROR .
                IF AVAILABLE fgcat THEN
                    ASSIGN
                        opdOverPer  = fgcat.over-pct
                        opdUnderPer = fgcat.under-pct.                 
            END.
            IF opdOverPer EQ 0 AND opdUnderPer EQ 0 THEN DO:
                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ ipcCompany 
                     AND cust.cust-no EQ ipcCustomerID NO-ERROR .
                IF AVAILABLE cust THEN     
                ASSIGN
                    opdOverPer  = cust.over-pct
                    opdUnderPer = cust.under-pct.            
            END.
        END.           
END CASE.

 
   
      
      




    

