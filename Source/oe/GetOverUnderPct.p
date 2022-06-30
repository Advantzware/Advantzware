/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       return over run and under run  
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcCompany      AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustomerID   AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcShipToId     AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipcFGItem       AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAMETER ipiOrderNo      AS   INTEGER                 NO-UNDO.
DEFINE OUTPUT PARAMETER opdOverPer      AS   DECIMAL                 NO-UNDO.
DEFINE OUTPUT PARAMETER opdUnderPer     AS   DECIMAL                 NO-UNDO.
DEFINE OUTPUT PARAMETER opcTagDesc      AS   CHARACTER               NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGOversDefault AS CHARACTER NO-UNDO.


RUN sys/ref/nk1look.p (ipcCompany, "FGOversDefault", "C", NO, NO, "", "", 
        OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cFGOversDefault = STRING(cRtnChar) NO-ERROR. 
        
IF  ipiOrderNo NE 0 AND  cFGOversDefault NE  "FG category" THEN  
    cFGOversDefault = "Order".

CASE cFGOversDefault:
    WHEN "Customer" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipcCompany 
                AND cust.cust-no EQ ipcCustomerID NO-ERROR .
            IF AVAILABLE cust THEN     
                ASSIGN
                    opdOverPer  = cust.over-pct
                    opdUnderPer = cust.under-pct
                    opcTagDesc  = "Customer no. - " + ipcCustomerID.  
                                      
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
                    opdUnderPer = shipto.undersPercent
                    opcTagDesc  = "Customer no. - " + ipcCustomerID + " Ship ID - " + ipcShipToId .                              
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
                        opdUnderPer = shipto.undersPercent
                        opcTagDesc  = "ShipToOverride Customer no. - " + ipcCustomerID + " Ship ID - " + ipcShipToId .
                ELSE
                    ASSIGN
                        opdOverPer  = cust.over-pct
                        opdUnderPer = cust.under-pct
                        opcTagDesc  = "Customer no. - " + ipcCustomerID + " Ship ID - " + ipcShipToId.      
            END.           
        END.                     
    WHEN  "FG category" THEN 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ ipcCompany
                AND itemfg.i-no EQ ipcFGItem 
                AND itemfg.i-no NE "" NO-ERROR .
                 
            IF AVAILABLE itemfg THEN
            DO:
                FIND FIRST fgcat NO-LOCK
                    WHERE fgcat.company EQ ipcCompany
                    AND fgcat.procat EQ itemfg.procat NO-ERROR .
                IF AVAILABLE fgcat THEN
                    ASSIGN
                        opdOverPer  = fgcat.over-pct
                        opdUnderPer = fgcat.under-pct
                        opcTagDesc  = "FG category  Customer no. - " + ipcCustomerID + 
                                                       " Ship ID - " + ipcShipToId + 
                                                       " Item No. - " + ipcFGItem +
                                                       " Category - " + string(fgcat.procat).                 
            END.
            ELSE DO:
                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ ipcCompany 
                     AND cust.cust-no EQ ipcCustomerID NO-ERROR .
                IF AVAILABLE cust THEN     
                ASSIGN
                    opdOverPer  = cust.over-pct
                    opdUnderPer = cust.under-pct
                    opcTagDesc  = "Customer no. - " + ipcCustomerID + " Ship ID - " + ipcShipToId.            
            END.
        END. 
    WHEN "Order" THEN 
        DO:       
            IF ipiOrderNo NE 0 THEN 
            DO:             
                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ ipcCompany
                      AND oe-ord.ord-no EQ ipiOrderNo NO-ERROR . 
                    
                IF AVAILABLE oe-ord THEN
                    ASSIGN
                        opdOverPer  = oe-ord.over-pct
                        opdUnderPer = oe-ord.under-pct 
                        opcTagDesc  = "Order no. - " + string(ipiOrderNo).
            END.                              
        END.           
END CASE.

 
   
      
      




    

