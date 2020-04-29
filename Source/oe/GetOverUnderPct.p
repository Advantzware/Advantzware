/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       return over run and under run  
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM ipcCompany      AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAM ipcCustomerID   AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAM ipcShipToId     AS   CHARACTER               NO-UNDO.
DEFINE OUTPUT PARAM opcOverPer      AS   CHARACTER               NO-UNDO.
DEFINE OUTPUT PARAM opcUnderPer     AS   CHARACTER               NO-UNDO.
  
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ ipcCompany 
    AND cust.cust-no EQ ipcCustomerID NO-ERROR .
      
FIND FIRST shipto NO-LOCK
    WHERE shipto.company EQ ipcCompany
    AND shipto.cust-no EQ ipcCustomerID
    AND shipto.ship-id EQ ipcShipToId NO-ERROR .   
      
  IF AVAIL cust THEN
  DO:
   IF AVAIL shipto AND shipto.overUnderEnabled THEN
     ASSIGN
        opcOverPer  = string(shipto.oversPercent)
        opcUnderPer = string(shipto.undersPercent) .
   ELSE
      ASSIGN
        opcOverPer  = string(cust.over-pct)
        opcUnderPer = string(cust.under-pct).      
  END.
      
      
      
      




    

