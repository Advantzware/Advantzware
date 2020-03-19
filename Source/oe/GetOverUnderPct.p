/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       return over run and under run  
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAM ipcCompany      AS   CHARACTER               NO-UNDO.
DEFINE INPUT  PARAM iproRowid       AS   ROWID                   NO-UNDO.
DEFINE INPUT  PARAM iproRowidShipTo AS   ROWID                   NO-UNDO.
DEFINE OUTPUT PARAM opcOverPer      AS   CHARACTER               NO-UNDO.
DEFINE OUTPUT PARAM opcUnderPer     AS   CHARACTER               NO-UNDO.

  FIND FIRST cust NO-LOCK
    WHERE cust.company EQ ipcCompany 
      AND ROWID(cust) EQ iproRowid NO-ERROR .
  FIND FIRST shipto NO-LOCK
    WHERE shipto.company EQ ipcCompany 
      AND ROWID(shipto) EQ iproRowidShipTo NO-ERROR .
  IF AVAIL cust THEN
  DO:
   IF AVAIL shipto AND shipto.overUnderEnabled THEN
     ASSIGN
        opcOverPer  = string(shipto.overs)
        opcUnderPer = string(shipto.Unders) .
   ELSE
      ASSIGN
        opcOverPer  = string(cust.over-pct)
        opcUnderPer = string(cust.under-pct).      
  END.
      
      
      
      




    

