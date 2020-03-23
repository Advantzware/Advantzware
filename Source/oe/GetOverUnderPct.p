/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       return over run and under run  
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM iproRowid       AS   ROWID                   NO-UNDO.
DEFINE OUTPUT PARAM opcOverPer      AS   CHARACTER               NO-UNDO.
DEFINE OUTPUT PARAM opcUnderPer     AS   CHARACTER               NO-UNDO.
    
  FIND FIRST shipto NO-LOCK
    WHERE ROWID(shipto) EQ iproRowid NO-ERROR .   
  FIND FIRST cust NO-LOCK
    WHERE cust.company EQ shipto.company 
      AND cust.cust-no EQ shipto.cust-no NO-ERROR .    
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
      
      
      
      




    

