/* Obtain Job Status Description  */
  
    DEFINE INPUT PARAMETER ipStatus AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opStatDesc AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStatDesc AS CHARACTER NO-UNDO.
   

    CASE ipStatus:
      WHEN "D" THEN cStatDesc = "D- Deleted".
      WHEN "X" THEN cStatDesc = "X- Completed".     
      WHEN "P" THEN cStatDesc = "P- Pending".
      WHEN "C" THEN cStatDesc = "C- Closed".
      WHEN "H" THEN cStatDesc = "H- On Hold".
      WHEN "R" THEN cStatDesc = "R- Repeat".
      WHEN "W" THEN cStatDesc = "W- Work in Process".
   
   
   //   WHEN "Z" THEN cStatDesc = "C- On Hold".
   //   WHEN "L" THEN cStatDesc = "C- On Hold".
   //   WHEN "A" THEN cStatDesc = "C- On Hold".
   
    END CASE.
    
ASSIGN 
    opStatDesc = cStatDesc .
