/* Obtain Release Status Description  */
  
    DEFINE INPUT PARAMETER ipStatus AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opStatDesc AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStatDesc AS CHARACTER NO-UNDO.

    CASE ipStatus:
      WHEN "S" THEN cStatDesc = "S- Scheduled".
      WHEN "L" THEN cStatDesc = "L- Late".     
      WHEN "I" THEN cStatDesc = "I- Invoice Per Terms".
      WHEN "A" THEN cStatDesc = "A- Actual".
      WHEN "P" THEN cStatDesc = "P- Posted".
      WHEN "B" THEN cStatDesc = "B- Backorder".
      WHEN "Z" THEN cStatDesc = "Z- Posted BOL".
      WHEN "C" THEN cStatDesc = "C- Completed".
    END CASE.

 
ASSIGN 
    opStatDesc = cStatDesc .