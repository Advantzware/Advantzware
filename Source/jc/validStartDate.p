DEFINE INPUT PARAMETER ipcStartDate AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

  IF DATE(ipcStartDate) LT TODAY THEN
      opcMessage = "earlier than today".
  IF opcMessage EQ ""
      AND DATE(ipcStartDate) GT TODAY + 180 THEN
      opcMessage = "later than 180 days from today".
  IF opcMessage NE "" THEN 
      opcMessage = "Start Date cannot be " + TRIM(opcMessage) + "..." .
         
  

