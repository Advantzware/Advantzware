DEFINE INPUT PARAMETER ipcStartDate AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOG NO-UNDO.

DEFINE VARIABLE lcMsg AS CHAR NO-UNDO.
      
  oplValid = YES.

  IF DATE(ipcStartDate) LT TODAY THEN
      lcMsg = "earlier than today".
  IF lcMsg EQ ""
      AND DATE(ipcStartDate) GT TODAY + 180 THEN
      lcMsg = "later than 180 days from today".
  IF lcMsg NE "" THEN DO:
      MESSAGE "Start Date cannot be " + TRIM(lcMsg) + "..."
          VIEW-AS ALERT-BOX ERROR.
      oplValid = NO.
  END.
  

