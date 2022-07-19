/* currentDatePrompt.i */
DEFINE VARIABLE lvContinue AS LOGICAL NO-UNDO.
IF {1} NE '/  /' AND
  (MONTH(DATE({1})) NE MONTH(TODAY) OR
   YEAR(DATE({1})) NE YEAR(TODAY)) THEN DO:
   RUN displayMessageQuestion ("78", OUTPUT lvContinue).  
  IF NOT lvContinue THEN DO:
    APPLY 'ENTRY':U TO SELF.
    RETURN NO-APPLY.
  END. /* if not lvcontinue */
END. /* if month */
