/* currentDatePrompt.i */

IF {1} NE '/  /' AND
  (MONTH(DATE({1})) NE MONTH(TODAY) OR
   YEAR(DATE({1})) NE YEAR(TODAY)) THEN DO:
  MESSAGE 'Date is not Current Month and/or Year, Continue?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lvContinue AS LOGICAL.
  IF NOT lvContinue THEN DO:
    APPLY 'ENTRY':U TO SELF.
    RETURN NO-APPLY.
  END. /* if not lvcontinue */
END. /* if month */
