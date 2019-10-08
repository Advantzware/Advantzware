/* bank.i Bank validation */

IF {1}:MODIFIED THEN DO:
   FIND FIRST bank WHERE bank.company = g_company AND                         
                         bank.bank-code = {1}:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL bank THEN DO:
         MESSAGE "Invalid bank. Try Help (F1)." VIEW-AS ALERT-BOX ERROR.
         apply "entry" TO {1}.
         RETURN NO-APPLY.
    END.
    {2} = bank.bank-NAME.
    
END.
