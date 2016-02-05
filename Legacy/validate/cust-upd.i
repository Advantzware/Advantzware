/* cust-upd.i customer validation from local-update-record */

IF {1}:MODIFIED THEN DO:
   FIND FIRST cust WHERE cust.company = g_company AND
                         cust.active = "A" AND
                         cust.cust-no = {1}:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DO:
         MESSAGE "Invalid Customer. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO {1}.
         RETURN NO-APPLY.
    END.
    {2} = cust.NAME.
    
END.
