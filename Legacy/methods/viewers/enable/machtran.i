/* machtran.i */
IF "{&update-tran}" <> "" THEN DO:
   
   RUN {&update-tran} NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN ERROR.
END.

{methods/viewers/enable/timeflds.i}

IF "{&enable-proc}" NE "" THEN RUN {&enable-proc}.
