/* custom/asimail.i */
DEF VAR ret-code AS INT NO-UNDO.
DEF VAR ls-mail-file AS cha NO-UNDO.

IF "{&TYPE}" = "CUSTOMER"  THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
      MESSAGE "Beginning Customer and Ending Customer must be the same for E-mail."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&END_cust} .
              RETURN NO-APPLY.
   END.
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".xpr" + ",r:\asi_gui9\source\viewer.exe".  */
   ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".xpr".
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".pdf". */
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).
   run custom/xpmail.p ("CUSTOMER",ls-mail-file,{&begin_cust},
                                '{&mail-subject}',
                                '{&mail-body}',OUTPUT ret-code).
END.
ELSE IF "{&TYPE}" = "VENDOR" THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
      MESSAGE "Beginning Vendor and Ending Vendor must be the same for E-mail."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&END_cust} .
              RETURN NO-APPLY.
   END.
   ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".xpr". 
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".pdf". */
   

   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).
   run custom/xpmail.p ("VENDOR",ls-mail-file,{&begin_cust},
                                '{&mail-subject}',
                                '{&mail-body}',OUTPUT ret-code).
END.
ELSE DO:
   
   ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".xpr". 
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".pdf". */
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).
   run custom/xpmail.p ("",ls-mail-file,{&begin_cust},
                                '{&mail-subject}',
                                '{&mail-body}',OUTPUT ret-code).
END.
 
IF ret-code <> 0 THEN
   MESSAGE "Error: " ret-code view-as alert-box ERROR.
