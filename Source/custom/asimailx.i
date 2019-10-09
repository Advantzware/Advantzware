/* custom/asimail1.i no begin and end cust validation */
DEF VAR ret-code AS INT NO-UNDO.
DEF VAR ls-mail-file AS cha FORM "x(15)" NO-UNDO.

IF "{&TYPE}" = "CUSTOMER"  THEN DO:
   ls-mail-file = {&mail-file} + ".pdf".
    
   run custom/xpmail.p ("CUSTOMER",ls-mail-file,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code).
END.
ELSE IF "{&TYPE}" = "VENDOR" THEN DO:
   
   ls-mail-file = {&mail-file} /*list-name*/ + ".pdf".
   /*OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).*/
   run custom/xpmail.p ("VENDOR",ls-mail-file,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code).
   
END.
ELSE IF "{&TYPE}" = "Excel" THEN DO:
   ls-mail-file = {&mail-file} /*list-name*/ .
   /*OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).*/
   run custom/xpmail.p ("VENDOR",ls-mail-file,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code).
   
END.
ELSE DO:
   
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".xpr". */
   /*ls-mail-file = "c:\tmp\att" + STRING(TIME) + ".pdf". 
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file).*/
   /*ls-mail-file = list-name + ".pdf". */
   ls-mail-file = {&mail-file}  + ".pdf".
   run custom/xpmail.p ("",ls-mail-file,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code).
END.
 /*
IF ret-code <> 0 THEN
   MESSAGE "Error: " ret-code view-as alert-box ERROR.
   */
