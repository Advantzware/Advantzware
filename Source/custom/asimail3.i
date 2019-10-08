/* custom/asimail3.i  include file for reports using txt extention from asimailr.i */
DEF VAR ret-code2 AS INT NO-UNDO.
DEF VAR ls-mail-file2 AS cha NO-UNDO.

IF init-dir = "" THEN
DO:
   FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

   IF AVAIL users AND users.user_program[2] NE "" THEN
      init-dir = users.user_program[2].
   ELSE
      init-dir = "c:\tmp".
END.

IF "{&TYPE}" = "CUSTOMER"  THEN DO:   
   ls-mail-file2 = init-dir + "\att" + STRING(TIME) + ".txt". 
   
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file2).
   run custom/xpmail.p ("CUSTOMER",ls-mail-file2,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code2).
END.
ELSE IF "{&TYPE}" = "VENDOR" THEN DO:
   ls-mail-file2 = init-dir + "\att" + STRING(TIME) + ".txt". 
   
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file2).
   run custom/xpmail.p ("VENDOR",ls-mail-file2,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code2).
END.
ELSE DO:
   
   ls-mail-file2 = init-dir + "\att" + STRING(TIME) + ".txt". 
   
   OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file2).

   run custom/xpmail.p ("{&TYPE}",ls-mail-file2,{&begin_cust},
                                {&mail-subject},
                                {&mail-body},OUTPUT ret-code2).
END.
 
