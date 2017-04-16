/* custom/asimail2.i  added &group-title instead of using email and RUN xpmail2.p */
/* dgd 04/26/2007 - reformatted for readability. */

&SCOPED-DEFINE type         {&type}
&SCOPED-DEFINE group-title  {&group-title}
&SCOPED-DEFINE begin_cust   {&begin_cust}
&SCOPED-DEFINE end_cust     {&end_cust}
&SCOPED-DEFINE mail-subject {&mail-subject}
&SCOPED-DEFINE mail-body    {&mail-body}
&SCOPED-DEFINE mail-file    {&mail-file}

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

/* custom/asimail.i - shared by asimailr.i, asimail2.i and asimailr2.i */
/* dgd 04/26/2007   - reformatted for readability. */

/* Preprocessors */
&SCOPED-DEFINE xpmailProgram  custom/xpmail
&SCOPED-DEFINE xpmailProgrm2  custom/xpmail2
&SCOPED-DEFINE groupTitle     ,

&IF DEFINED(group-title) NE 0 &THEN
  &SCOPED-DEFINE xpmailProgram  {&xpmailProgram}2    
  &SCOPED-DEFINE groupTitle     ,{&group-title},    
&ENDIF

&IF '{&two}' EQ '' &THEN
  IF INDEX({&mail-file},'.pdf') EQ 0 THEN
    ls-mail-file{&two} = {&mail-file} + '.pdf'.
  else
    ls-mail-file{&two} = {&mail-file}.

&ELSE
  ls-mail-file{&two} = init-dir + '\att' + STRING(TIME) + '.txt'. 
  OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file{&two}).
&ENDIF

IF CAN-DO('Customer1,Customer,Vendor','{&type}') THEN DO:
  
  RUN {&xpmailProgrm2}.p (input   "{&type}",
                          input   v-prgmname,
                          input   ls-mail-file{&two},
                          input   {&begin_cust},
                          input   {&WINDOW-NAME}:TITLE,
                          input   {&WINDOW-NAME}:TITLE,
                          OUTPUT  ret-code{&two}).
end. /* can-do */


