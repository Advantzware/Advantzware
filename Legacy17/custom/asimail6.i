/* custom/asimail2.i  added &group-title instead of using email and RUN xpmail2.p */

&SCOPED-DEFINE type {&type}
&SCOPED-DEFINE group-title {&group-title}
&SCOPED-DEFINE begin_cust {&begin_cust}
&SCOPED-DEFINE end_cust {&end_cust}
&SCOPED-DEFINE mail-subject {&mail-subject}
&SCOPED-DEFINE mail-body {&mail-body}
&SCOPED-DEFINE mail-file {&mail-file}

&SCOPED-DEFINE xpmailProgram custom/xpmail
&SCOPED-DEFINE xpmailProgrm2 custom/xpmail2
&SCOPED-DEFINE groupTitle ,

&IF DEFINED(group-title) NE 0 &THEN
&SCOPED-DEFINE xpmailProgram {&xpmailProgram}2    
&SCOPED-DEFINE groupTitle ,{&group-title},    
&ENDIF

DEFINE VARIABLE ret-code{&two} AS INTEGER NO-UNDO.
DEFINE VARIABLE ls-mail-file{&two} AS CHARACTER NO-UNDO.

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


&IF '{&two}' EQ '' &THEN
IF INDEX({&mail-file},'.pdf') EQ 0 THEN
ls-mail-file{&two} = {&mail-file} + '.pdf'.
&ELSE
ls-mail-file{&two} = init-dir + '\att' + STRING(TIME) + '.txt'. 
OS-COPY VALUE({&mail-file}) VALUE(ls-mail-file{&two}).
&ENDIF

DO WHILE TRUE:
  &IF '{&begin_cust}' NE '' AND '{&end_cust}' NE '' &THEN
  IF CAN-DO('Customer,Vendor','{&type}') THEN DO:
    IF {&begin_cust} NE {&end_cust} THEN DO:
      FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.active EQ 'X' NO-ERROR.
      IF AVAILABLE cust THEN {&begin_cust} = cust.cust-no.
      RUN {&xpmailProgrm2}.p ("{&type}",v-prgmname,ls-mail-file{&two},{&begin_cust},
                              {&WINDOW-NAME}:TITLE,{&WINDOW-NAME}:TITLE,OUTPUT ret-code{&two}).
      LEAVE.
    END. /* if ne */
  END. /* if can-do */
  &ENDIF
  /*
  &IF '{&type}' EQ 'Customer' AND '{&begin_cust}' NE '' &THEN
  IF NOT CAN-FIND(cust WHERE cust.company EQ cocode
                         AND cust.cust-no EQ STRING({&begin_cust})
                         AND cust.active EQ 'E') THEN LEAVE.
  &ENDIF
  */
  
  IF index(v-prgmname,"r-bolpr2") > 0 THEN
  RUN {&xpmailProgram}.p ("{&type}"{&groupTitle}{&mail-file},{&begin_cust},
                          {&WINDOW-NAME}:TITLE,{&WINDOW-NAME}:TITLE,OUTPUT ret-code{&two}).
  ELSE
  RUN {&xpmailProgram}.p ("{&type}"{&groupTitle}ls-mail-file{&two},{&begin_cust},
                          {&WINDOW-NAME}:TITLE,{&WINDOW-NAME}:TITLE,OUTPUT ret-code{&two}).
  LEAVE.
END. /* do while */
