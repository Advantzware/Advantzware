&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME cmpltjob

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST job-mch WHERE job-mch.company EQ cmpltjob.company
                       /*AND job-mch.job     EQ pc-prdd.job*/
                     AND job-mch.m-code = cmpltjob.machine
                     AND job-mch.job-no  EQ cmpltjob.job_number
                     AND job-mch.job-no2 EQ cmpltjob.job_sub
                     AND job-mch.frm    EQ cmpltjob.FORM_number
                     AND job-mch.blank-no = cmpltjob.BLANK_number
                     NO-ERROR.
IF AVAIL job-mch THEN
   ASSIGN job-mch.run-complete = NO
          job-mch.mr-complete = NO.
          
