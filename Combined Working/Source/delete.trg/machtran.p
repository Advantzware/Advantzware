&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME machtran

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

IF CAN-FIND (FIRST cmpltjob
             WHERE cmpltjob.company EQ machtran.company
               AND cmpltjob.machine EQ machtran.machine
               AND cmpltjob.job_number EQ machtran.job_number
               AND cmpltjob.job_sub EQ machtran.job_sub
               AND cmpltjob.FORM_number EQ machtran.FORM_number
               AND cmpltjob.blank_number EQ machtran.blank_number) THEN
/* Not any more per job#
   IF NOT AVAIL cmpltjob THEN
    FIND FIRST cmpltjob WHERE cmpltjob.company = machtran.company
                      AND cmpltjob.machine = machtran.machine
                      AND cmpltjob.job_number = machtran.job_number
                      AND cmpltjob.job_sub = machtran.job_sub
                      NO-LOCK NO-ERROR. */                      
DO:
   MESSAGE "Job is completed. Can't Delete Completed Job." VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

{methods/triggers/delete.i}
{methods/delete.trg/{&TABLENAME}.i}
