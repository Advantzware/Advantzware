&Scoped-define TABLENAME job-mch

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

job-mch.job-mchID = NEXT-VALUE(job-mch_seq,ASI).
