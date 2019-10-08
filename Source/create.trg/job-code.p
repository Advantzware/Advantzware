&Scoped-define TABLENAME job-code

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

IF CURRENT-VALUE(jobCodeDMISeq) LT 100 THEN 
CURRENT-VALUE(jobCodeDMISeq) = CURRENT-VALUE(jobCodeDMISeq) + 100.

job-code.dmiID = NEXT-VALUE(jobCodeDMISeq).
