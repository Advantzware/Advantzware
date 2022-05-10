&Scoped-define TABLENAME job-code

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

IF CURRENT-VALUE(jobCodeDMISeq) LT 100 THEN 
CURRENT-VALUE(jobCodeDMISeq) = CURRENT-VALUE(jobCodeDMISeq) + 100.

DEFINE VARIABLE iNextValue AS INTEGER NO-UNDO.

// ensure no duplicate value already exists, look for next avail value
DO WHILE TRUE:
    iNextValue = NEXT-VALUE(jobCodeDMISeq).
    IF CAN-FIND(FIRST {&TABLENAME}
                WHERE {&TABLENAME}.dmiID EQ iNextValue) THEN
    NEXT.
    job-code.dmiID = NEXT-VALUE(jobCodeDMISeq).
    LEAVE.
END. // do while
