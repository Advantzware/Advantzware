&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME vendItemCost

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

IF length({&TABLENAME}.estimateNo) < 8 THEN 
    {&TABLENAME}.estimateNo = fill(" ", 8 - length({&TABLENAME}.estimateNo)) + trim({&TABLENAME}.estimateNo) . 

ASSIGN 
    {&TABLENAME}.updatedID = USERID('ASI')
    {&TABLENAME}.updatedDate = DATE(TODAY)
    .
