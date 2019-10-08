&Scoped-define TABLENAME vendItemCostLevel

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.vendItemCostLevelID = NEXT-VALUE(vendItemCostLevelID_seq,ASI).

{methods/triggers/create.i}

ASSIGN 
    {&TABLENAME}.createdID = USERID('ASI')
    {&TABLENAME}.createdDate = DATE(TODAY)
    .
