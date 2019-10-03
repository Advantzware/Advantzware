&Scoped-define TABLENAME vendItemCost

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.vendItemCostID = NEXT-VALUE(vendItemCostID_seq,ASI).

{methods/triggers/create.i}

ASSIGN 
    {&TABLENAME}.createdID = USERID('ASI')
    {&TABLENAME}.createdDate = DATE(TODAY)
    {&TABLENAME}.dimUOM = "IN"
    .
