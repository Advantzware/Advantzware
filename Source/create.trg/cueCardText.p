&Scoped-define TABLENAME cueCardText

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

ASSIGN
    {&TABLENAME}.cueTextID   = NEXT-VALUE(cueText_seq,ASI)
    {&TABLENAME}.createdBy   = USERID("ASI")
    {&TABLENAME}.createdDate = TODAY
    {&TABLENAME}.createdTime = TIME
    .
{methods/triggers/create.i}
