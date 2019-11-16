&Scoped-define TABLENAME emailConfig

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN
    {&TABLENAME}.configID   = NEXT-VALUE(configID_seq)
    {&TABLENAME}.createBy   = USERID("ASI")
    {&TABLENAME}.createTime = NOW
    .
