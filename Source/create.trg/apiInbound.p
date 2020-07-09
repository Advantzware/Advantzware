&Scoped-define TABLENAME APIInbound

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN 
    {&TABLENAME}.apiInboundID = NEXT-VALUE(apiInboundID_seq)
    {&TABLENAME}.createBy     = USERID("ASI")
    {&TABLENAME}.createTime   = NOW      
    .