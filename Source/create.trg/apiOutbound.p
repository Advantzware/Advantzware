&Scoped-define TABLENAME APIOutbound

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN 
    {&TABLENAME}.apiOutboundID = NEXT-VALUE(apiOutboundID_seq)
    {&TABLENAME}.createBy      = USERID("ASI")
    {&TABLENAME}.createTime    = NOW      
    .
