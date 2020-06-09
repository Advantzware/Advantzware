&Scoped-define TABLENAME APIOutboundTrigger

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN 
    {&TABLENAME}.apiOutboundTriggerID = NEXT-VALUE(apiOutboundTriggerID_seq)
    {&TABLENAME}.createBy             = USERID("ASI")
    {&TABLENAME}.createTime           = NOW      
    .