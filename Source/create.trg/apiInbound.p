&Scoped-define TABLENAME APIInbound

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiInboundID = NEXT-VALUE(apiInboundID_seq).
