&Scoped-define TABLENAME APIInboundDetail

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiInboundDetailID = NEXT-VALUE(apiInboundDetailID_seq).
