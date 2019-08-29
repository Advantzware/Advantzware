&Scoped-define TABLENAME APIOutboundDetail

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiOutboundDetailID = NEXT-VALUE(apiOutboundDetailID_seq).
