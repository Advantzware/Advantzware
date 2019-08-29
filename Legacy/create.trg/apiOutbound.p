&Scoped-define TABLENAME APIOutbound

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiOutboundID = NEXT-VALUE(apiOutboundID_seq).
