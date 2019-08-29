&Scoped-define TABLENAME APIInboundEvent

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiInboundEventID = NEXT-VALUE(apiInboundEventID_seq).
