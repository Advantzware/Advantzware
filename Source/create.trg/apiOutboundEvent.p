&Scoped-define TABLENAME APIOutboundEvent

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiOutboundEventID = NEXT-VALUE(apiOutboundEventID_seq).
