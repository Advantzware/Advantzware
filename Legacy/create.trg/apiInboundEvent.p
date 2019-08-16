&Scoped-define TABLENAME APIInboundEvent

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.eventID = NEXT-VALUE(EventIDSeq).
