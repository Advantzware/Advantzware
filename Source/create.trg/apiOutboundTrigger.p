&Scoped-define TABLENAME APIOutboundTrigger

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.apiOutboundTriggerID = NEXT-VALUE(apiOutboundTriggerID_seq).
