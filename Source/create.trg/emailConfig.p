&Scoped-define TABLENAME emailConfig

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}
{&TABLENAME}.configID = NEXT-VALUE(configID_seq).
