&Scoped-define TABLENAME setting

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.settingID = NEXT-VALUE(settingID_seq).
