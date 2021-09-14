&Scoped-define TABLENAME settingType

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.settingTypeID = NEXT-VALUE(settingTypeID_seq).
