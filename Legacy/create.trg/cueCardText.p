&Scoped-define TABLENAME cueCardText

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.cueTextID = NEXT-VALUE(cueText_seq,ASI).

{methods/triggers/create.i}
