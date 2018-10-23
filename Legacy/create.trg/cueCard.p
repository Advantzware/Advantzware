&Scoped-define TABLENAME cueCard

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.cueID = NEXT-VALUE(cueCard_seq,ASI).

{methods/triggers/create.i}
