&Scoped-define TABLENAME ef

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


ASSIGN
 {&TABLENAME}.n-out   = 1
 {&TABLENAME}.n-out-l = 1
 {&TABLENAME}.n-out-d = 1.
