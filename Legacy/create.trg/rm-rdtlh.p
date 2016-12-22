&Scoped-define TABLENAME rm-rdtlh

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


ASSIGN
 {&TABLENAME}.user-id  = USERID("ASI")
 {&TABLENAME}.upd-date = TODAY
 {&TABLENAME}.upd-time = TIME.
