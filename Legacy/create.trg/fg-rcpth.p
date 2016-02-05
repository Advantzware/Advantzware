&Scoped-define TABLENAME fg-rcpth

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


ASSIGN
 {&TABLENAME}.user-id  = USERID('nosweat')
 {&TABLENAME}.upd-date = TODAY
 {&TABLENAME}.upd-time = TIME
 {&TABLENAME}.create-by  = USERID('nosweat').
