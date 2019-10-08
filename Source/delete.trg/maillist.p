&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME maillist

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

def buffer bf-cont for mailcont.
for each bf-cont of maillist :
    delete bf-cont.
end.
