&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME mach

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

do: {est/del-mach.i } end.

FOR EACH mach-part WHERE
    mach-part.company EQ {&TABLENAME}.company AND
    mach-part.m-code EQ {&TABLENAME}.m-code
    EXCLUSIVE-LOCK:
    DELETE mach-part.
END.

