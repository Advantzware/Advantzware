&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fg-rdtl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


{&TABLENAME}.qty = ({&TABLENAME}.cases * {&TABLENAME}.qty-case) + {&TABLENAME}.partial.
