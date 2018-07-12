&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME userLanguage

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


// Test Gilles 1
