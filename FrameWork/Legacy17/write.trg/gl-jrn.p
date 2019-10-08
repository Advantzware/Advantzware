&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME gl-jrn

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


IF {&TABLENAME}.user-id EQ "" THEN
  {&TABLENAME}.user-id  = USERID("nosweat").

IF {&TABLENAME}.reverse THEN {&TABLENAME}.from-reverse = NO.
IF {&TABLENAME}.from-reverse THEN {&TABLENAME}.reverse = NO.
