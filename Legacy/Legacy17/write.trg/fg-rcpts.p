&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-rcpts

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


{&TABLENAME}.po-no = TRIM(STRING(INT({&TABLENAME}.po-no),">>>>>>>>>>")).
