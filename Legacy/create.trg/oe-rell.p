&Scoped-define TABLENAME oe-rell

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}
ASSIGN 
  {&TABLENAME}.enteredBy = USERID("asi")
  {&TABLENAME}.enteredDT = DATETIME(TODAY, MTIME)
  . 