&Scoped-define TABLENAME ssrelbol

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN
   {&TABLENAME}.created-date = TODAY
   {&TABLENAME}.created-id = USERID("NOSWEAT")
   {&TABLENAME}.created-time = TIME.
       
       

