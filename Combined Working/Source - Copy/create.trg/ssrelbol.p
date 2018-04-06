&Scoped-define TABLENAME ssrelbol

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{custom/rec_key.i {&TABLENAME}}

ASSIGN
   {&TABLENAME}.created-date = TODAY
   {&TABLENAME}.created-id = USERID("NOSWEAT")
   {&TABLENAME}.created-time = TIME.
       
       

