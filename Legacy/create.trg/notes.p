&Scoped-define TABLENAME notes

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN notes.createDate = TODAY
       notes.createTime = TIME
       notes.createUser = USERID("asi")
       notes.updateDate = TODAY
       notes.updateTime = TIME
       notes.updateUser = USERID("asi")       
       notes.note_date  = TODAY 
       notes.note_time  = TIME 
       notes.user_id    = USERID("asi")
       .
