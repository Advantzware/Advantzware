&Scoped-define TABLENAME users

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN
    {&TABLENAME}.track_usage    = TRUE.
