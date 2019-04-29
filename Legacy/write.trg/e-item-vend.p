&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME e-item-vend

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF VAR v-count AS INT NO-UNDO.

{methods/triggers/write.i}

{custom/e-item-edit.i {&TABLENAME}}
MESSAGE PROGRAM-NAME(2) SKIP 
PROGRAM-NAME(3) SKIP  
PROGRAM-NAME(4) SKIP 
PROGRAM-NAME(5)
VIEW-AS ALERT-BOX.    
ASSIGN
    {&TABLENAME}.updated-id[1]   = USERID("NOSWEAT")
    {&TABLENAME}.updated-date[1] = TODAY.
