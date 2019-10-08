&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-cash

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


IF ar-cash.posted THEN DO:
  MESSAGE "Record already posted, no deletion allowed..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

IF {&TABLENAME}.c-no NE 0 THEN
FOR EACH ar-cashl WHERE ar-cashl.c-no EQ {&TABLENAME}.c-no:
  DELETE ar-cashl.
END.


