&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME stack-flute

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-stack-fl FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.


IF {&TABLENAME}.pallet NE "" THEN
FOR EACH b-stack-fl
    WHERE b-stack-fl.company EQ {&TABLENAME}.company
      AND b-stack-fl.loc     EQ {&TABLENAME}.loc
      AND b-stack-fl.code    EQ {&TABLENAME}.code
      AND b-stack-fl.pallet  EQ {&TABLENAME}.pallet
    BREAK BY b-stack-fl.page-no:
  
  IF FIRST(b-stack-fl.page-no)                AND
     ROWID(b-stack-fl) NE ROWID({&TABLENAME}) THEN RETURN ERROR.
  
  IF ROWID(b-stack-fl) NE ROWID({&TABLENAME}) THEN DELETE b-stack-fl.
END.
