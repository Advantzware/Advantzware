&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME freight-class

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH frt-class-desc-trans WHERE
    frt-class-desc-trans.freight-class EQ {&TABLENAME}.freight-class
    EXCLUSIVE-LOCK:
  
    DELETE frt-class-desc-trans.
END.
