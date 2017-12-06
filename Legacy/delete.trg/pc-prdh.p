&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME pc-prdh

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


DISABLE TRIGGERS FOR LOAD OF pc-prdd.

FOR EACH pc-prdd
    WHERE pc-prdd.company EQ {&TABLENAME}.company
      AND pc-prdd.m-code  EQ {&TABLENAME}.m-code
      AND pc-prdd.op-date EQ {&TABLENAME}.trans-date
      AND pc-prdd.shift   EQ {&TABLENAME}.shift
      AND NOT CAN-FIND(FIRST b-{&TABLENAME}
                       WHERE b-{&TABLENAME}.company    EQ pc-prdd.company
                         AND b-{&TABLENAME}.m-code     EQ pc-prdd.m-code
                         AND b-{&TABLENAME}.trans-date EQ pc-prdd.op-date
                         AND b-{&TABLENAME}.shift      EQ pc-prdd.shift
                         AND ROWID(b-{&TABLENAME})     NE ROWID({&TABLENAME})):
  DELETE pc-prdd.
END.
