&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME custype

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH sman-mtx
    WHERE sman-mtx.company EQ {&TABLENAME}.company
      AND sman-mtx.custype EQ {&TABLENAME}.custype:
  DELETE sman-mtx.
END.

IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "smanmtrx") THEN
  RUN custom/delsmtrx.p ({&TABLENAME}.company, 4, {&TABLENAME}.custype).
