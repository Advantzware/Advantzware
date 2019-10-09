&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME truck

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST reftable WHERE
     reftable.reftable EQ "msf-limit" AND
     reftable.company  EQ truck.company AND
     reftable.loc      EQ truck.loc AND 
     reftable.CODE     EQ truck.carrier AND
     reftable.code2    EQ truck.truck-code
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.
