&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME mach-part

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST reftable WHERE
     reftable.reftable EQ "MACHPARTHOURS" AND
     reftable.company  EQ mach-part.company AND
     reftable.loc      EQ mach-part.m-code AND
     reftable.code     EQ mach-part.rm-part-code
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.
