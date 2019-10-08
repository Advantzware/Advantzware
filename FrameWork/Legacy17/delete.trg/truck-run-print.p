&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME truck-run-print

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST reftable WHERE
     reftable.reftable = "trp-car" AND
     reftable.rec_key  = truck-run-print.rec_key
     USE-INDEX rec_key
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.
