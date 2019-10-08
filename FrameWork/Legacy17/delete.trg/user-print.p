&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME user-print

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

/* rstark 6.27.2015 #06261504
FOR EACH user-batch EXCLUSIVE-LOCK
    WHERE user-batch.company = {&TABLENAME}.company
      AND user-batch.batch-seq = {&TABLENAME}.batch-seq
      AND user-batch.prog-seq = {&TABLENAME}.prog-seq:
  DELETE user-batch.
END.
*/
