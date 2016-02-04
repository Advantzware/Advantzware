&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobs

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


 FOR EACH jobcad OF jobs EXCLUSIVE-LOCK:
    DELETE jobcad.
 END.
 FOR EACH jobitems OF jobs EXCLUSIVE-LOCK:
    DELETE jobitems.
 END.
 FOR EACH jobmach OF jobs EXCLUSIVE-LOCK:
    DELETE jobmach.
 END.
 FOR EACH jobmatl OF jobs EXCLUSIVE-LOCK:
    DELETE jobmatl.
 END.
 FOR EACH jobnotes OF jobs EXCLUSIVE-LOCK:
    DELETE jobnotes.
 END.
 FOR EACH jobsheet OF jobs EXCLUSIVE-LOCK:
    DELETE jobsheet.
 END.
 FOR EACH jobstack OF jobs EXCLUSIVE-LOCK:
    DELETE jobstack.
 END.
