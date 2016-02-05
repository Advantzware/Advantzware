&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ar-mcash

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN
FOR EACH reftable
    WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key:

  DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable = "AR-MCASH" AND
    reftable.company  = {&TABLENAME}.company AND
    reftable.loc      = STRING({&TABLENAME}.m-no,">>>>>>9") AND
    reftable.code     = {&TABLENAME}.rec_key:
    DELETE reftable.
END.
