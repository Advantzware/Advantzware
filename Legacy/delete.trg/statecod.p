&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME statecod

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST state WHERE
     state.state EQ statecod.statecod
     NO-ERROR.

IF AVAIL state THEN
   DELETE state.
