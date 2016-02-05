&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME quotehd

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH quoteitm OF quotehd:
  FOR EACH quoteqty OF quoteitm:
    {delete.trg/quoteqty.i}
    DELETE quoteqty.                         
  END.
  FOR EACH quotechg OF quoteitm:
    DELETE quotechg.                         
  END.
  DELETE quoteitm.                     
END.

FOR EACH quotechg OF quotehd:
  DELETE quotechg.                         
END.

find first ce-ctrl WHERE
     ce-ctrl.company = {&TABLENAME}.company AND
     ce-ctrl.loc EQ {&tablename}.loc
     exclusive-lock NO-ERROR NO-WAIT.

if avail ce-ctrl and {&TABLENAME}.q-no = ce-ctrl.q-num then
DO:
   ce-ctrl.q-num = ce-ctrl.q-num - 1.
   FIND CURRENT ce-ctrl NO-LOCK.
END.
