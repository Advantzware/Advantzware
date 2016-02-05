&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME quote-vendor

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH quote-vendor-item OF quote-vendor:
  FOR EACH quote-vendor-qty OF quote-vendor-item:
      DELETE quote-vendor-qty.                         
  END.
  
  DELETE quote-vendor-item.                     
END.

find first ce-ctrl WHERE
     ce-ctrl.company = {&TABLENAME}.company AND
     ce-ctrl.loc EQ {&tablename}.loc
     exclusive-lock NO-ERROR NO-WAIT.

if avail ce-ctrl and {&TABLENAME}.q-no = ce-ctrl.num-len then
DO:
   ce-ctrl.num-len = ce-ctrl.num-len - 1.
   FIND CURRENT ce-ctrl NO-LOCK.
END.
