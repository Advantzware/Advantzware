&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME mstd

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
for each mmtx of mstd :
    delete mmtx.
end.

for each mmty of mstd:
    delete mmty.
end.
