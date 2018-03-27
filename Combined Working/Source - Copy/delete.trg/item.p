&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME item

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


DISABLE TRIGGERS FOR LOAD OF e-item.
DISABLE TRIGGERS FOR LOAD OF e-item-vend.
DISABLE TRIGGERS FOR LOAD OF stack-flute.
DISABLE TRIGGERS FOR LOAD OF stack-size.

for each e-item of item:
    delete e-item.
end.
for each e-item-vend where e-item-vend.company = item.company and
                           e-item-vend.i-no = item.i-no:
    delete e-item-vend.
end.    
for each /*  old reftable where reftable.reftable eq "FLUTE"
                  and reftable.company  eq ""
                  and reftable.loc      eq ""
                  */
               flute  no-lock,
          each stack-flute  where stack-flute.company eq item.company
                              and stack-flute.loc     eq item.loc
                              and stack-flute.code    eq flute.code
                              and stack-flute.pallet  eq item.i-no:
               delete stack-flute.
end.
for each stack-size where stack-size.company eq item.company
                            and stack-size.loc     eq item.loc
                            and stack-size.pallet  eq item.i-no:
          delete stack-size.
end.
