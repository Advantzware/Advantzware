&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME e-item

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH reftable where
    reftable.reftable EQ "blank-vend-qty" and
    reftable.company = e-item.company and
    reftable.CODE    = e-item.i-no:
          
    DELETE reftable.
END.

FOR EACH reftable where
    reftable.reftable EQ "blank-vend-cost" and
    reftable.company = e-item.company and
    reftable.CODE    = e-item.i-no:
          
    DELETE reftable.
END.
       

       
 
