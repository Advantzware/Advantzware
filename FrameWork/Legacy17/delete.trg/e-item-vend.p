&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME e-item-vend

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


IF TRIM({&TABLENAME}.rec_key) NE "" THEN
   FOR EACH reftable
       WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
         AND reftable.reftable EQ "e-item-vend.adders"
       USE-INDEX rec_key:

     DELETE reftable.
   END.

FOR EACH reftable WHERE
    reftable.reftable = "vend-qty" AND
    reftable.company = e-item-vend.company and
    reftable.CODE    = e-item-vend.i-no AND
    reftable.code2   = e-item-vend.vend-no:

    DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable = "vend-cost" AND
    reftable.company = e-item-vend.company and
    reftable.CODE    = e-item-vend.i-no AND
    reftable.code2   = e-item-vend.vend-no:

    DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable = "vend-setup" AND
    reftable.company = e-item-vend.company and
    reftable.CODE    = e-item-vend.i-no AND
    reftable.code2   = e-item-vend.vend-no:
   
    DELETE reftable.
END.

   
   

