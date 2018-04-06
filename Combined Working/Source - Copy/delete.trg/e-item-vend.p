&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
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





   
   

