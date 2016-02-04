def buffer strap for reftable.

for each reftable where reftable.reftable = "stack".
disp code.
find first strap where strap.reftable = "stackstrap" and
        strap.code = reftable.code no-error.
        if not available strap then do:
        disp "no strap".
        create strap.
        assign strap.reftable = "STACKSTRAP"
               strap.company = ""
               strap.loc = ""
               strap.code = reftable.code.
               
        end.
        
        
        end.
        
