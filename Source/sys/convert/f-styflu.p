/*for each reftable where reftable.reftable = "styflu".
disp company loc code reftable.reftable.
*/
def temp-table tt-ref like reftable.

input from c:\ddata\reftable.d no-echo.

repeat.
   create tt-ref.   
   import tt-ref.
   
   if tt-ref.reftable = "styflu" then do:
      find first reftable where reftable.reftable = tt-ref.reftable 
           and reftable.company = tt-ref.company
            and reftable.loc = tt-ref.loc
            and reftable.code = tt-ref.code no-lock no-error.
      if not avail reftable then do:
         create reftable.
         buffer-copy tt-ref to reftable.     
         disp reftable.code.
         pause 0.
      end.
                  
   end.
   end.
   
