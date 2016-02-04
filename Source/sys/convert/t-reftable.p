 for each reftable where reftable = "itemfg.exempt-disc"  and val[1] = 1 no-lock:
disp company code val[1].


/*
def temp-table tt-ref like reftable.
def var i as int no-undo.

input from d:\ddata\reftable.d no-echo.

repeat:
   create tt-ref.
   set tt-ref.reftable form "x(20)"
          tt-ref.company form "x(10)"
          tt-ref.loc form "x(20)"
          tt-ref.code form "x(30)"
          tt-ref.code2 form "x(30)"
          tt-ref.dscr form "x(50)"
          tt-ref.val.
   
   if tt-ref.reftable = "itemfg.exempt-disc" then do:
      find first reftable where reftable.reftable = tt-ref.reftable
                 and reftable.company = "001"
                 and reftable.loc = tt-ref.loc
                 and reftable.code = tt-ref.code  no-error.
      if not avail reftable then do:
         create reftable.
         buffer-copy tt-ref to reftable.
      end.
      else do:
         if reftable.val[1] = 0 then reftable.val[1] = tt-ref.val[1].
      end.
      
      
   end.
   
 end.
 */
