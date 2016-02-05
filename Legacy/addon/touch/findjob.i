/* touch/findjob.i */

   /* it override brwstrg.i */  
on return of auto_find in frame {&frame-name}
do:
   ASSIGN auto_find
  
      auto_find = fill(" ", 6 - length(auto_find)) + auto_find. 
  
   find-auto = yes.
   APPLY "ANY-PRINTABLE" TO {&BROWSE-NAME}.
   find-auto = no.
end.
