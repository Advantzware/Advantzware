/* t-styflu_corr.p  Run for Corrugated style flute scores*/

def buffer bf-ref for reftable.

FoR EACH REFTABLE WHERE REFTABle.reftable = "styflu" and code2 = "" TRANSACTION:
  find first bf-ref where bf-ref.reftable = reftable.reftable
           and bf-ref.company = reftable.company
           and bf-ref.loc = reftable.loc
           and bf-ref.code = reftable.code 
           and bf-ref.code2 = "1" no-lock no-error.
   if not avail bf-ref then do:
      create bf-ref.
      buffer-copy reftable EXCEPT rec_key to bf-ref
      ASSIGN
       bf-ref.code2 = "1"
       bf-ref.val = 0
       bf-ref.val[13] = reftable.val[13].
   end.
END.
           
