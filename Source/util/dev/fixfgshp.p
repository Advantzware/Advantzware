
{sys/inc/var.i new shared}

SESSION:SET-WAIT-STATE ("general").

for each itemfg where isaset and alloc and
can-find(first fg-set where fg-set.company eq itemfg.company
                        and fg-set.set-no eq itemfg.i-no
                        and fg-set.part-no eq itemfg.i-no)
   no-lock,                        
   each fg-rcpth where fg-rcpth.company eq itemfg.company
                      and fg-rcpth.i-no eq itemfg.i-no
                      and fg-rcpth.rita-code eq "S",
      each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no
                      and fg-rdtlh.rita-code eq fg-rcpth.rita-code
                    break by itemfg.company BY itemfg.i-no by b-no by fg-rcpth.r-no:

  cocode = itemfg.company. 

  if last-of(b-no) and not first-of(b-no) then do:
    DELETE fg-rdtlh.
    DELETE fg-rcpth.
  END.

  IF LAST-OF(itemfg.i-no) THEN RUN fg/fg-reset.p (RECID(itemfg)).
end.      

SESSION:SET-WAIT-STATE ("").

