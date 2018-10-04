
SESSION:SET-WAIT-STATE ("general").

FOR EACH itemfg:
  RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).
  FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no:
     RUN fg/calcqono.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-ono).
  END.  
END.

SESSION:SET-WAIT-STATE ("").
