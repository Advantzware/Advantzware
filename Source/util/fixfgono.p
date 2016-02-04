FOR EACH itemfg BY company BY i-no TRANSACTION:
  DISPLAY itemfg.company LABEL "Company"
          itemfg.i-no    LABEL "FG Item#" FORMAT "x(20)".
  RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).
  
  FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no:
     RUN fg/calcqool.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-ono).
  END.
                      
END.
