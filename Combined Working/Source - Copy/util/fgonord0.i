
for each itemfg
    {sys/look/itemfgW.i}
      and (v-pur-man             eq "B"                                  or
	   (v-pur-man             eq "P"      and itemfg.pur-man)         or
	   (v-pur-man             eq "M"      and (not itemfg.pur-man)))
    exclusive-lock:

  status default "Processing FG Item: " + string(itemfg.i-no).

  itemfg.q-ono = 0.
  FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no
                      EXCLUSIVE-LOCK:
      itemfg-loc.q-ono = 0.    
  END.
end.

