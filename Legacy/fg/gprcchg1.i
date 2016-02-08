/* -------------------------------------------------- rm/gprcchg1.i 08/96 JLF */
/* Global Price Change by Mat-type                                            */
/* -------------------------------------------------------------------------- */

for each itemfg
    {sys/look/itemfgW.i}
      and itemfg.i-no     ge v-i-no[1]
      and itemfg.i-no     le v-i-no[2]
      AND NOT CAN-FIND(FIRST tt-rowid
                       WHERE tt-rowid.row-id EQ ROWID(itemfg))
    use-index mat-type,
    
    first e-itemfg of itemfg:

  CREATE tt-rowid.
  tt-rowid.row-id = ROWID(itemfg).
    
  itemfg.price-code = v-price-code.
  
  if itemfg.vend-no ge v-vend-no[1] and
     itemfg.vend-no le v-vend-no[2] then do:

    {rm/gprcchg.i "v-pct" "amount_chg" "fg"}
  end.
  
  for each e-itemfg-vend of e-itemfg
      where e-itemfg-vend.itemfg-type eq yes 
        and e-itemfg-vend.vend-no     ge v-vend-no[1]
        and e-itemfg-vend.vend-no     le v-vend-no[2]:
        
    {rm/gprcchg.i "v-pct" "amount_chg" "fg-vend"}
  end.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
