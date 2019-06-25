
/* -------------------------------------------------- oe/ordltot.i 6/93 rd  */
/* o/e module - Calculate order line ext. price                              */
/* -------------------------------------------------------------------------- */

  def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
  def var lv-t-price as dec no-undo.


  find first itemfg   WHERE itemfg.company = prmComp
       itemfg.i-no eq ttUpdateEstItem.i-no
      no-lock no-error.
      
  assign
   v-tmp-price = if ttUpdateEstItem.pr-uom begins "L" AND ttUpdateEstItem.pr-uom NE "LB" then
                   if ttUpdateEstItem.qty lt 0 then -1 else 1
                 else
                 if ttUpdateEstItem.pr-uom eq "CS" then
                   ttUpdateEstItem.qty / (if ttUpdateEstItem.cas-cnt ne 0 then ttUpdateEstItem.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                 else
                 if ttUpdateEstItem.pr-uom eq "C" then
                   input ttUpdateEstItem.qty / 100
                 else
                 if input ttUpdateEstItem.pr-uom eq "M" then
                   input ttUpdateEstItem.qty / 1000
                 else
                   input ttUpdateEstItem.qty
                            
    lv-t-price = v-tmp-price * ttUpdateEstItem.price
    /*{1}.t-price:screen-value = string(round(lv-t-price - (lv-t-price * INPUT {1}.disc / 100),2)).*/
    ttUpdateEstItem.t-price = STRING(
          IF v-print-fmt EQ "Dayton" THEN 
            (lv-t-price - ROUND(lv-t-price * ttUpdateEstItem.disc / 100,2))
          ELSE
            ROUND(lv-t-price * (1 - (ttUpdateEstItem.disc / 100)),2)).

/* end ---------------------------------- copr. 1992  advanced software, inc. */

