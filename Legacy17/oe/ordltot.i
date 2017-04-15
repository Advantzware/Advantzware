/* -------------------------------------------------- oe/ordltot.i 6/93 rd  */
/* o/e module - Calculate order line ext. price                              */
/* -------------------------------------------------------------------------- */

  def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
  def var lv-t-price as dec no-undo.


  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no eq input {1}.i-no
      no-lock no-error.
      
  assign
   v-tmp-price = if input {1}.pr-uom begins "L" AND input {1}.pr-uom NE "LB" then
                   if INPUT {1}.{2} lt 0 then -1 else 1
                 else
                 if input {1}.pr-uom eq "CS" then
                   input {1}.{2} / (if INPUT {1}.cas-cnt ne 0 then INPUT {1}.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                 else
                 if input {1}.pr-uom eq "C" then
                   input {1}.{2} / 100
                 else
                 if input {1}.pr-uom eq "M" then
                   input {1}.{2} / 1000
                 else
                   input {1}.{2}
                            
    lv-t-price = v-tmp-price * input {1}.price
    /*{1}.t-price:screen-value = string(round(lv-t-price - (lv-t-price * INPUT {1}.disc / 100),2)).*/
    {1}.t-price:screen-value = STRING(
          IF v-print-fmt EQ "Dayton" THEN 
            (lv-t-price - ROUND(lv-t-price * INPUT {1}.disc / 100,2))
          ELSE
            ROUND(lv-t-price * (1 - (INPUT {1}.disc / 100)),2)).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
