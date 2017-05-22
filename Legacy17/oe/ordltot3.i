/* -------------------------------------------------- oe/ordltot3.i 6/93 rd  */
/*  Calculate order line ext. price same as ordltot.i but no screen/input                    */
/* -------------------------------------------------------------------------- */

  def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
  def var lv-t-price as dec no-undo.

/*
  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no eq {1}.i-no
      no-lock no-error.
*/      
  assign
   v-tmp-price = if {1}.pr-uom begins "L" AND {1}.pr-uom NE "LB" then
                   if  {1}.{2} lt 0 then -1 else 1
                 else
                 if  {1}.pr-uom eq "CS" then
                    {1}.{2} / (if {1}.cas-cnt ne 0 then  {1}.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                 else
                 if  {1}.pr-uom eq "C" then
                    {1}.{2} / 100
                 else
                 if  {1}.pr-uom eq "M" then
                    {1}.{2} / 1000
                 else
                    {1}.{2}
                            
    lv-t-price = v-tmp-price *  {1}.price
     {1}.t-price =  ROUND(lv-t-price * (1 - ( {1}.disc / 100)),2).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
