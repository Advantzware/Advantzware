FOR each itemfg
    where itemfg.company eq cocode
      and itemfg.cust-no ge fcus
      and itemfg.cust-no le tcus
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
         AND ttCustList.log-fld no-lock) else true)
      and itemfg.i-no    ge fitm
      and itemfg.i-no    le titm
      and itemfg.procat  ge fcat
      and itemfg.procat  le tcat
    use-index {1} no-lock
    
    break by itemfg.{2}:
    
    {custom/statusMsg.i " 'Processing FG item#  ' +  itemfg.i-no "}

  assign
   v-cases-pal = 0
   v-qty       = 0.
    
  for each fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq itemfg.i-no
      use-index co-ino no-lock
      
      break by fg-bin.i-no:
      
    if first(fg-bin.i-no) then
      v-cases-pal = fg-bin.cases-unit * fg-bin.units-pallet.
      
    create w-qty.

    for each fg-rcpth
        where fg-rcpth.company    eq cocode
          and fg-rcpth.i-no       eq itemfg.i-no
          and fg-rcpth.job-no     eq fg-bin.job-no
          and fg-rcpth.job-no2    eq fg-bin.job-no2
          and fg-rcpth.trans-date le vdat
        no-lock use-index tran,

        each fg-rdtlh
        where fg-rdtlh.r-no      eq fg-rcpth.r-no
          and fg-rdtlh.rita-code eq fg-rcpth.rita-code
          and fg-rdtlh.loc       eq fg-bin.loc
          and fg-rdtlh.loc-bin   eq fg-bin.loc-bin
          and fg-rdtlh.tag       eq fg-bin.tag
        no-lock

        by fg-rcpth.trans-date
        BY fg-rdtlh.trans-time
        by fg-rcpth.r-no:
            
      {fg/fgmkbin1.i w-qty}
    end.
  end.
  
  for each w-qty:
    v-qty = v-qty + w-qty.
    delete w-qty.
  end.
  
  if "{1}" ne "i-no" and first-of(itemfg.{2}) and v-break then do:
    v-page-brk = if "{1}" eq "customer" then
                   ("Customer: " + itemfg.cust-no)
                 else
                   ("Product Category:" + itemfg.procat).
  
    if first(itemfg.{2}) then view frame r-top.
    page.
  end.
  
  if itemfg.sell-uom eq "EA" then
    v-price = itemfg.sell-price.
  else
    run sys/ref/convcuom.p (itemfg.sell-uom, "EA",
                            0, 0, 0, 0,
                            itemfg.sell-price, output v-price).
                            
  v-val[1] = v-qty * v-price.
  
  if v-qty ne 0 or not v-exc then do:
  
    /* gdm - 04210913 */
    /*IF v-item# 
      THEN PUT UNFORMATTED itemfg.i-no FORMAT "x(16)".
        
    IF v-itemnm 
      THEN PUT UNFORMATTED itemfg.i-name FORMAT "x(31)".

    IF v-UOM     
      THEN PUT UNFORMATTED itemfg.sell-uom FORMAT "x(4)".
                                      
    IF v-Sellprc 
      THEN PUT UNFORMATTED STRING(itemfg.sell-price,">,>>>,>>9.99<<<<") " ".
                                      
    IF v-QOH     
      THEN PUT UNFORMATTED STRING(v-qty,"->>>>>>>>>9") " ".

    IF v-Value   
      THEN PUT UNFORMATTED STRING(v-val[1],"->,>>>,>>9.99") " ".
                                      
    IF v-Cstprt  
      THEN PUT UNFORMATTED itemfg.part-no FORMAT "x(13)".
                                      
    IF v-Weight  
      THEN PUT UNFORMATTED STRING(itemfg.weight-100 / 100,">>>9.9<<<") " ".

    IF v-CsPl    
      THEN PUT UNFORMATTED STRING(v-cases-pal,">>,>>>") " ".
                                      
    IF v-PckCnt  
      THEN PUT UNFORMATTED itemfg.prod-notes  FORMAT "x(11)".

    PUT SKIP.*/

      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item"    THEN cVarValue = string(itemfg.i-no,"x(15)") .
                         WHEN "name"   THEN cVarValue = string(itemfg.i-name,"x(30)").
                         WHEN "uom"   THEN cVarValue = STRING(itemfg.sell-uom,"x(3)").
                         WHEN "sell-pr"  THEN cVarValue = STRING(itemfg.sell-price,">,>>>,>>9.99<<<<") .
                         WHEN "qty-hand"   THEN cVarValue = STRING(v-qty,"->>>>>>>>9") .
                         WHEN "val"  THEN cVarValue = STRING(v-val[1],"->,>>>,>>9.99") .
                         WHEN "cust-part"   THEN cVarValue = STRING(itemfg.part-no,"x(15)") .
                         WHEN "weight"  THEN cVarValue = STRING(itemfg.weight-100 / 100,">>>9.9<<<") .
                         WHEN "cas-pal"   THEN cVarValue = STRING(v-cases-pal,">>>>,>>>,>>>") .
                         WHEN "pack/ctn"  THEN cVarValue = STRING(itemfg.prod-notes,"x(10)") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
            END.
  end.

  assign
   v-val[2]  = v-val[2]  + v-val[1]
   v-qohs[1] = v-qohs[1] + v-qohi[1]
   v-qohs[2] = v-qohs[2] + v-qohi[2]
   v-qohs[3] = v-qohs[3] + v-qohi[3]
   v-qohs[4] = v-qohs[4] + v-qohi[4]
   v-qohs[5] = v-qohs[5] + v-qohi[5]
   
   v-val[1]  = 0
   v-qohi    = 0.
  
  if last-of(itemfg.{2}) then do:
    v-qty = 0.
    do v = 1 to 5:
      v-qty = v-qty + v-qohs[v].
    end.
  
    if (v-qty ne 0 or not v-exc) and "{1}" ne "i-no" then do:  

      /* gdm - 04210913 */
      PUT SKIP(1).

      IF "{1}" eq "procat" 
        THEN
          PUT UNFORMATTED 
             "Product Category " + itemfg.procat + " Totals:" AT 17.
        ELSE 
          PUT UNFORMATTED 
             "Customer " + itemfg.cust-no + " Totals:" AT 17.

      PUT UNFORMATTED 
       "Qty : "                         AT 48
       STRING(v-qty,"->>>>>>>>>9")      AT 54
       "Value : "                       AT 80
       STRING(v-val[2],"->,>>>,>>9.99") AT 88.
          
      PUT SKIP(1).
      /* gdm - 04210913 end */

    end.

    assign
     v-val[3]  = v-val[3]  + v-val[2]
     v-qohg[1] = v-qohg[1] + v-qohs[1]
     v-qohg[2] = v-qohg[2] + v-qohs[2]
     v-qohg[3] = v-qohg[3] + v-qohs[3]
     v-qohg[4] = v-qohg[4] + v-qohs[4]
     v-qohg[5] = v-qohg[5] + v-qohs[5]

     v-val[2] = 0
     v-qohs   = 0.
  end.

  if last(itemfg.{2}) then do:
    v-qty = 0.
    do v = 1 to 5:
      v-qty = v-qty + v-qohg[v].
    end.
  
    if v-qty ne 0 or not v-exc then do:

      /* gdm - 04210913 end */
      PUT SKIP(1).

      PUT UNFORMATTED 
          "Grand Totals:"                  AT 17
          "Qty : "                         AT 48
           STRING(v-qty,"->>>>>>>>>9")      AT 54
           "Value : "                       AT 80
           STRING(v-val[3],"->,>>>,>>9.99") AT 88
         SKIP(2).
      /* gdm - 04210913 end */

    end.
  end.
end.  
    
