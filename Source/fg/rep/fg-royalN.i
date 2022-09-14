FOR EACH itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.cust-no GE fcus
    AND itemfg.cust-no LE tcus
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ itemfg.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
    AND itemfg.i-no    GE fitm
    AND itemfg.i-no    LE titm
    AND itemfg.procat  GE fcat
    AND itemfg.procat  LE tcat
    USE-INDEX {1} NO-LOCK
    
    BREAK BY itemfg.{2}:
    
    {custom/statusMsg.i " 'Processing FG item#  ' +  itemfg.i-no "}

    ASSIGN
        v-cases-pal = 0
        v-qty       = 0.
    
    FOR EACH fg-bin
        WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ itemfg.i-no
        USE-INDEX co-ino NO-LOCK
      
        BREAK BY fg-bin.i-no:
      
        IF FIRST(fg-bin.i-no) THEN
            v-cases-pal = fg-bin.cases-unit * fg-bin.units-pallet.
      
        CREATE w-qty.

        FOR EACH fg-rcpth
            WHERE fg-rcpth.company    EQ cocode
            AND fg-rcpth.i-no       EQ itemfg.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.trans-date LE vdat
            NO-LOCK USE-INDEX tran,

            EACH fg-rdtlh
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.loc       EQ fg-bin.loc
            AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
            AND fg-rdtlh.tag       EQ fg-bin.tag
            NO-LOCK

            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:
            
            {fg/fgmkbin1.i w-qty}
        END.
    END.
  
    FOR EACH w-qty:
        v-qty = v-qty + w-qty.
        DELETE w-qty.
    END.
  
    IF "{1}" NE "i-no" AND first-of(itemfg.{2}) AND v-break THEN 
    DO:
        v-page-brk = IF "{1}" EQ "customer" THEN
            ("Customer: " + itemfg.cust-no)
            ELSE
            ("Product Category:" + itemfg.procat).
  
        IF FIRST(itemfg.{2}) THEN VIEW FRAME r-top.
        PAGE.
    END.
  
    IF itemfg.sell-uom EQ "EA" THEN
        v-price = itemfg.sell-price.
    ELSE
        RUN sys/ref/convcuom.p (itemfg.sell-uom, "EA",
            0, 0, 0, 0,
            itemfg.sell-price, OUTPUT v-price).
                            
    v-val[1] = v-qty * v-price.
  
    IF v-qty NE 0 OR NOT v-exc THEN 
    DO:
  
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

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
          
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "item"    THEN 
                    cVarValue = STRING(itemfg.i-no,"x(15)") .
                WHEN "name"   THEN 
                    cVarValue = STRING(itemfg.i-name,"x(30)").
                WHEN "uom"   THEN 
                    cVarValue = STRING(itemfg.sell-uom,"x(3)").
                WHEN "sell-pr"  THEN 
                    cVarValue = STRING(itemfg.sell-price,">,>>>,>>9.99<<<<") .
                WHEN "qty-hand"   THEN 
                    cVarValue = STRING(v-qty,"->>>>>>>>9") .
                WHEN "val"  THEN 
                    cVarValue = STRING(v-val[1],"->,>>>,>>9.99") .
                WHEN "cust-part"   THEN 
                    cVarValue = STRING(itemfg.part-no,"x(15)") .
                WHEN "weight"  THEN 
                    cVarValue = STRING(itemfg.weight-100 / 100,">>>9.9<<<") .
                WHEN "cas-pal"   THEN 
                    cVarValue = STRING(v-cases-pal,">>>>,>>>,>>>") .
                WHEN "pack/ctn"  THEN 
                    cVarValue = STRING(itemfg.prod-notes,"x(10)") .
                         
            END CASE.
                      
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.
    END.

    ASSIGN
        v-val[2]  = v-val[2]  + v-val[1]
        v-qohs[1] = v-qohs[1] + v-qohi[1]
        v-qohs[2] = v-qohs[2] + v-qohi[2]
        v-qohs[3] = v-qohs[3] + v-qohi[3]
        v-qohs[4] = v-qohs[4] + v-qohi[4]
        v-qohs[5] = v-qohs[5] + v-qohi[5]
   
        v-val[1]  = 0
        v-qohi    = 0.
  
    IF LAST-OF(itemfg.{2}) THEN 
    DO:
        v-qty = 0.
        DO v = 1 TO 5:
            v-qty = v-qty + v-qohs[v].
        END.
  
        IF (v-qty NE 0 OR NOT v-exc) AND "{1}" NE "i-no" THEN 
        DO:  

            /* gdm - 04210913 */
            PUT SKIP(1).

            IF "{1}" EQ "procat" 
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

        END.

        ASSIGN
            v-val[3]  = v-val[3]  + v-val[2]
            v-qohg[1] = v-qohg[1] + v-qohs[1]
            v-qohg[2] = v-qohg[2] + v-qohs[2]
            v-qohg[3] = v-qohg[3] + v-qohs[3]
            v-qohg[4] = v-qohg[4] + v-qohs[4]
            v-qohg[5] = v-qohg[5] + v-qohs[5]

            v-val[2]  = 0
            v-qohs    = 0.
    END.

    IF LAST(itemfg.{2}) THEN 
    DO:
        v-qty = 0.
        DO v = 1 TO 5:
            v-qty = v-qty + v-qohg[v].
        END.
  
        IF v-qty NE 0 OR NOT v-exc THEN 
        DO:

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

        END.
    END.
END.  
    
