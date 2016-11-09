/*------------------------------------------------------------------------
  File: rd-fgexp.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Finished Goods Export.rpa */
{aoa/tempTable/ttFinishedGoodsExport.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttFinishedGoodsExport.
{aoa/includes/pFinishedGoodsExport.i}

/* local variables */
DEFINE BUFFER bItemFG FOR itemfg.

DEFINE VARIABLE cStyle          LIKE eb.style                         NO-UNDO.
DEFINE VARIABLE dLen            LIKE eb.len                           NO-UNDO.
DEFINE VARIABLE dWid            LIKE eb.wid                           NO-UNDO.
DEFINE VARIABLE dDep            LIKE eb.dep                           NO-UNDO.
DEFINE VARIABLE dBlen           LIKE eb.t-len                         NO-UNDO.
DEFINE VARIABLE dBwid           LIKE eb.t-wid                         NO-UNDO.
DEFINE VARIABLE cCad            LIKE eb.cad-no                        NO-UNDO.
DEFINE VARIABLE cDie            LIKE eb.die-no                        NO-UNDO.
DEFINE VARIABLE cPlate          LIKE eb.plate                         NO-UNDO.
DEFINE VARIABLE cBoard          AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE cBoard2         AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE iInkCnt         AS   INTEGER                          NO-UNDO.
DEFINE VARIABLE iCoatCnt        AS   INTEGER                          NO-UNDO.
DEFINE VARIABLE cInk            AS   CHARACTER FORM "x(10)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE cCoat           AS   CHARACTER FORM "x(10)" EXTENT 10 NO-UNDO. 
DEFINE VARIABLE cCaseCount      LIKE itemfg.case-count                NO-UNDO.
DEFINE VARIABLE cPalletCount    LIKE itemfg.case-count                NO-UNDO.
DEFINE VARIABLE cCasesPerPallet LIKE itemfg.case-count                NO-UNDO.
DEFINE VARIABLE dClip           LIKE item.cal                         NO-UNDO.
DEFINE VARIABLE cTrNno          AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cTrName         AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cCasNo          AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cCasName        AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cSpecNote       AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE i               AS   INTEGER                          NO-UNDO.
DEFINE VARIABLE dFuncAlloc      AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE cShipMethod     AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE iPeriod         AS   INTEGER                          NO-UNDO.

FIND FIRST period NO-LOCK
     WHERE period.company EQ ipcCompany 
       AND period.pstat EQ TRUE
       AND period.pst   LE TODAY
       AND period.pend  GE TODAY
     NO-ERROR.
IF NOT AVAILABLE period THEN
FIND LAST period NO-LOCK
     WHERE period.company EQ ipcCompany
       AND period.pstat   EQ TRUE
     NO-ERROR.
IF AVAILABLE period THEN
iPeriod = period.pnum.

/* subject business logic */
FOR EACH bItemFG NO-LOCK 
    WHERE bItemFG.company EQ ipcCompany
      AND bItemFG.i-no    GE cStartItemNo
      AND bItemFG.i-no    LE cEndItemNo
      AND bItemFG.i-name  GE cStartItemName
      AND bItemFG.i-name  LE cEndItemName
      AND bItemFG.part-no GE cStartCustPart
      AND bItemFG.part-no LE cEndCustPart
      AND bItemFG.cust-no GE cStartCustNo
      AND bItemFG.cust-no LE cEndCustNo
      AND bItemFG.est-no  GE cStartEstimate
      AND bItemFG.est-no  LE cEndEstimate
      AND bItemFG.style   GE cStartStyle
      AND bItemFG.style   LE cEndStyle
      AND bItemFG.procat  GE cStartProdCategory
      AND bItemFG.procat  LE cEndProdCategory
      AND (((bItemFG.stat EQ "A"
       OR bItemFG.stat    EQ "")
      AND lActive)
       OR (bItemFG.stat   EQ "I"
      AND lInactive))
    :
    ASSIGN
        dLen = 0     
        dWid = 0    
        dDep = 0     
        dBlen = 0  
        dBwid = 0 
        cCad = ""  
        cDie = ""  
        cPlate = ""
        cTrNno = "" 
        cTrName = ""
        cCasNo = ""
        cCasName = ""
        cBoard = ""
        cBoard2 = ""
        cCaseCount = 0
        cPalletCount = 0
        cCasesPerPallet = 0 
        dClip = 0
        cSpecNote = ""
        .
    IF lSpecNote THEN DO:
        FOR EACH notes NO-LOCK 
            WHERE notes.rec_key EQ bItemFG.rec_key
            :
            IF AVAILABLE notes THEN
            cSpecNote = cSpecNote + " " + notes.note_text.
        END.
    END.

    FIND FIRST est NO-LOCK
         WHERE est.company EQ ipcCompany
           AND est.est-no  EQ bItemFG.est-no
         NO-ERROR.
    IF bItemFG.ship-meth THEN
    ASSIGN
        cCaseCount = bItemFG.case-count
        cPalletCount = 0
        .
    ELSE
    ASSIGN
        cCaseCount = 0
        cPalletCount = bItemFG.case-count
        .
    
    FIND FIRST eb NO-LOCK 
        WHERE eb.company  EQ ipcCompany
          AND eb.est-no   EQ bItemFG.est-no 
          AND eb.stock-no EQ bItemFG.i-no NO-ERROR.
    
    IF AVAILABLE eb THEN DO:
        FIND FIRST ef NO-LOCK
             WHERE ef.company EQ ipcCompany
               AND ef.est-no  EQ bItemFG.est-no 
               AND ef.form-no EQ eb.form-no NO-ERROR.
        FIND FIRST style NO-LOCK 
            WHERE style.company EQ bItemFG.company
              AND style.style   EQ eb.style NO-ERROR. 
        ASSIGN
            cStyle = IF AVAILABLE style THEN style.dscr ELSE eb.style
            dLen = eb.len
            dWid = eb.wid
            dDep = eb.dep
            dBlen = eb.t-len
            dBwid = eb.t-wid
            cCad = eb.cad-no
            cDie = eb.die-no
            cPlate = eb.plate-no
            cTrNno = eb.tr-no
            cCasNo = eb.cas-no
            cBoard = IF AVAILABLE ef THEN ef.board ELSE ""
            cBoard2 = IF AVAILABLE ef THEN ef.brd-dscr ELSE ""
            cCaseCount = eb.cas-cnt
            cPalletCount = eb.tr-cnt
            cCasesPerPallet = eb.cas-pal
            .
        FIND FIRST item NO-LOCK 
            WHERE item.company EQ eb.company
              AND item.i-no EQ eb.tr-no NO-ERROR.

        IF AVAILABLE item THEN cTrName = item.i-name.
        FIND FIRST item NO-LOCK 
            WHERE item.company EQ eb.company
              AND item.i-no    EQ eb.cas-no NO-ERROR.
        IF AVAILABLE item THEN cCasName = item.i-name.
        FIND FIRST item NO-LOCK 
            WHERE item.company EQ eb.company
              AND item.i-no    EQ cBoard NO-ERROR.
        IF AVAILABLE item THEN dClip = item.cal. 
        
        ASSIGN
            iInkCnt  = 1
            iCoatCnt = 1
            . 
        DO i = 1 TO 10:
            IF eb.est-type LE 4 THEN DO:
                IF eb.i-code2[i] NE "" THEN DO:
                    FIND FIRST item NO-LOCK 
                        WHERE item.company EQ ipcCompany
                          AND item.i-no EQ eb.i-code2[i] NO-ERROR.
                    IF AVAILABLE item AND item.mat-type EQ "V" THEN 
                        ASSIGN cCoat[iCoatCnt] = eb.i-code2[i]
                        iCoatCnt = iCoatCnt + 1.
                    ELSE ASSIGN cInk[iInkCnt] = eb.i-code2[i]
                        iInkCnt = iInkCnt + 1.
                END.
            END.
            ELSE DO:
                IF eb.i-code[i] NE "" THEN DO:
                    FIND FIRST item NO-LOCK 
                        WHERE item.company EQ ipcCompany
                          AND item.i-no EQ eb.i-code[i] NO-ERROR.
                    IF AVAIL item AND item.mat-type EQ "V" THEN 
                        ASSIGN cCoat[iCoatCnt] = eb.i-code[i]
                        iCoatCnt = iCoatCnt + 1.
                    ELSE ASSIGN cInk[iInkCnt] = eb.i-code[i]
                        iInkCnt = iInkCnt + 1.
                END.
            END.
        END.
    END.
    
    CASE bItemFG.ship-meth :
        WHEN YES THEN
            cShipMethod = "Case".
        WHEN NO THEN
            cShipMethod = "Pallet".
    END CASE.

    CASE bItemFG.alloc :
        WHEN YES THEN
            dFuncAlloc = "Unassembled".
        WHEN NO THEN
            dFuncAlloc = "Assembled".
        OTHERWISE
            dFuncAlloc = "Assembled w/Part Receipts".
    END CASE.
    
    CREATE ttFinishedGoodsExport.
    ASSIGN 
        ttFinishedGoodsExport.itemNo            = bItemFG.i-no                  
        ttFinishedGoodsExport.itemName          = bItemFG.i-name                
        ttFinishedGoodsExport.custPartNo        = bItemFG.part-no                             
        ttFinishedGoodsExport.custNo            = bItemFG.cust-no                             
        ttFinishedGoodsExport.custName          = bItemFG.cust-name                           
        ttFinishedGoodsExport.estimate          = bItemFG.est-no                              
        ttFinishedGoodsExport.style             = bItemFG.style                               
        ttFinishedGoodsExport.category          = bItemFG.procat                              
        ttFinishedGoodsExport.categoryDescr     = bItemFG.procat-desc         
        ttFinishedGoodsExport.categoryDescr1    = bItemFG.part-dscr1          
        ttFinishedGoodsExport.categoryDescr2    = bItemFG.part-dscr2                          
        ttFinishedGoodsExport.categoryDescr3    = bItemFG.part-dscr3                          
        ttFinishedGoodsExport.stockCust         = bItemFG.i-code                              
        ttFinishedGoodsExport.die               = bItemFG.die-no                              
        ttFinishedGoodsExport.plate             = bItemFG.plate-no                            
        ttFinishedGoodsExport.upc               = bItemFG.upc-no                              
        ttFinishedGoodsExport.cad               = bItemFG.cad-no 
        ttFinishedGoodsExport.qualitySpc        = bItemFG.spc-no                              
        ttFinishedGoodsExport.stocked           = bItemFG.stocked                             
        ttFinishedGoodsExport.setHeader         = bItemFG.isaset                              
        ttFinishedGoodsExport.aGROUP            = bItemFG.spare-char-1                        
        ttFinishedGoodsExport.exemptFromDisc    = bItemFG.exempt-disc                         
        ttFinishedGoodsExport.pm                = bItemFG.pur-man                             
        ttFinishedGoodsExport.sellPrice         = bItemFG.sell-price                          
        ttFinishedGoodsExport.sellPriceUom      = bItemFG.sell-uom                            
        ttFinishedGoodsExport.typeCode          = bItemFG.type-code                           
        ttFinishedGoodsExport.currency          = bItemFG.curr-code[1]
        ttFinishedGoodsExport.warehouse         = bItemFG.def-loc                             
        ttFinishedGoodsExport.bin               = bItemFG.def-loc-bin                         
        ttFinishedGoodsExport.inventoryClass    = bItemFG.class                               
        ttFinishedGoodsExport.cycleCountCode    = bItemFG.cc-code                             
        ttFinishedGoodsExport.prodcode          = bItemFG.prod-code                           
        ttFinishedGoodsExport.aCOUNT            = bItemFG.case-count 
        ttFinishedGoodsExport.weight            = bItemFG.weight-100                          
        ttFinishedGoodsExport.freezeWeight      = bItemFG.spare-int-1                         
        ttFinishedGoodsExport.pknote            = bItemFG.prod-notes                          
        ttFinishedGoodsExport.freightClass      = bItemFG.frt-class                           
        ttFinishedGoodsExport.freightClassDesc  = bItemFG.frt-class-dscr                      
        ttFinishedGoodsExport.stdMaterialCost   = bItemFG.std-mat-cost                        
        ttFinishedGoodsExport.stdLaborCost      = bItemFG.std-lab-cost                        
        ttFinishedGoodsExport.stdVarOHCost      = bItemFG.std-var-cost                        
        ttFinishedGoodsExport.stdFixOHCost      = bItemFG.std-fix-cost                        
        ttFinishedGoodsExport.totalStdCost      = bItemFG.total-std-cost      
        ttFinishedGoodsExport.averageCost       = bItemFG.avg-cost
        ttFinishedGoodsExport.lastCost          = bItemFG.last-cost 
        ttFinishedGoodsExport.costUOM           = bItemFG.prod-uom            
        ttFinishedGoodsExport.fullCost          = bItemFG.spare-dec-1         
        ttFinishedGoodsExport.varied            = bItemFG.spare-char-2        
        ttFinishedGoodsExport.taxable           = bItemFG.taxable             
        ttFinishedGoodsExport.astatus           = bItemFG.stat                
        ttFinishedGoodsExport.shipMeth          = cShipMethod
        ttFinishedGoodsExport.vendor1           = bItemFG.vend-no             
        ttFinishedGoodsExport.vendor1Item       = bItemFG.vend-item           
        ttFinishedGoodsExport.vendor2           = bItemFG.vend2-no
        ttFinishedGoodsExport.vendor2Item       = bItemFG.vend2-item 
        ttFinishedGoodsExport.stocked2          = bItemFG.stocked
        ttFinishedGoodsExport.setAllocation     = dFuncAlloc  
        ttFinishedGoodsExport.reorderPolicy     = bItemFG.ord-policy          
        ttFinishedGoodsExport.reorderLevel      = bItemFG.ord-level           
        ttFinishedGoodsExport.minOrder          = bItemFG.ord-min             
        ttFinishedGoodsExport.maxOrder          = bItemFG.ord-max             
        ttFinishedGoodsExport.purchasedQtyUOM   = bItemFG.pur-uom             
        ttFinishedGoodsExport.leadTimeDays      = bItemFG.lead-days           
        ttFinishedGoodsExport.begDate           = bItemFG.beg-date            
        ttFinishedGoodsExport.begBalance        = bItemFG.beg-bal             
        ttFinishedGoodsExport.qtyOnHand         = bItemFG.q-onh               
        ttFinishedGoodsExport.qtyOnOrd          = bItemFG.q-ono               
        ttFinishedGoodsExport.qtyAllocated      = bItemFG.q-alloc             
        ttFinishedGoodsExport.qtyBackordered    = bItemFG.q-back              
        ttFinishedGoodsExport.qtyAvailable      = bItemFG.q-avail
        ttFinishedGoodsExport.qtyOrderedPTD     = bItemFG.q-ptd               
        ttFinishedGoodsExport.qtyOrderedYTD     = bItemFG.q-ord-ytd 
        ttFinishedGoodsExport.qtyOrderedLastYr  = bItemFG.u-ord 
        ttFinishedGoodsExport.qtyProducedPTD    = bItemFG.q-prod-ptd          
        ttFinishedGoodsExport.qtyProducedYTD    = bItemFG.q-prod-ytd          
        ttFinishedGoodsExport.qtyProducedLastYr = bItemFG.u-prod  
        ttFinishedGoodsExport.qtyShippedPTD     = bItemFG.q-ship-ptd          
        ttFinishedGoodsExport.qtyShippedYTD     = bItemFG.q-ship-ytd          
        ttFinishedGoodsExport.qtyShippedLastYr  = bItemFG.u-ship 
        ttFinishedGoodsExport.qtyInvoicedPTD    = bItemFG.q-inv-ptd           
        ttFinishedGoodsExport.qtyInvoicedYTD    = bItemFG.q-inv-ytd
        ttFinishedGoodsExport.qtyInvoicedLastYr = bItemFG.u-inv
        ttFinishedGoodsExport.totalMsfYTD       = bItemFG.ytd-msf   
        ttFinishedGoodsExport.totalMsfLastYr    = bItemFG.lyytd-msf
        ttFinishedGoodsExport.boxLength         = bItemFG.l-score[50]           
        ttFinishedGoodsExport.boxWidth          = bItemFG.w-score[50]         
        ttFinishedGoodsExport.boxDepth          = bItemFG.d-score[50]         
        ttFinishedGoodsExport.blankLength       = bItemFG.t-len        
        ttFinishedGoodsExport.blankWidth        = bItemFG.t-wid              
        ttFinishedGoodsExport.totalSqIn         = bItemFG.t-sqin               
        ttFinishedGoodsExport.totalSqFt         = bItemFG.t-sqft 
        ttFinishedGoodsExport.aColor1           = cInk[1]
        ttFinishedGoodsExport.aColor2           = cInk[2]
        ttFinishedGoodsExport.aColor3           = cInk[3]
        ttFinishedGoodsExport.aColor4           = cInk[4]
        ttFinishedGoodsExport.aColor5           = cInk[5]
        ttFinishedGoodsExport.aColor6           = cInk[6]
        ttFinishedGoodsExport.aColor7           = cInk[7]
        ttFinishedGoodsExport.aColor8           = cInk[8]
        ttFinishedGoodsExport.aColor9           = cInk[9]
        ttFinishedGoodsExport.aColor10          = cInk[10]
        ttFinishedGoodsExport.aCoating1         = cCoat[1]              
        ttFinishedGoodsExport.aCoating2         = cCoat[2]  
        ttFinishedGoodsExport.aCoating3         = cCoat[3] 
        ttFinishedGoodsExport.boardCode         = cBoard              
        ttFinishedGoodsExport.boardName         = cBoard2             
        ttFinishedGoodsExport.caliper           = dClip                
        ttFinishedGoodsExport.caseCode          = cCasNo              
        ttFinishedGoodsExport.caseName          = cCasName             
        ttFinishedGoodsExport.caseQty           = cCaseCount              
        ttFinishedGoodsExport.skidCode          = cTrNno            
        ttFinishedGoodsExport.skidName          = cTrName            
        ttFinishedGoodsExport.skidQty           = cPalletCount            
        ttFinishedGoodsExport.specCode1         = cSpecNote
        .
        IF iPeriod GT 0 THEN
        ttFinishedGoodsExport.totalMsfPTD       = (bItemFG.ptd-msf[iPeriod]).
END.  /* FOR EACH bItemFG */     
                                   
{aoa/BL/pBuildCustList.i}
