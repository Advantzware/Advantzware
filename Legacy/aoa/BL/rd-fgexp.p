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
/* set parameter values here if not running from parameter screen */

/* local variables */

DEFINE BUFFER b-itemfg FOR itemfg.

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
DEFINE VARIABLE dClip           LIKE ITEM.cal                         NO-UNDO.
DEFINE VARIABLE cTrNno          AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cTrName         AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cCasNo          AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cCasName        AS   CHARACTER INIT ""                NO-UNDO.
DEFINE VARIABLE cSpecNote       AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE i               AS   INTEGER                          NO-UNDO.
DEFINE VARIABLE dfuncAlloc      AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE cShipMethod     AS   CHARACTER                        NO-UNDO.
DEFINE VARIABLE iPeriod         AS   INTEGER                          NO-UNDO.

 FIND FIRST period WHERE period.company EQ ipcCompany 
     AND period.pstat EQ TRUE   
     AND period.pst LE today
     AND period.pend GE today NO-LOCK NO-ERROR.
 IF NOT AVAILABLE period THEN
     FIND LAST period WHERE period.company EQ ipcCompany  AND
     period.pstat EQ TRUE NO-LOCK NO-ERROR.
 IF AVAILABLE period THEN
     iPeriod = period.pnum.


/* subject business logic */
FOR EACH b-itemfg NO-LOCK 
    WHERE b-itemfg.company EQ ipcCompany
      AND b-itemfg.i-no GE cStartItemNo
      AND b-itemfg.i-no LE cEndItemNo
      AND b-itemfg.i-name GE cStartItemName
      AND b-itemfg.i-name LE cEndItemName
      AND b-itemfg.part-no GE cStartCustPart
      AND b-itemfg.part-no LE cEndCustPart
      AND b-itemfg.cust-no GE cStartCustNo
      AND b-itemfg.cust-no LE cEndCustNo
      AND b-itemfg.est-no GE cStartEstimate
      AND b-itemfg.est-no LE cEndEstimate
      AND b-itemfg.style GE cStartStyle
      AND b-itemfg.style LE cEndStyle
      AND b-itemfg.procat GE cStartProdCategory
      AND b-itemfg.procat LE cEndProdCategory
      AND ( ((b-itemfg.stat = "A" OR b-itemfg.stat = "" ) AND lActive ) 
            OR (b-itemfg.stat = "I" AND lInactive ) )
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
        dClip  =  0.

    cSpecNote = "".
    
    IF lSpecNote THEN do:
        FOR EACH notes NO-LOCK 
            WHERE notes.rec_key EQ b-itemfg.rec_key
             /* AND CAN-DO(v-dept,notes.note_code) */ :
            IF AVAILABLE notes THEN cSpecNote = cSpecNote + " " + notes.note_text.
        END.
    END.

    FIND FIRST est NO-LOCK
         WHERE est.company EQ ipcCompany
           AND est.est-no  EQ b-itemfg.est-no NO-ERROR.
    
    IF b-itemfg.ship-meth THEN
        ASSIGN cCaseCount = b-itemfg.case-count
        cPalletCount = 0.
    ELSE
        ASSIGN cCaseCount = 0
            cPalletCount = b-itemfg.case-count.
    
    FIND FIRST eb NO-LOCK 
        WHERE eb.company EQ ipcCompany
          AND eb.est-no EQ b-itemfg.est-no 
          AND eb.stock-no EQ b-itemfg.i-no NO-ERROR.
    
    IF AVAILABLE eb THEN DO:
        FIND FIRST ef NO-LOCK
             WHERE ef.company EQ ipcCompany
               AND ef.est-no  EQ b-itemfg.est-no 
               AND ef.form-no EQ eb.form-no NO-ERROR.
        FIND FIRST style NO-LOCK 
            WHERE style.company EQ b-itemfg.company
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

        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ eb.company
              AND ITEM.i-no EQ eb.tr-no NO-ERROR.

        IF AVAILABLE ITEM THEN cTrName = ITEM.i-NAME.
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ eb.company
              AND ITEM.i-no EQ eb.cas-no NO-ERROR.
        IF AVAILABLE ITEM THEN cCasName = ITEM.i-NAME.
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ eb.company
              AND ITEM.i-no EQ cBoard NO-ERROR.
        IF AVAILABLE ITEM THEN dClip = ITEM.cal . 
        
        ASSIGN iInkCnt = 1
            iCoatCnt = 1. 
        DO i = 1 TO 10:
            IF eb.est-type LE 4 THEN DO:
                IF eb.i-code2[i] NE "" THEN DO:
                    FIND FIRST ITEM NO-LOCK 
                        WHERE ITEM.company EQ ipcCompany
                          AND ITEM.i-no EQ eb.i-code2[i] NO-ERROR.
                    IF AVAILABLE ITEM AND ITEM.mat-type EQ "V" THEN 
                        ASSIGN cCoat[iCoatCnt] = eb.i-code2[i]
                        iCoatCnt = iCoatCnt + 1.
                    ELSE ASSIGN cInk[iInkCnt] = eb.i-code2[i]
                        iInkCnt = iInkCnt + 1.
                END.
            END.
            else DO:
                IF eb.i-code[i] NE "" THEN DO:
                    FIND FIRST ITEM NO-LOCK 
                        WHERE ITEM.company EQ ipcCompany
                          AND ITEM.i-no EQ eb.i-code[i] NO-ERROR.
                    IF AVAIL ITEM AND ITEM.mat-type EQ "V" THEN 
                        ASSIGN cCoat[iCoatCnt] = eb.i-code[i]
                        iCoatCnt = iCoatCnt + 1.
                    ELSE ASSIGN cInk[iInkCnt] = eb.i-code[i]
                        iInkCnt = iInkCnt + 1.
                END.
            END.
        END.
    END.
    
    CASE b-itemfg.ship-meth :
        WHEN YES THEN
            cShipMethod = "Case".
        WHEN NO THEN
            cShipMethod = "Pallet".
    END CASE.
    

    CASE b-itemfg.alloc :
        WHEN YES THEN
            dfuncAlloc = "Unassembled".
        WHEN NO THEN
            dfuncAlloc = "Assembled".
        OTHERWISE
            dfuncAlloc = "Assembled w/Part Receipts".
    END CASE.
    
    CREATE ttFinishedGoodsExport.
    ASSIGN 
        ttFinishedGoodsExport.itemNo            = b-itemfg.i-no                  
        ttFinishedGoodsExport.itemName          = b-itemfg.i-name                
        ttFinishedGoodsExport.custPartNo        = b-itemfg.part-no                             
        ttFinishedGoodsExport.custNo            = b-itemfg.cust-no                             
        ttFinishedGoodsExport.custName          = b-itemfg.cust-name                           
        ttFinishedGoodsExport.estimate          = b-itemfg.est-no                              
        ttFinishedGoodsExport.style             = b-itemfg.style                               
        ttFinishedGoodsExport.category          = b-itemfg.procat                              
        ttFinishedGoodsExport.categoryDescr     = b-itemfg.procat-desc         
        ttFinishedGoodsExport.categoryDescr1    = b-itemfg.part-dscr1          
        ttFinishedGoodsExport.categoryDescr2    = b-itemfg.part-dscr2                          
        ttFinishedGoodsExport.categoryDescr3    = b-itemfg.part-dscr3                          
        ttFinishedGoodsExport.stockCust         = b-itemfg.i-code                              
        ttFinishedGoodsExport.die               = b-itemfg.die-no                              
        ttFinishedGoodsExport.plate             = b-itemfg.plate-no                            
        ttFinishedGoodsExport.upc               = b-itemfg.upc-no                              
        ttFinishedGoodsExport.cad               = b-itemfg.cad-no 
        ttFinishedGoodsExport.qualitySpc        = b-itemfg.spc-no                              
        ttFinishedGoodsExport.stocked           = b-itemfg.stocked                             
        ttFinishedGoodsExport.setHeader         = b-itemfg.isaset                              
        ttFinishedGoodsExport.aGROUP            = b-itemfg.spare-char-1                        
        ttFinishedGoodsExport.exemptFromDisc    = b-itemfg.exempt-disc                         
        ttFinishedGoodsExport.pm                = b-itemfg.pur-man                             
        ttFinishedGoodsExport.sellPrice         = b-itemfg.sell-price                          
        ttFinishedGoodsExport.sellPriceUom      = b-itemfg.sell-uom                            
        ttFinishedGoodsExport.typeCode          = b-itemfg.type-code                           
        ttFinishedGoodsExport.currency          = b-itemfg.curr-code[1]
        ttFinishedGoodsExport.warehouse         = b-itemfg.def-loc                             
        ttFinishedGoodsExport.bin               = b-itemfg.def-loc-bin                         
        ttFinishedGoodsExport.inventoryClass    = b-itemfg.class                               
        ttFinishedGoodsExport.cycleCountCode    = b-itemfg.cc-code                             
        ttFinishedGoodsExport.prodcode          = b-itemfg.prod-code                           
        ttFinishedGoodsExport.aCOUNT            = b-itemfg.case-count 
        ttFinishedGoodsExport.weight            = b-itemfg.weight-100                          
        ttFinishedGoodsExport.freezeWeight      = b-itemfg.spare-int-1                         
        ttFinishedGoodsExport.pknote            = b-itemfg.prod-notes                          
        ttFinishedGoodsExport.freightClass      = b-itemfg.frt-class                           
        ttFinishedGoodsExport.freightClassDesc  = b-itemfg.frt-class-dscr                      
        ttFinishedGoodsExport.stdMaterialCost    = b-itemfg.std-mat-cost                        
        ttFinishedGoodsExport.stdLaborCost      = b-itemfg.std-lab-cost                        
        ttFinishedGoodsExport.stdVarOHCost      = b-itemfg.std-var-cost                        
        ttFinishedGoodsExport.stdFixOHCost      = b-itemfg.std-fix-cost                        
        ttFinishedGoodsExport.totalStdCost      = b-itemfg.total-std-cost      
        ttFinishedGoodsExport.averageCost       = b-itemfg.avg-cost
        ttFinishedGoodsExport.lastCost          = b-itemfg.last-cost 
        ttFinishedGoodsExport.costUOM           = b-itemfg.prod-uom            
        ttFinishedGoodsExport.fullCost          = b-itemfg.spare-dec-1         
        ttFinishedGoodsExport.varied            = b-itemfg.spare-char-2        
        ttFinishedGoodsExport.taxable           = b-itemfg.taxable             
        ttFinishedGoodsExport.astatus           = b-itemfg.stat                
        ttFinishedGoodsExport.shipMeth          = cShipMethod
        ttFinishedGoodsExport.vendor1           = b-itemfg.vend-no             
        ttFinishedGoodsExport.vendor1Item       = b-itemfg.vend-item           
        ttFinishedGoodsExport.vendor2           = b-itemfg.vend2-no
        ttFinishedGoodsExport.vendor2Item       = b-itemfg.vend2-item 
        ttFinishedGoodsExport.stocked2          = b-itemfg.stocked
        ttFinishedGoodsExport.setAllocation     = dfuncAlloc  
        ttFinishedGoodsExport.reorderPolicy     = b-itemfg.ord-policy          
        ttFinishedGoodsExport.reorderLevel      = b-itemfg.ord-level           
        ttFinishedGoodsExport.minOrder          = b-itemfg.ord-min             
        ttFinishedGoodsExport.maxOrder          = b-itemfg.ord-max             
        ttFinishedGoodsExport.purchasedQtyUOM   = b-itemfg.pur-uom             
        ttFinishedGoodsExport.leadTimeDays      = b-itemfg.lead-days           
        ttFinishedGoodsExport.begDate           = b-itemfg.beg-date            
        ttFinishedGoodsExport.begBalance        = b-itemfg.beg-bal             
        ttFinishedGoodsExport.qtyOnHand         = b-itemfg.q-onh               
        ttFinishedGoodsExport.qtyOnOrd          = b-itemfg.q-ono               
        ttFinishedGoodsExport.qtyAllocated      = b-itemfg.q-alloc             
        ttFinishedGoodsExport.qtyBackordered    = b-itemfg.q-back              
        ttFinishedGoodsExport.qtyAvailable      = b-itemfg.q-avail
        ttFinishedGoodsExport.qtyOrderedPTD     = b-itemfg.q-ptd               
        ttFinishedGoodsExport.qtyOrderedYTD     = b-itemfg.q-ord-ytd 
        ttFinishedGoodsExport.qtyOrderedLastYr  = b-itemfg.u-ord 
        ttFinishedGoodsExport.qtyProducedPTD    = b-itemfg.q-prod-ptd          
        ttFinishedGoodsExport.qtyProducedYTD    = b-itemfg.q-prod-ytd          
        ttFinishedGoodsExport.qtyProducedLastYr = b-itemfg.u-prod  
        ttFinishedGoodsExport.qtyShippedPTD     = b-itemfg.q-ship-ptd          
        ttFinishedGoodsExport.qtyShippedYTD     = b-itemfg.q-ship-ytd          
        ttFinishedGoodsExport.qtyShippedLastYr  = b-itemfg.u-ship 
        ttFinishedGoodsExport.qtyInvoicedPTD    = b-itemfg.q-inv-ptd           
        ttFinishedGoodsExport.qtyInvoicedYTD    = b-itemfg.q-inv-ytd
        ttFinishedGoodsExport.qtyInvoicedLastYr = b-itemfg.u-inv
        ttFinishedGoodsExport.totalMsfYTD       = b-itemfg.ytd-msf   
        ttFinishedGoodsExport.totalMsfLastYr    = b-itemfg.lyytd-msf
        ttFinishedGoodsExport.boxLength         = b-itemfg.l-score[50]           
        ttFinishedGoodsExport.boxWidth          = b-itemfg.w-score[50]         
        ttFinishedGoodsExport.boxDepth          = b-itemfg.d-score[50]         
        ttFinishedGoodsExport.blankLength       = b-itemfg.t-len        
        ttFinishedGoodsExport.blankWidth        = b-itemfg.t-wid              
        ttFinishedGoodsExport.totalSqIn         = b-itemfg.t-sqin               
        ttFinishedGoodsExport.totalSqFt         = b-itemfg.t-sqft 
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
        ttFinishedGoodsExport.specCode1         = cSpecNote .
        
        if iPeriod gt 0 then
        assign
        ttFinishedGoodsExport.totalMsfPTD       = (b-itemfg.ptd-msf[iPeriod])  .
        
END.  /* FOR EACH b-itemfg */     
                                   
{aoa/BL/pBuildCustList.i}
 
                              
                                    
/**************** use "aoa/BL/blankLength      template.p" as an example ***************/
                                  
