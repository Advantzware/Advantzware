
/*------------------------------------------------------------------------
    File        : ChangeLocationProc.p
    Purpose     : 
    Syntax      : 

    Description : 	

    Author(s)   : Sewa Singh
    Created     : Mon Nov 22 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcOldLoc   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcNewLoc   AS CHARACTER NO-UNDO.

DEFINE BUFFER b-loc FOR loc.
DEFINE BUFFER b-location FOR location.

/* ********************  Preprocessor Definitions  ******************** */
        
/* ***************************  Main Block  *************************** */
     
RUN spProgressBar ("Change Location", 1, 100).

FIND FIRST loc NO-LOCK
    WHERE loc.company EQ ipcCompany
    AND loc.loc EQ ipcOldLoc 
    NO-ERROR.
     
FIND FIRST location NO-LOCK
    WHERE location.locationCode EQ loc.loc
    AND location.rec_key EQ loc.addrRecKey 
    AND location.company EQ loc.company
    NO-ERROR. 

FOR EACH ap-inv FIELD(loc)
    WHERE ap-inv.company EQ ipcCompany
    AND ap-inv.loc EQ ipcOldLoc    
    TRANSACTION:   
  
    FOR EACH ap-invl FIELD(loc) EXCLUSIVE-LOCK
        WHERE ap-invl.i-no EQ ap-inv.i-no: 
        ap-invl.loc = ipcNewLoc.
    END.

    ap-inv.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 2, 100).

FOR EACH APIOutboundEvent FIELD(locationID)
    WHERE APIOutboundEvent.company EQ ipcCompany
    AND APIOutboundEvent.locationID EQ ipcOldLoc    
    TRANSACTION: 

    APIOutboundEvent.locationID = ipcNewLoc.
END.  

RUN spProgressBar ("Change Location", 3, 100).

FOR EACH ar-inv FIELD(loc)
    WHERE ar-inv.company EQ ipcCompany
    AND ar-inv.loc EQ ipcOldLoc
    USE-INDEX ar-inv
    TRANSACTION:   
  
    FOR EACH ar-invl FIELD(loc) EXCLUSIVE-LOCK
        WHERE ar-invl.x-no EQ ar-inv.x-no USE-INDEX x-no:
        ar-invl.loc = ipcNewLoc.
    END.

    ar-inv.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 4, 100).

FOR EACH carr-mtx FIELD(loc)
    WHERE carr-mtx.company EQ ipcCompany
    AND carr-mtx.loc EQ ipcOldLoc    
    TRANSACTION:
    
    carr-mtx.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 5, 100). 

FOR EACH carrier FIELD(loc)
    WHERE carrier.company EQ ipcCompany
    AND carrier.loc EQ ipcOldLoc    
    USE-INDEX loc
    TRANSACTION:
    
    carrier.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 6, 100).

FOR EACH ce-ctrl FIELD(loc)
    WHERE ce-ctrl.company EQ ipcCompany
    AND ce-ctrl.loc EQ ipcOldLoc    
    TRANSACTION:
    
    ce-ctrl.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 7, 100).

FOR EACH costtype FIELD(loc)
    WHERE costtype.company EQ ipcCompany
    AND costtype.loc EQ ipcOldLoc    
    TRANSACTION:
    
    costtype.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 8, 100).

FOR EACH cust FIELD(loc def-loc)
    WHERE cust.company EQ ipcCompany
    AND (cust.loc EQ ipcOldLoc OR cust.def-loc EQ ipcOldLoc)
    TRANSACTION:
    IF cust.loc EQ ipcOldLoc THEN
        cust.loc = ipcNewLoc.
    IF cust.def-loc EQ ipcOldLoc THEN
        cust.def-loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 9, 100).

FOR EACH cust-itm  FIELD(loc)
    WHERE cust-itm.company EQ ipcCompany
    AND cust-itm.loc EQ ipcOldLoc
    USE-INDEX cust
    TRANSACTION:
    
    cust-itm.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 10, 100).

FOR EACH e-item FIELD(loc)
    WHERE e-item.company EQ ipcCompany
    AND e-item.loc EQ ipcOldLoc    
    TRANSACTION:
    
    e-item.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 11, 100).

FOR EACH est  FIELD(loc)
    WHERE est.company EQ ipcCompany
    AND est.loc EQ ipcOldLoc 
    USE-INDEX est-no
    TRANSACTION:
    
    est.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 12, 100).

FOR EACH eb FIELD(loc)
    WHERE eb.company EQ ipcCompany
    AND eb.loc EQ ipcOldLoc       
    TRANSACTION:
    
    eb.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 13, 100).

FOR EACH ef  FIELD(loc)
    WHERE ef.company EQ ipcCompany
    AND ef.loc EQ ipcOldLoc       
    TRANSACTION:
    
    ef.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 14, 100).

FOR EACH est-inst FIELD(loc)
    WHERE est-inst.company EQ ipcCompany
    AND est-inst.loc EQ ipcOldLoc       
    TRANSACTION:
    
    est-inst.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 15, 100).

FOR EACH fg-act FIELD(loc)
    WHERE fg-act.company EQ ipcCompany
    AND fg-act.loc EQ ipcOldLoc       
    TRANSACTION:
    
    fg-act.loc = ipcNewLoc.
END.

FOR EACH fg-bin  FIELD(loc)
    WHERE fg-bin.company EQ ipcCompany
    AND fg-bin.loc EQ ipcOldLoc       
    TRANSACTION:
    
    fg-bin.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 16, 100).

FOR EACH fg-hist  FIELD(loc)
    WHERE fg-hist.company EQ ipcCompany
    AND fg-hist.loc EQ ipcOldLoc       
    TRANSACTION:
    
    fg-hist.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 17, 100).

FOR EACH fg-rcpth  FIELD(loc)
    WHERE fg-rcpth.company EQ ipcCompany
    AND fg-rcpth.loc EQ ipcOldLoc       
    TRANSACTION:
    
    fg-rcpth.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 18, 100).

FOR EACH fg-rcpts FIELD(loc)
    WHERE fg-rcpts.company EQ ipcCompany
    AND fg-rcpts.loc EQ ipcOldLoc       
    TRANSACTION:
    
    fg-rcpts.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 19, 100).

FOR EACH fg-rctd FIELD(loc loc2)
    WHERE fg-rctd.company EQ ipcCompany
    AND (fg-rctd.loc EQ ipcOldLoc OR fg-rctd.loc2 EQ ipcOldLoc)
    TRANSACTION:
    IF fg-rctd.loc EQ ipcOldLoc THEN 
        fg-rctd.loc = ipcNewLoc.
    
    IF fg-rctd.loc2 EQ ipcOldLoc THEN 
        fg-rctd.loc2 = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 20, 100).

FOR EACH fg-rdtlh  FIELD(loc loc2)
    WHERE fg-rdtlh.company EQ ipcCompany
    AND (fg-rdtlh.loc EQ ipcOldLoc OR fg-rdtlh.loc2 EQ ipcOldLoc)
    TRANSACTION:
    IF fg-rdtlh.loc EQ ipcOldLoc THEN 
        fg-rdtlh.loc = ipcNewLoc.
    
    IF fg-rdtlh.loc2 EQ ipcOldLoc THEN 
        fg-rdtlh.loc2 = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 21, 100).

FOR EACH flute  FIELD(loc)
    WHERE flute.company EQ ipcCompany
    AND flute.loc EQ ipcOldLoc       
    TRANSACTION:
    
    flute.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 22, 100).

FOR EACH inventorySnapshot FIELD(warehouseID)
    WHERE inventorySnapshot.company EQ ipcCompany
    AND inventorySnapshot.warehouseID EQ ipcOldLoc       
    TRANSACTION:
    
    inventorySnapshot.warehouseID = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 23, 100).

FOR EACH inventoryStock  FIELD(warehouseID)
    WHERE inventoryStock.company EQ ipcCompany
    AND inventoryStock.warehouseID EQ ipcOldLoc       
    TRANSACTION:
    
    inventoryStock.warehouseID = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 24, 100).

FOR EACH inventoryStockSnapshot  FIELD(warehouseID)
    WHERE inventoryStockSnapshot.company EQ ipcCompany
    AND inventoryStockSnapshot.warehouseID EQ ipcOldLoc       
    TRANSACTION:
    
    inventoryStockSnapshot.warehouseID = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 25, 100).

FOR EACH inventoryTransaction FIELD(warehouseID)
    WHERE inventoryTransaction.company EQ ipcCompany
    AND inventoryTransaction.warehouseID EQ ipcOldLoc       
    TRANSACTION:
    
    inventoryTransaction.warehouseID = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 26, 100).

FOR EACH ITEM FIELD(loc)
    WHERE ITEM.company EQ ipcCompany
    AND ITEM.loc EQ ipcOldLoc       
    TRANSACTION:
    
    ITEM.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 27, 100).

FOR EACH itemfg FIELD(loc)
    WHERE itemfg.company EQ ipcCompany
    AND (itemfg.loc EQ ipcOldLoc OR itemfg.def-loc EQ ipcOldLoc)
    TRANSACTION:
    IF itemfg.loc EQ ipcOldLoc THEN 
        itemfg.loc = ipcNewLoc.
    
    IF itemfg.def-loc EQ ipcOldLoc THEN 
        itemfg.def-loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 28, 100).

FOR EACH itemfg-loc  FIELD(loc)
    WHERE itemfg-loc.company EQ ipcCompany
    AND itemfg-loc.loc EQ ipcOldLoc       
    TRANSACTION:
    
    itemfg-loc.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 29, 100).

FOR EACH itemfgdtl FIELD(loc)
    WHERE itemfgdtl.company EQ ipcCompany
    AND itemfgdtl.loc EQ ipcOldLoc       
    TRANSACTION:
    
    itemfgdtl.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 30, 100).

FOR EACH job
    WHERE job.company EQ ipcCompany
    AND (job.loc EQ ipcOldLoc OR job.shipFromLocation EQ ipcOldLoc)
    TRANSACTION:
    
    FOR EACH job-hdr EXCLUSIVE-LOCK
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job EQ job.job
        AND job-hdr.job-no EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        AND job-hdr.loc EQ ipcOldLoc: 
        job-hdr.loc = ipcNewLoc.
    END.
    
    IF job.loc EQ ipcOldLoc THEN 
        job.loc = ipcNewLoc.
    
    IF job.shipFromLocation EQ ipcOldLoc THEN 
        job.shipFromLocation = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 31, 100).

FOR EACH job-farm-rctd  FIELD(loc)
    WHERE job-farm-rctd.company EQ ipcCompany
    AND job-farm-rctd.loc EQ ipcOldLoc       
    TRANSACTION:
    
    job-farm-rctd.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 32, 100).

FOR EACH loadtag  FIELD(loc)
    WHERE loadtag.company EQ ipcCompany
    AND loadtag.loc EQ ipcOldLoc       
    TRANSACTION:
    
    loadtag.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 33, 100).

FOR EACH mach FIELD(loc physicalLoc)
    WHERE mach.company EQ ipcCompany
    AND (mach.loc EQ ipcOldLoc OR mach.physicalLoc EQ ipcOldLoc)
    TRANSACTION:
        
    IF mach.loc EQ ipcOldLoc THEN 
        mach.loc = ipcNewLoc.
    
    IF mach.physicalLoc EQ ipcOldLoc THEN 
        mach.physicalLoc = ipcNewLoc.
END.  

RUN spProgressBar ("Change Location", 34, 100).

FOR EACH mat-act FIELD (loc)
    WHERE mat-act.company EQ ipcCompany
    AND mat-act.loc EQ ipcOldLoc       
    TRANSACTION:
    
    mat-act.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 35, 100).

FOR EACH mmtx  FIELD (loc)
    WHERE mmtx.company EQ ipcCompany
    AND mmtx.loc EQ ipcOldLoc       
    TRANSACTION:
    
    mmtx.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 36, 100).

FOR EACH mmty FIELD (loc)
    WHERE mmty.company EQ ipcCompany
    AND mmty.loc EQ ipcOldLoc       
    TRANSACTION:
    
    mmty.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 37, 100).

FOR EACH mstd FIELD (loc)
    WHERE mstd.company EQ ipcCompany
    AND mstd.loc EQ ipcOldLoc       
    TRANSACTION:
    
    mstd.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 38, 100).

FOR EACH mstd FIELD (loc)
    WHERE mstd.company EQ ipcCompany
    AND mstd.loc EQ ipcOldLoc       
    TRANSACTION:
    
    mstd.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 38, 100).

FOR EACH oe-bolh
    WHERE oe-bolh.company EQ ipcCompany
    AND (oe-bolh.loc EQ ipcOldLoc OR oe-bolh.dock-loc EQ ipcOldLoc)
    TRANSACTION:
    
    FOR EACH oe-boll EXCLUSIVE-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.bol-no EQ oe-bolh.bol-no
        AND (oe-boll.loc EQ ipcOldLoc OR oe-boll.dock-loc EQ ipcOldLoc): 
        IF oe-boll.loc EQ ipcOldLoc THEN
            oe-boll.loc = ipcNewLoc.
        IF oe-boll.dock-loc EQ ipcOldLoc THEN
            oe-boll.dock-loc = ipcNewLoc.
    END.
    
    IF oe-bolh.loc EQ ipcOldLoc THEN 
        oe-bolh.loc = ipcNewLoc.
    
    IF oe-bolh.dock-loc EQ ipcOldLoc THEN 
        oe-bolh.dock-loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 40, 100).

FOR EACH oe-ctrl FIELD (loc)
    WHERE oe-ctrl.company EQ ipcCompany
    AND oe-ctrl.loc EQ ipcOldLoc       
    TRANSACTION:
    
    oe-ctrl.loc = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 41, 100).

FOR EACH oe-ord FIELD (loc)
    WHERE oe-ord.company EQ ipcCompany
    AND oe-ord.loc EQ ipcOldLoc
    TRANSACTION: 
    
    oe-ord.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 42, 100).

FOR EACH oe-rel FIELD (loc spare-char-1)
    WHERE oe-rel.company EQ ipcCompany
    AND (oe-rel.loc EQ ipcOldLoc OR oe-rel.spare-char-1 EQ ipcOldLoc)
    TRANSACTION:
        
    IF oe-rel.loc EQ ipcOldLoc THEN 
        oe-rel.loc = ipcNewLoc.
    
    IF oe-rel.spare-char-1 EQ ipcOldLoc THEN 
        oe-rel.spare-char-1 = ipcNewLoc.
END.

RUN spProgressBar ("Change Location", 43, 100).

FOR EACH oe-rell  FIELD(loc)
    WHERE oe-rell.company EQ ipcCompany
    AND oe-rell.loc EQ ipcOldLoc
    TRANSACTION: 
    
    oe-rell.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 44, 100).

FOR EACH oe-retl FIELD(loc)
    WHERE oe-retl.company EQ ipcCompany
    AND oe-retl.loc EQ ipcOldLoc
    TRANSACTION: 
    
    oe-rell.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 45, 100).

FOR EACH oe-ship FIELD(loc)
    WHERE oe-ship.company EQ ipcCompany
    AND oe-ship.loc EQ ipcOldLoc
    TRANSACTION: 
    
    oe-ship.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 46, 100).

FOR EACH palletSize  FIELD(location)
    WHERE palletSize.company EQ ipcCompany
    AND palletSize.location EQ ipcOldLoc
    TRANSACTION: 
    
    palletSize.location = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 47, 100).

FOR EACH pc-prdd-wip FIELD(loc)
    WHERE pc-prdd-wip.company EQ ipcCompany
    AND pc-prdd-wip.loc EQ ipcOldLoc
    TRANSACTION: 
    
    pc-prdd-wip.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 48, 100).

FOR EACH po-ord  FIELD(loc)
    WHERE po-ord.company EQ ipcCompany
    AND po-ord.loc EQ ipcOldLoc
    TRANSACTION: 
    
    po-ord.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 49, 100).

FOR EACH po-rcpts  FIELD(loc)
    WHERE po-rcpts.company EQ ipcCompany
    AND po-rcpts.loc EQ ipcOldLoc
    TRANSACTION: 
    
    po-rcpts.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 50, 100).

FOR EACH prep  FIELD(loc)
    WHERE prep.company EQ ipcCompany
    AND prep.loc EQ ipcOldLoc
    TRANSACTION: 
    
    prep.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 51, 100).

FOR EACH printer  FIELD(loc)
    WHERE printer.company EQ ipcCompany
    AND printer.loc EQ ipcOldLoc
    TRANSACTION: 
    
    printer.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 52, 100).

FOR EACH procat  FIELD(def-loc)
    WHERE procat.company EQ ipcCompany
    AND procat.def-loc EQ ipcOldLoc
    TRANSACTION: 
    
    procat.def-loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 53, 100).

FOR EACH quotehd
    WHERE quotehd.company EQ ipcCompany
    AND quotehd.loc EQ ipcOldLoc 
    TRANSACTION:
    
    FOR EACH quoteitm EXCLUSIVE-LOCK
        WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.q-no EQ quotehd.q-no
        AND quoteitm.loc EQ ipcOldLoc: 
        
        FOR EACH quoteqty EXCLUSIVE-LOCK
            WHERE quoteqty.company EQ quoteitm.company
            AND quoteqty.q-no EQ quoteitm.q-no
            AND quoteqty.LINE EQ quoteitm.line
            AND quoteqty.loc EQ ipcOldLoc: 
            
            FOR EACH quotechg EXCLUSIVE-LOCK
                WHERE quotechg.company EQ quoteqty.company 
                AND quotechg.loc EQ quoteqty.loc 
                AND quotechg.q-no EQ quoteqty.q-no 
                AND ASI.quotechg.line EQ quoteqty.LINE:
                ASSIGN 
                    quotechg.loc = ipcNewLoc  .
            END.
        
            quoteqty.loc = ipcNewLoc.         
        END.
    
        quoteitm.loc = ipcNewLoc.         
    END.       
    
    quotehd.loc = ipcNewLoc.      
   
END.

RUN spProgressBar ("Change Location", 54, 100).

FOR EACH rfq  FIELD(loc company rfq-no)
    WHERE rfq.company EQ ipcCompany
    AND rfq.loc EQ ipcOldLoc
    TRANSACTION: 
    
    FOR EACH rfqitem FIELD (loc) EXCLUSIVE-LOCK
        WHERE rfqitem.company EQ rfq.company
        AND rfqitem.rfq-no EQ rfq.rfq-no             
        AND rfqitem.loc EQ ipcOldLoc:           
        
        rfqitem.loc = ipcNewLoc.         
    END.
    
    rfq.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 55, 100).

FOR EACH rfq-ctrl FIELD(loc)
    WHERE rfq-ctrl.company EQ ipcCompany
    AND rfq-ctrl.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rfq-ctrl.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 56, 100).

FOR EACH rm-bin FIELD(loc)
    WHERE rm-bin.company EQ ipcCompany
    AND rm-bin.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rm-bin.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 57, 100).

FOR EACH rm-rcpt FIELD(loc)
    WHERE rm-rcpt.company EQ ipcCompany
    AND rm-rcpt.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rm-rcpt.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 58, 100).

FOR EACH rm-rcpth FIELD(loc)
    WHERE rm-rcpth.company EQ ipcCompany
    AND rm-rcpth.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rm-rcpth.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 59, 100).

FOR EACH rm-rctd FIELD(loc loc2)
    WHERE rm-rctd.company EQ ipcCompany
    AND (rm-rctd.loc EQ ipcOldLoc OR rm-rctd.loc2 EQ ipcOldLoc)
    TRANSACTION: 
    IF rm-rctd.loc EQ ipcOldLoc THEN
        rm-rctd.loc = ipcNewLoc.    
    
    IF rm-rctd.loc2 EQ ipcOldLoc THEN
        rm-rctd.loc2 = ipcNewLoc. 
END.

RUN spProgressBar ("Change Location", 60, 100).

FOR EACH rm-rcth FIELD(loc)
    WHERE rm-rcth.company EQ ipcCompany
    AND rm-rcth.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rm-rcth.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 61, 100).

FOR EACH rm-rdtl FIELD(loc loc2)
    WHERE rm-rdtl.company EQ ipcCompany
    AND (rm-rdtl.loc EQ ipcOldLoc OR rm-rdtl.loc2 EQ ipcOldLoc)
    TRANSACTION: 
    IF rm-rdtl.loc EQ ipcOldLoc THEN
        rm-rdtl.loc = ipcNewLoc.    
    
    IF rm-rdtl.loc2 EQ ipcOldLoc THEN
        rm-rdtl.loc2 = ipcNewLoc. 
END.

RUN spProgressBar ("Change Location", 62, 100).

FOR EACH rm-rdtlh FIELD(loc loc2)
    WHERE rm-rdtlh.company EQ ipcCompany
    AND (rm-rdtlh.loc EQ ipcOldLoc OR rm-rdtlh.loc2 EQ ipcOldLoc)
    TRANSACTION: 
    IF rm-rdtlh.loc EQ ipcOldLoc THEN
        rm-rdtlh.loc = ipcNewLoc.    
    
    IF rm-rdtlh.loc2 EQ ipcOldLoc THEN
        rm-rdtlh.loc2 = ipcNewLoc. 
END.

RUN spProgressBar ("Change Location", 63, 100).

FOR EACH rm-receipts FIELD(loc)
    WHERE rm-receipts.company EQ ipcCompany
    AND rm-receipts.loc EQ ipcOldLoc
    TRANSACTION: 
    
    rm-receipts.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 64, 100).

FOR EACH routing FIELD(loc)
    WHERE routing.company EQ ipcCompany
    AND routing.loc EQ ipcOldLoc
    TRANSACTION: 
    
    routing.loc = ipcNewLoc.    
END.
     
RUN spProgressBar ("Change Location", 65, 100).

FOR EACH routing-mtx  FIELD(loc)
    WHERE routing-mtx.company EQ ipcCompany
    AND routing-mtx.loc EQ ipcOldLoc
    TRANSACTION: 
    
    routing-mtx.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 66, 100).

FOR EACH shipto FIELD(loc dock-loc)
    WHERE shipto.company EQ ipcCompany
    AND (shipto.loc EQ ipcOldLoc OR shipto.dock-loc EQ ipcOldLoc)
    TRANSACTION: 
    IF shipto.loc EQ ipcOldLoc THEN
        shipto.loc = ipcNewLoc.    
    
    IF shipto.dock-loc EQ ipcOldLoc THEN
        shipto.dock-loc = ipcNewLoc. 
END.

RUN spProgressBar ("Change Location", 67, 100).

FOR EACH ssrelbol FIELD(loc)
    WHERE ssrelbol.company EQ ipcCompany
    AND ssrelbol.loc EQ ipcOldLoc
    TRANSACTION: 
    
    ssrelbol.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 68, 100).

FOR EACH stack-flute FIELD(loc)
    WHERE stack-flute.company EQ ipcCompany
    AND stack-flute.loc EQ ipcOldLoc
    TRANSACTION: 
    
    stack-flute.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 69, 100).

FOR EACH stack-size FIELD(loc)
    WHERE stack-size.company EQ ipcCompany
    AND stack-size.loc EQ ipcOldLoc
    TRANSACTION: 
    
    stack-size.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 70, 100).

FOR EACH storageCost FIELD(location)
    WHERE storageCost.company EQ ipcCompany
    AND storageCost.location EQ ipcOldLoc
    TRANSACTION: 
    
    storageCost.location = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 71, 100).

FOR EACH style-score FIELD(loc)
    WHERE style-score.company EQ ipcCompany
    AND style-score.loc EQ ipcOldLoc
    TRANSACTION: 
    
    style-score.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 72, 100).

FOR EACH truck FIELD(loc)
    WHERE truck.company EQ ipcCompany
    AND truck.loc EQ ipcOldLoc
    TRANSACTION: 
    
    truck.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 73, 100).

FOR EACH usercomp  FIELD(loc)
    WHERE usercomp.company EQ ipcCompany
    AND usercomp.loc EQ ipcOldLoc 
    TRANSACTION: 
    
    usercomp.loc = ipcNewLoc.     
END.   

RUN spProgressBar ("Change Location", 74, 100).

FOR EACH usr FIELD(loc)
    WHERE usr.company EQ ipcCompany
    AND usr.loc EQ ipcOldLoc
    TRANSACTION: 
    
    usr.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 75, 100).

FOR EACH usrx FIELD(loc)
    WHERE usrx.company EQ ipcCompany
    AND usrx.loc EQ ipcOldLoc
    TRANSACTION: 
    
    usrx.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 76, 100).

FOR EACH vend  FIELD(loc)
    WHERE vend.company EQ ipcCompany
    AND vend.loc EQ ipcOldLoc
    TRANSACTION: 
    
    vend.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 77, 100).

FOR EACH wip-bin FIELD(loc)
    WHERE wip-bin.company EQ ipcCompany
    AND wip-bin.loc EQ ipcOldLoc
    TRANSACTION: 
    
    wip-bin.loc = ipcNewLoc.    
END.

RUN spProgressBar ("Change Location", 78, 100).

FOR EACH wiptag FIELD(loc)
    WHERE wiptag.company EQ ipcCompany
    AND wiptag.loc EQ ipcOldLoc
    TRANSACTION: 
    
    wiptag.loc = ipcNewLoc.    
END.

do transaction:
  FIND CURRENT loc EXCLUSIVE-LOCK NO-ERROR.
  FIND CURRENT location EXCLUSIVE-LOCK NO-ERROR.

  if avail loc then do:
    find first b-loc
        where b-loc.company eq ipcCompany
          and b-loc.loc eq ipcNewLoc
        no-lock no-error.                   
    if avail b-loc then do:
      for each notes where notes.rec_key eq loc.rec_key:
        notes.rec_key = b-loc.rec_key.
      end.
      delete loc.
      DELETE location.
      IF b-loc.ACTIVE EQ ? THEN do:
          FIND CURRENT b-loc EXCLUSIVE-LOCK NO-ERROR.
          ASSIGN b-loc.ACTIVE = YES .
      END.
    end. /* avail b-cust*/
    ELSE do:
         loc.loc = ipcNewLoc.
         location.locationCode = ipcNewLoc.
         IF loc.ACTIVE EQ ? THEN
          ASSIGN loc.ACTIVE = YES .
    END. /* else do */
  end.
end.

RUN spProgressBar (?, ?, 100).

RELEASE loc.
RELEASE location. 
    
    
/* **********************  Internal Procedures  *********************** */

    
