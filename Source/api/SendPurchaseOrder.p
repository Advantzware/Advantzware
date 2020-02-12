/*------------------------------------------------------------------------
    File        : api/SendPurchaseOrder.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    /* Variables to store order line's request data */
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineData AS LONGCHAR  NO-UNDO.

    /* Variables to store order line adder's request data */
    DEFINE VARIABLE lcLineAdderData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineAdderData AS LONGCHAR  NO-UNDO.

    /* Variables to store order line scores request data */
    DEFINE VARIABLE lcLineScoresData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineScoresData AS LONGCHAR  NO-UNDO.
    
    /* Purchase Order Header Variables */
    DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNO              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoType            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoStatus          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoStatusExt       AS CHARACTER NO-UNDO.   /* Will store the extension of the PO Status. Eg: "Open", Delete */
    DEFINE VARIABLE cPoDate            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDueDate           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorName        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorAddress1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorAddress2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorAddress3    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorCity        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorState       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorZip         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorAddressFull AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cContact           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToName        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress3    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToCity        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToState       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToZip         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddressFull AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCarrierID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightTerms      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightFOB        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBuyer             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNotes           AS CHARACTER NO-UNDO.
    
    /* Purchase Order Line Variables */
    DEFINE VARIABLE cPoLine            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityOrdered   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemType          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc1         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc2         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemIDVendor      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOverPct           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUnderPct          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineDueDate     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWidth         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemLength        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDepth         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostPerUOM        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostUOM           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostSetup         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostDiscount      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerID        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperationID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStackHeight       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletWidth       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletLength      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletHeight      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUnitPallet        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID2            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDFormNo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDBlankNo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineStatus      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityReceived  AS CHARACTER NO-UNDO.
    
    /* Purchase Order Line adder Variables */
    DEFINE VARIABLE cAdderItemID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdderItemName     AS CHARACTER NO-UNDO.

    /* Purchase Order Line scores Variables */
    DEFINE VARIABLE cScoreOn           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSize         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreType         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cWhsCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyPerPack        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurchaseUnit      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail.    
    DEFINE BUFFER bf-reftable1          FOR reftable.
    DEFINE BUFFER bf-reftable2          FOR reftable.
    
    DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    /* This is to run client specific request handler to fetch request data */
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:    
        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND APIOutboundDetail.detailID      EQ "detail"
               AND APIOutboundDetail.parentID      EQ "SendPurchaseOrder"
             NO-ERROR.
        
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ detail ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        FIND FIRST bf-APIOutboundDetail1 NO-LOCK
             WHERE bf-APIOutboundDetail1.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail1.detailID      EQ "adder"
               AND bf-APIOutboundDetail1.parentID      EQ APIOutboundDetail.detailID
             NO-ERROR.
        
        FIND FIRST bf-APIOutboundDetail2 NO-LOCK
             WHERE bf-APIOutboundDetail2.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail2.detailID      EQ "scores"
               AND bf-APIOutboundDetail2.parentID      EQ APIOutboundDetail.detailID
             NO-ERROR.
                        
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "po-ord"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid po-ord record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST po-ord NO-LOCK
             WHERE ROWID(po-ord) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE po-ord THEN DO:
            ASSIGN
                opcMessage = "Invalid po-ord ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        IF NOT CAN-FIND(FIRST po-ordl
             WHERE po-ordl.company EQ po-ord.company
               AND po-ordl.po-no   EQ po-ord.po-no) THEN DO:
            ASSIGN
                opcMessage = "No po-ordl records available for po [ " + STRING(po-ord.po-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        ASSIGN
            cCompany           = STRING(po-ord.company)
            cPoNO              = STRING(po-ord.po-no)
            cPoType            = STRING(po-ord.type)
            cPoStatus          = STRING(po-ord.stat)
            cPoStatusExt       = IF po-ord.stat EQ "H" THEN
                                     "Delete"
                                 ELSE
                                     "Open"
            cPoDate            = STRING(po-ord.po-date)
            cDueDate           = STRING(po-ord.due-date)
            cVendorID          = STRING(po-ord.vend-no)
            cContact           = STRING(po-ord.contact)
            cShipToID          = STRING(po-ord.ship-id)
            cShipToName        = STRING(po-ord.ship-name)
            cShipToAddress1    = STRING(po-ord.ship-addr[1])
            cShipToAddress2    = STRING(po-ord.ship-addr[2])
            cShipToCity        = STRING(po-ord.ship-city)
            cShipToState       = STRING(po-ord.ship-state)
            cShipToZip         = STRING(po-ord.ship-zip)
            cShipToAddress3    = cShipToCity + ","
                               + cShipToState + " "
                               + cShipToZip
            cShipToAddressFull = cShipToAddress1 + " "
                               + cShipToAddress2 + " "
                               + cShipToAddress3
            cCarrierID         = STRING(po-ord.carrier)
            cFreightTerms      = STRING(po-ord.frt-pay)
            cFreightFOB        = STRING(po-ord.fob-code)
            cBuyer             = STRING(po-ord.buyer)
            cWhsCode           = po-ord.loc
            .
        
        /* Fetch purchase order notes from notes table */    
        FOR EACH notes NO-LOCK
           WHERE notes.rec_key EQ po-ord.rec_key:
            cPoNotes = cPoNotes + STRING(notes.note_text).
        END.
        
        /* Fetch Vendor details for the purchase order */    
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ po-ord.company
               AND vend.vend-no EQ po-ord.vend-no
             NO-ERROR.
        IF AVAILABLE vend THEN
            ASSIGN
                cVendorName        = STRING(vend.name)
                cVendorAddress1    = STRING(vend.add1)
                cVendorAddress2    = STRING(vend.add2)
                cVendorCity        = STRING(vend.city)
                cVendorState       = STRING(vend.state)
                cVendorZip         = STRING(vend.zip)
                cVendorAddress3    = cVendorCity + ","
                                   + cVendorState + " "
                                   + cVendorZip
                cVendorAddressFull = cVendorAddress1 + " " 
                                   + cVendorAddress2 + " "
                                   + cVendorAddress3
                .
        
        /* Fetch line details for the purchase order */         
        FOR EACH po-ordl
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:                       
            
            ASSIGN
                lcLineData            = STRING(APIOutboundDetail.data)
                lcConcatLineAdderData = ""
                cPoLine               = STRING(po-ordl.line)
                cQuantityOrdered      = STRING(po-ordl.ord-qty,"->>>>>>>>9.9<<<<<")
                cQuantityUOM          = STRING(po-ordl.pr-qty-uom)
                cItemType             = STRING(po-ordl.item-type)
                cItemID               = STRING(po-ordl.i-no)
                cItemName             = STRING(po-ordl.i-name)
                cItemDesc1            = STRING(po-ordl.dscr[1])
                cItemDesc2            = STRING(po-ordl.dscr[2])
                cItemIDVendor         = STRING(po-ordl.vend-i-no)
                cOverPct              = STRING(po-ordl.over-pct,">>9.99")
                cUnderPct             = STRING(po-ordl.under-pct,">>9.99")
                cPoLineDueDate        = STRING(po-ordl.due-date)
                cItemWidth            = STRING(po-ordl.s-wid,">>9.9999")
                cItemLength           = STRING(po-ordl.s-len,">>9.9999")
                cItemDepth            = STRING(po-ordl.s-dep,">>9.9999")
                cCostPerUOM           = STRING(po-ordl.cost,"->>>>>>9.99<<<<")
                cCostUOM              = STRING(po-ordl.pr-uom)
                cCostSetup            = STRING(po-ordl.setup)
                cCostDiscount         = STRING(po-ordl.disc,"->>>>>9.99")
                cCustomerID           = STRING(po-ordl.cust-no)
                cOrderNo              = STRING(po-ordl.ord-no)
                cPoLineStatus         = STRING(po-ordl.stat)
                cOperationID          = ""
                cQtyPerPack           = ""
                cStackHeight          = "0"
                cPalletWidth          = "0.00"
                cPalletHeight         = "0.00"
                cPalletLength         = "0.00"
                cUnitPallet           = "0"
                cPurchaseUnit         = STRING(po-ordl.pr-qty-uom)
                cJobID                = STRING(po-ordl.job-no)
                cJobID2               = STRING(po-ordl.job-no2)
                cJobIDFormNo          = STRING(po-ordl.s-num)
                cJobIDBlankNo         = STRING(po-ordl.b-num)
                cQuantityReceived     = STRING(po-ordl.t-rec-qty, "->>>>>>>>9.9<<<<<")
                .

            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ po-ordl.company
                   AND itemfg.i-no    EQ po-ordl.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg AND NOT po-ordl.item-type THEN
                ASSIGN
                    cQtyPerPack   = STRING(itemfg.case-count)
                    cStackHeight  = STRING(itemfg.stackHeight)
                    cPalletWidth  = STRING(itemfg.unitWidth, ">>>>9.99")
                    cPalletHeight = STRING(itemfg.unitHeight, ">>>>9.99")
                    cPalletLength = STRING(itemfg.unitLength, ">>>>9.99")
                    cUnitPallet   = STRING(itemfg.case-pall)
                    .

            /* Fetch first operation id (job-mch.m-code) for the order line */
            RUN GetOperation IN hdJobProcs (
                INPUT        po-ordl.company,
                INPUT        po-ordl.job-no,
                INPUT        INTEGER(po-ordl.job-no2),
                INPUT        INTEGER(po-ordl.s-num),
                INPUT        "First",
                INPUT-OUTPUT cOperationID
                ).

            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLine", cPoLine).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLineStatus", cPoLineStatus).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityOrdered", cQuantityOrdered).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityUOM", cQuantityUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemType", cItemType).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemID", cItemID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemName", cItemName).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDesc1", cItemDesc1).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDesc2", cItemDesc2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemIDVendor", cItemIDVendor).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "overPct", cOverPct).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "underPct", cUnderPct).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLineDueDate", cPoLineDueDate).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWidth", cItemWidth).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLength", cItemLength).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDepth", cItemDepth).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "costPerUOM", cCostPerUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "costUOM", cCostUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "costSetup", cCostSetup).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "costDiscount", cCostDiscount).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "customerID", cCustomerID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "orderNo", cOrderNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "stackHeight", cStackHeight).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "palletWidth", cPalletWidth).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "palletLength", cPalletLength).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "palletHeight", cPalletHeight).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "unitPallet", cUnitPallet).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "operationID", cOperationID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobID", cJobID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobID2", cJobID2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobIDFormNo", cJobIDFormNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobIDBlankNo", cJobIDBlankNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "whscode", cWhsCode).                
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "unitsperpack", cQtyPerPack).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "purchaseunit", cPurchaseUnit).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityReceived", cQuantityReceived).
                        
            IF AVAILABLE bf-APIOutboundDetail1 THEN DO:
                /* Fetch adder details for the purchase order line */
                FOR EACH po-ordl-add NO-LOCK
                   WHERE po-ordl-add.company EQ po-ordl.company
                     AND po-ordl-add.po-no   EQ po-ordl.po-no  
                     AND po-ordl-add.line    EQ po-ordl.line:
                    
                    ASSIGN
                        lcLineAdderData = STRING(bf-APIOutboundDetail1.data)
                        cAdderItemID    = STRING(po-ordl-add.adder-i-no)
                        cAdderItemName  = ""
                        .
                    
                    FIND FIRST item NO-LOCK
                         WHERE item.company EQ po-ordl-add.company 
                           AND item.i-no    EQ po-ordl-add.adder-i-no NO-ERROR.
                    IF AVAILABLE item THEN
                        cAdderItemName = item.i-name.
                    
                    RUN updateRequestData(INPUT-OUTPUT lcLineAdderData, "adderItemID", cAdderItemID).
                    RUN updateRequestData(INPUT-OUTPUT lcLineAdderData, "adderItemName", cAdderItemName).
                    
                    lcConcatLineAdderData = lcConcatLineAdderData + "," + lcLineAdderData.
                END.
            END.
            
            IF AVAILABLE bf-APIOutboundDetail2 THEN DO:
                cScoreOn   = IF po-ordl.spare-char-1 = "LENGTH" THEN
                                 "Length" 
                             ELSE 
                                 "Width".
                
                FIND FIRST bf-reftable1
                     WHERE bf-reftable1.reftable EQ "POLSCORE"
                       AND bf-reftable1.company  EQ po-ordl.company
                       AND bf-reftable1.loc      EQ "1"
                       AND bf-reftable1.code     EQ STRING(po-ordl.po-no,"9999999999")
                       AND bf-reftable1.code2    EQ STRING(po-ordl.line, "9999999999")
                     NO-ERROR.
                IF AVAILABLE bf-reftable1 THEN DO:
                    DO iIndex = 1 TO 12:
                        IF bf-reftable1.val[iIndex] EQ 0 THEN
                            LEAVE.
                        
                        ASSIGN
                            cScoreSize = ""
                            cScoreType = ""
                            .
    
                        ASSIGN
                            lcLineScoresData = STRING(bf-APIOutboundDetail2.data)
                            cScoreSize       = STRING(bf-reftable1.val[iIndex])
                            cScoreType       = SUBSTRING(bf-reftable1.dscr,iIndex,1)
                            .
                        
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreOn", cScoreOn).
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreSize", cScoreSize).
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreType", cScoreType).
                        
                        lcConcatLineScoresData = lcConcatLineScoresData + "," + lcLineScoresData.
                    END.
                END.
                    
                FIND FIRST bf-reftable2
                     WHERE bf-reftable2.reftable EQ "POLSCORE"
                       AND bf-reftable2.company  EQ po-ordl.company
                       AND bf-reftable2.loc      EQ "2"
                       AND bf-reftable2.code     EQ STRING(po-ordl.po-no,"9999999999")
                       AND bf-reftable2.code2    EQ STRING(po-ordl.line, "9999999999")
                     NO-ERROR.            
                IF AVAILABLE bf-reftable2 THEN DO:
                    DO iIndex = 1 TO 8:
                        IF bf-reftable2.val[iIndex] EQ 0 THEN
                            LEAVE.
                                                    
                        ASSIGN
                            cScoreSize = ""
                            cScoreType = ""
                            .
    
                        ASSIGN
                            lcLineScoresData = STRING(bf-APIOutboundDetail2.data)
                            cScoreSize       = STRING(bf-reftable2.val[iIndex])
                            cScoreType       = SUBSTRING(bf-reftable2.dscr,iIndex,1)
                            .
                        
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreOn", cScoreOn).
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreSize", cScoreSize).
                        RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreType", cScoreType).
                        
                        lcConcatLineScoresData = lcConcatLineScoresData + "," + lcLineScoresData.
                    END.
                END.
            END.
            
            lcConcatLineAdderData = TRIM(lcConcatLineAdderData,",").
            
            lcLineData = REPLACE(lcLineData, "$lineAdder$", lcConcatLineAdderData).

            lcConcatLineScoresData = TRIM(lcConcatLineScoresData,",").
            
            lcLineData = REPLACE(lcLineData, "$lineScores$", lcConcatLineScoresData).
                
            lcConcatLineData = lcConcatLineData + "," + lcLineData.
        END.      
        
        lcConcatLineData = TRIM(lcConcatLineData,",").        
                         
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "company", cCompany).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poNO", cPoNO).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poType", cPoType).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poStatus", cPoStatus).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poStatusExt", cPoStatusExt).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poDate", cPoDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "dueDate", cDueDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorID", cVendorID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorName", cVendorName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorAddress1", cVendorAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorAddress2", cVendorAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorAddress3", cVendorAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorCity", cVendorCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorState", cVendorState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorZip", cVendorZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vendorAddressFull", cVendorAddressFull).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "contact", cContact).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToID", cShipToID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToName", cShipToName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToAddress1", cShipToAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToAddress2", cShipToAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToAddress3", cShipToAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToCity", cShipToCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToState", cShipToState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToZip", cShipToZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToAddressFull", cShipToAddressFull).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "carrierID", cCarrierID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "freightTerms", cFreightTerms).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "freightFOB", cFreightFOB).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "buyer", cBuyer).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poNotes", cPoNotes).
    
        /* This replace is required for replacing nested JSON data */
        ioplcRequestData = REPLACE(ioplcRequestData, "$lineDetail$", lcConcatLineData).
        
        RELEASE bf-APIOutboundDetail1.
        RELEASE bf-APIOutboundDetail2.

        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
