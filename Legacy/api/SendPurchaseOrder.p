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
    
    DEFINE INPUT        PARAMETER TABLE             FOR ttArgs.
    DEFINE INPUT        PARAMETER ipcParentID       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage        AS CHARACTER NO-UNDO.
    
    /* Variables to store order line's request data */
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineData AS LONGCHAR  NO-UNDO.

    /* Variables to store order line adder's request data */
    DEFINE VARIABLE lcLineAdderData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineAdderData AS LONGCHAR  NO-UNDO.

    /* Purchase Order Header Variables */
    DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNO              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoType            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoStatus          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoDate            AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE cJobID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID2            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDFormNo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDBlankNo      AS CHARACTER NO-UNDO.
    
    /* Purchase Order Line adder Variables */
    DEFINE VARIABLE cAdderItemID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdderItemName     AS CHARACTER NO-UNDO.
    
    /* The below variables are hardcoded as Premier is expecting these
       values, which currently are not mapped to any database table's fields */           
    DEFINE VARIABLE cRequestIP         AS CHARACTER NO-UNDO INITIAL "locahost".
    DEFINE VARIABLE cWhsCode           AS CHARACTER NO-UNDO INITIAL "005".
    DEFINE VARIABLE cQtyPerPack        AS CHARACTER NO-UNDO INITIAL "24.0".
    DEFINE VARIABLE cPurchaseUnit      AS CHARACTER NO-UNDO INITIAL "CASE24".
    
    DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
    
    /* This is to run client specific request handler to fetch request data */
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipcParentID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:    
        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.apiID    EQ ipcParentID
               AND APIOutboundDetail.detailID EQ "detail"
               AND APIOutboundDetail.parentID EQ ipcParentID
             NO-ERROR.
        
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ detail ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        FIND FIRST bf-APIOutboundDetail NO-LOCK
             WHERE bf-APIOutboundDetail.apiID    EQ ipcParentID
               AND bf-APIOutboundDetail.detailID EQ "adder"
               AND bf-APIOutboundDetail.parentID EQ APIOutboundDetail.detailID
             NO-ERROR.
        
        IF NOT AVAILABLE bf-APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ adder ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
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
            cPoDate            = STRING(po-ord.po-date)
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
                cQuantityOrdered      = STRING(po-ordl.ord-qty)
                cQuantityUOM          = STRING(po-ordl.pr-qty-uom)
                cItemType             = STRING(po-ordl.item-type)
                cItemID               = STRING(po-ordl.i-no)
                cItemName             = STRING(po-ordl.i-name)
                cItemDesc1            = STRING(po-ordl.dscr[1])
                cItemDesc2            = STRING(po-ordl.dscr[2])
                cItemIDVendor         = STRING(po-ordl.vend-i-no)
                cOverPct              = STRING(po-ordl.over-pct)
                cUnderPct             = STRING(po-ordl.under-pct)
                cPoLineDueDate        = STRING(po-ordl.due-date)
                cItemWidth            = STRING(po-ordl.s-wid)
                cItemLength           = STRING(po-ordl.s-len)
                cItemDepth            = STRING(po-ordl.s-dep)
                cCostPerUOM           = STRING(po-ordl.cost)
                cCostUOM              = STRING(po-ordl.pr-uom)
                cCostSetup            = STRING(po-ordl.setup)
                cCostDiscount         = STRING(po-ordl.disc)
                cCustomerID           = STRING(po-ordl.cust-no)
                cOrderNo              = STRING(po-ordl.ord-no)
                cOperationID          = ""
                cJobID                = STRING(po-ordl.job-no)
                cJobID2               = STRING(po-ordl.job-no2)
                cJobIDFormNo          = STRING(po-ordl.s-num)
                cJobIDBlankNo         = STRING(po-ordl.b-num)
                .
                                 
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.line", cPoLine).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.ord-qty", cQuantityOrdered).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.pr-qty-uom", cQuantityUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.item-type", cItemType).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.i-no", cItemID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.i-name", cItemName).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.dscr[1]", cItemDesc1).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.dscr[2]", cItemDesc2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.vend-i-no", cItemIDVendor).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.over-pct", cOverPct).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.under-pct", cUnderPct).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.due-date", cPoLineDueDate).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.s-wid", cItemWidth).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.s-len", cItemLength).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.s-dep", cItemDepth).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.cost", cCostPerUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.pr-uom", cCostUOM).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.setup", cCostSetup).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.disc", cCostDiscount).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.cust-no", cCustomerID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.ord-no", cOrderNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "TBD1", cOperationID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.job-no", cJobID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.job-no2", cJobID2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.s-num", cJobIDFormNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "po-ordl.b-num", cJobIDBlankNo).

            /* The below values are hardcoded as they are currently not mapped
               to any database table's field */
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "cWhsCode", cWhsCode).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "cQtyPerPack", cQtyPerPack).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "cPurchaseUnit", cPurchaseUnit).
            
            /* Fetch adder details for the purchase order line */
            FOR EACH po-ordl-add NO-LOCK
               WHERE po-ordl-add.company EQ po-ordl.company
                 AND po-ordl-add.po-no   EQ po-ordl.po-no  
                 AND po-ordl-add.line    EQ po-ordl.line:
                
                ASSIGN
                    lcLineAdderData = STRING(bf-APIOutboundDetail.data)
                    cAdderItemID    = STRING(po-ordl-add.adder-i-no)
                    cAdderItemName  = ""
                    .
                
                FIND FIRST item NO-LOCK
                     WHERE item.company EQ po-ordl-add.company 
                       AND item.i-no    EQ po-ordl-add.adder-i-no NO-ERROR.
                IF AVAILABLE item THEN
                    cAdderItemName = item.i-name.
                
                RUN updateRequestData(INPUT-OUTPUT lcLineAdderData, "po-ordl-add.adder-i-no", cAdderItemID).
                RUN updateRequestData(INPUT-OUTPUT lcLineAdderData, "item.i-name", cAdderItemName).
                
                lcConcatLineAdderData = lcConcatLineAdderData + "," + lcLineAdderData.
            END.
            
            lcConcatLineAdderData = TRIM(lcConcatLineAdderData,",").
            
            lcLineData = REPLACE(lcLineData, "$lineAdder$", lcConcatLineAdderData).
                
            lcConcatLineData = lcConcatLineData + "," + lcLineData.
        END.      
        
        lcConcatLineData = TRIM(lcConcatLineData,",").        
                         
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.company", cCompany).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.po-no", cPoNO).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.type", cPoType).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.stat", cPoStatus).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.po-date", cPoDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.vend-no", cVendorID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.name", cVendorName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.add1", cVendorAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.add2", cVendorAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cVendorAddress3", cVendorAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.city", cVendorCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.state", cVendorState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.zip", cVendorZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cVendorAddressFull", cVendorAddressFull).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.contact", cContact).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-id", cShipToID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-name", cShipToName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-addr[1]", cShipToAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-addr[2]", cShipToAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cShipToAddress3", cShipToAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-city", cShipToCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-state", cShipToState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.ship-zip", cShipToZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cShipToAddressFull", cShipToAddressFull).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.carrier", cCarrierID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.frt-pay", cFreightTerms).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.fob-code", cFreightFOB).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.buyer", cBuyer).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cPoNotes", cPoNotes).
    
        /* This replace is required for replacing nested JSON data */
        ioplcRequestData = REPLACE(ioplcRequestData, "$lineDetail$", lcConcatLineData).

        /* The below value is hardcoded as it is currently not mapped
           to any database table's field */
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cRequestIP", cRequestIP).
        
        RELEASE bf-APIOutboundDetail.
        
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
