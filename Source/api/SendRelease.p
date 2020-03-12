/*------------------------------------------------------------------------
    File        : api/SendRelease.p
    Purpose     : Returns the request data for picklist addition

    Syntax      :

    Description : Returns the request data for picklist addition

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
    
    DEFINE VARIABLE lcDetailData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatDetailData AS LONGCHAR NO-UNDO.
    DEFINE BUFFER   bf-oe-rell         FOR oe-rell.
    
    /* Release Header variables */
    DEFINE VARIABLE cCompany             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReleaseNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReleaseType         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReleaseStatus       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrintDate           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReleaseDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToName          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddress3      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToCity          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToState         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToZip           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddressFull   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCarrierID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrailerID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipFromWarehouseID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerName        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerAddress1    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerAddress2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerAddress3    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerCity        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerState       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerZip         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerAddressFull AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightTerms        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightFOB          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCsr                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipNotes           AS CHARACTER NO-UNDO.
    
    /* Release line variables */
    DEFINE VARIABLE cCustomerPO                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerLot                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPart                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderNo                       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToID                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToName                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToAddress1                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToAddress2                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToAddress3                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToCity                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToState                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToZip                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToAddressFull             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPOOrder               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLine                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityOrdered               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsRunAndShip                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsManagedInventory            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLineOverPct              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLineUnderPct             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPOOrderLine           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLineDueDateQualifier     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLineDueDate              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLinePromiseDateQualifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderLinePromiseDate          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc1                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc2                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc3                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityReleased              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReleaseLineType               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityReleased              AS DECIMAL   NO-UNDO.
    
    /* The below variables are hardcoded as Premier is expecting these
       values, which currently are not mapped to any database table's fields */
    DEFINE VARIABLE cPriority     AS CHARACTER NO-UNDO INITIAL "2".
    DEFINE VARIABLE cWhscode      AS CHARACTER NO-UNDO INITIAL "005".
    DEFINE VARIABLE cUnitsperpack AS CHARACTER NO-UNDO INITIAL "1".
    DEFINE VARIABLE cSalesunit    AS CHARACTER NO-UNDO INITIAL "Each".    
    
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
               AND APIOutboundDetail.parentID      EQ "SendRelease"
             NO-ERROR.    
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ detail ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "oe-relh"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid oe-relh record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-relh NO-LOCK
             WHERE ROWID(oe-relh) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE oe-relh THEN DO:
            ASSIGN
                opcMessage = "Invalid oe-relh ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        IF NOT CAN-FIND(FIRST oe-rell
             WHERE oe-rell.company EQ oe-relh.company
               AND oe-rell.r-no    EQ oe-relh.r-no) THEN DO:
            ASSIGN
                opcMessage = "No oe-rell records available for picklist [ " + STRING(oe-relh.r-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        ASSIGN
            cCompany             = STRING(oe-relh.company)
            cReleaseNo           = STRING(oe-relh.release#)
            cPrintDate           = IF oe-relh.prt-date EQ ? THEN "" ELSE STRING(oe-relh.prt-date)
            cReleaseDate         = IF oe-relh.rel-date EQ ? THEN "" ELSE STRING(oe-relh.rel-date)
            cShipToID            = STRING(oe-relh.ship-id)
            cCarrierID           = STRING(oe-relh.carrier)
            cTrailerID           = STRING(oe-relh.trailer)
            cCustomerID          = STRING(oe-relh.cust-no)
            cCsr                 = STRING(oe-relh.user-id)
            cShipFromWarehouseID = STRING(oe-relh.spare-char-1)
            cShipNotes           = STRING(oe-relh.ship-i[1]) + " "
                                 + STRING(oe-relh.ship-i[2]) + " "
                                 + STRING(oe-relh.ship-i[3]) + " "
                                 + STRING(oe-relh.ship-i[4])
            cReleaseStatus       = IF oe-relh.printed THEN "Update" ELSE "New"
            .
        
        RUN oe/custxship.p (
            INPUT oe-relh.company,
            INPUT oe-relh.cust-no,
            INPUT oe-relh.ship-id,
            BUFFER shipto
            ).
            
        IF AVAILABLE shipto THEN
            ASSIGN
                cShipToName        = STRING(shipto.ship-name)
                cShipToAddress1    = STRING(shipto.ship-addr[1])
                cShipToAddress2    = STRING(shipto.ship-addr[2])
                cShipToCity        = STRING(shipto.ship-city)
                cShipToState       = STRING(shipto.ship-state)
                cShipToZip         = STRING(shipto.ship-zip)
                cShipToAddress3    = cShipToCity + " "
                                   + cShipToCity + " "
                                   + cShipToZip
                cShipToAddressFull = cShipToAddress1 + " " 
                                   + cShipToAddress2 + " "
                                   + cShipToAddress3
                .    
        
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ oe-relh.company
               AND cust.cust-no EQ oe-relh.cust-no
             NO-ERROR.
        IF AVAILABLE cust THEN
            ASSIGN
                cCustomerName        = STRING(cust.name)
                cCustomerAddress1    = STRING(cust.addr[1])
                cCustomerAddress2    = STRING(cust.addr[2])
                cCustomerCity        = STRING(cust.city)
                cCustomerState       = STRING(cust.state)
                cCustomerZip         = STRING(cust.zip)
                cCustomerAddress3    = cCustomerCity + " " 
                                     + cCustomerState + " "
                                     + cCustomerZip        
                cCustomerAddressFull = cCustomerAddress1 + " "
                                     + cCustomerAddress2 + " "
                                     + cCustomerAddress3
                .
                                            
        FOR EACH oe-rell
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no:        
            
            ASSIGN
                cCustomerPO                    = STRING(oe-rell.po-no)
                cCustomerLot                   = STRING(oe-rell.lot-no)
                cOrderNo                       = STRING(oe-rell.ord-no)
                cOrderLine                     = STRING(oe-rell.line)
                cItemID                        = STRING(oe-rell.i-no)
                cCustomerPart                  = ""
                cSoldToID                      = ""
                cSoldToName                    = ""
                cSoldToAddress1                = ""
                cSoldToAddress2                = ""
                cSoldToAddress3                = ""
                cSoldToCity                    = ""
                cSoldToState                   = ""
                cSoldToZip                     = ""
                cSoldToAddressFull             = ""
                cCustomerPOOrder               = ""
                cQuantityOrdered               = ""
                cIsRunAndShip                  = ""
                cIsManagedInventory            = ""
                cOrderLineOverPct              = ""
                cOrderLineUnderPct             = ""
                cCustomerPOOrderLine           = ""
                cOrderLineDueDateQualifier     = ""
                cOrderLineDueDate              = ""
                cOrderLinePromiseDateQualifier = ""
                cOrderLinePromiseDate          = ""
                cItemName                      = ""
                cItemDesc1                     = ""
                cItemDesc2                     = ""
                cItemDesc3                     = ""
                cReleaseLineType               = ""
                cQuantityReleased              = ""
                dQuantityReleased              = 0
                .
            
            FOR EACH bf-oe-rell NO-LOCK
                WHERE bf-oe-rell.company EQ oe-rell.company
                  AND bf-oe-rell.ord-no  EQ oe-rell.ord-no
                  AND bf-oe-rell.i-no    EQ oe-rell.i-no
                  AND bf-oe-rell.po-no   EQ oe-rell.po-no
                  AND bf-oe-rell.lot-no  EQ oe-rell.lot-no:
                dQuantityReleased = dQuantityReleased + bf-oe-rell.qty.
            END.
            
            cQuantityReleased = STRING(dQuantityReleased).
            
            FIND FIRST oe-ord NO-LOCK
                 WHERE oe-ord.company EQ oe-rell.company
                   AND oe-ord.ord-no  EQ oe-rell.ord-no
                 NO-ERROR.
            IF AVAILABLE oe-ord THEN
                ASSIGN
                    cSoldToID          = STRING(oe-ord.sold-no)
                    cSoldToName        = STRING(oe-ord.sold-name)
                    cSoldToAddress1    = STRING(oe-ord.addr[1])
                    cSoldToAddress2    = STRING(oe-ord.addr[2])
                    cSoldToCity        = STRING(oe-ord.city)
                    cSoldToState       = STRING(oe-ord.state)
                    cSoldToZip         = STRING(oe-ord.zip)
                    cSoldToAddress3    = cSoldToCity + " " 
                                       + cSoldToState + " " 
                                       + cSoldToZip
                    cSoldToAddressFull = cSoldToAddress1 + " " 
                                       + cSoldToAddress2 + " "
                                       + cSoldToAddress3
                    cCustomerPOOrder   = STRING(oe-ord.po-no)
                    .
                                                    
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ oe-rell.company
                   AND oe-ordl.ord-no  EQ oe-rell.ord-no 
                   AND oe-ordl.i-no    EQ oe-rell.i-no 
                   AND oe-ordl.line    EQ oe-rell.line
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
                ASSIGN
                    cCustomerPart                  = STRING(oe-ordl.part-no)
                    cQuantityOrdered               = STRING(oe-ordl.qty)
                    cIsRunAndShip                  = STRING(oe-ordl.whsed)
                    cIsManagedInventory            = STRING(oe-ordl.managed)
                    cOrderLineOverPct              = STRING(oe-ordl.over-pct)
                    cOrderLineUnderPct             = STRING(oe-ordl.under-pct)
                    cCustomerPOOrderLine           = STRING(oe-ordl.po-no)
                    cOrderLineDueDateQualifier     = STRING(oe-ordl.req-code)
                    cOrderLineDueDate              = IF oe-ordl.req-date EQ ? THEN "" ELSE STRING(oe-ordl.req-date)
                    cOrderLinePromiseDateQualifier = STRING(oe-ordl.prom-date)
                    cOrderLinePromiseDate          = IF oe-ordl.prom-code EQ ? THEN "" ELSE STRING(oe-ordl.prom-code)
                    .
            
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ oe-rell.company
                   AND itemfg.i-no    EQ oe-rell.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg THEN
                ASSIGN
                    cItemName         = STRING(itemfg.i-name)
                    cItemDesc1        = STRING(itemfg.part-dscr1)
                    cItemDesc2        = STRING(itemfg.part-dscr2)
                    cItemDesc3        = STRING(itemfg.part-dscr3)
                    .                
                
            lcDetailData = STRING(APIOutboundDetail.data).
            
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.po-no", cCustomerPO).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.ord-no", cOrderNo).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.lot-no", cCustomerLot).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.line", cOrderLine).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.i-no", cItemID).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-rell.qty", cQuantityReleased).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.sold-no", cSoldToID).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.sold-name", cSoldToName).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.addr[1]", cSoldToAddress1).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.addr[2]", cSoldToAddress2).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "cSoldToAddress3", cSoldToAddress3).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.city", cSoldToCity).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.state", cSoldToState).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.zip", cSoldToZip).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "cSoldToAddressFull", cSoldToAddressFull).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ord.po-no", cCustomerPOOrder).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.part-no", cCustomerPart).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.qty", cQuantityOrdered).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.whsed", cIsRunAndShip).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.managed", cIsManagedInventory).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.over-pct", cOrderLineOverPct).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.under-pct", cOrderLineUnderPct).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.po-no", cCustomerPOOrderLine).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.req-code", cOrderLineDueDateQualifier).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.req-date", cOrderLineDueDate).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.prom-code", cOrderLinePromiseDateQualifier).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "oe-ordl.prom-date", cOrderLinePromiseDate).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "itemfg.i-name", cItemName).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "itemfg.part-dscr1", cItemDesc1).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "itemfg.part-dscr2", cItemDesc2).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "itemfg.part-dscr3", cItemDesc3).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "TBD1", cReleaseLineType).

            /* The below values are hardcoded as they are currently not mapped
               to any database table's field */
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "whscode", cWhscode).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "unitsperpack", cUnitsperpack).
            RUN updateRequestData(INPUT-OUTPUT lcDetailData, "salesunit", cSalesunit).
                    
            lcConcatDetailData = lcConcatDetailData + "," + lcDetailData.
        END.      
    
        lcConcatDetailData = TRIM(lcConcatDetailData,",").
            
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.company", cCompany).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.release#", cReleaseNo).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.prt-date", cPrintDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.rel-date", cReleaseDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.ship-id", cShipToID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.carrier", cCarrierID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.trailer", cTrailerID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.cust-no", cCustomerID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.user-id", cCsr).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cShipNotes", cShipNotes).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cReleaseStatus", cReleaseStatus).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD1", cReleaseType).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD2", cFreightTerms).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD3", cFreightFOB)            .
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "oe-relh.spare-char-1", cShipFromWarehouseID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-name", cShipToName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-addr[1]", cShipToAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-addr[2]", cShipToAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-city", cShipToCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-state", cShipToState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipto.ship-zip", cShipToZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cShipToAddress3", cShipToAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cShipToAddressFull", cShipToAddressFull).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.name", cCustomerName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.addr[1]", cCustomerAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.addr[2]", cCustomerAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cCustomerAddress3", cCustomerAddress3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.city", cCustomerCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.state", cCustomerState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cust.zip", cCustomerZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cCustomerAddressFull", cCustomerAddressFull).
        
        /* Priority value is hardcoded as this is currently not 
           mapped to any database table's field */
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "priority", cPriority).
        
        /* This replace is required for replacing nested JSON data */
        ioplcRequestData = REPLACE(ioplcRequestData, "$lineDetail$", lcConcatDetailData).
            
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .    
    END.
