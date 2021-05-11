/* vendItemCostExpire.p - rstark - 5.4.2021 */

DEFINE TEMP-TABLE ttVendItemCost NO-UNDO
    FIELD company    AS CHARACTER
    FIELD itemID     AS CHARACTER
    FIELD itemType   AS CHARACTER
    FIELD vendorID   AS CHARACTER
    FIELD customerID AS CHARACTER
    FIELD estimateNo AS CHARACTER
    FIELD formNo     AS INTEGER
    FIELD blankNo    AS INTEGER
    .

DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.

FOR EACH vendItemCost NO-LOCK
    BREAK BY vendItemCost.company
          BY vendItemCost.itemID
          BY vendItemCost.itemType
          BY vendItemCost.vendorID
          BY vendItemCost.customerID
          BY vendItemCost.estimateNo
          BY vendItemCost.formNo
          BY vendItemCost.blankNo
          BY vendItemCost.effectiveDate  DESCENDING
          BY vendItemCost.expirationDate DESCENDING
    :
    IF NOT FIRST(vendItemCost.effectiveDate) THEN DO:
        IF CAN-FIND(FIRST ttVendItemCost
                    WHERE ttVendItemCost.company    EQ vendItemCost.company    
                      AND ttVendItemCost.itemID     EQ vendItemCost.itemID     
                      AND ttVendItemCost.itemType   EQ vendItemCost.itemType   
                      AND ttVendItemCost.vendorID   EQ vendItemCost.vendorID   
                      AND ttVendItemCost.customerID EQ vendItemCost.customerID 
                      AND ttVendItemCost.estimateNo EQ vendItemCost.estimateNo 
                      AND ttVendItemCost.formNo     EQ vendItemCost.formNo     
                      AND ttVendItemCost.blankNo    EQ vendItemCost.blankNo) THEN NEXT.
        CREATE ttVendItemCost.
        ASSIGN
            ttVendItemCost.company    = vendItemCost.company
            ttVendItemCost.itemID     = vendItemCost.itemID
            ttVendItemCost.itemType   = vendItemCost.itemType
            ttVendItemCost.vendorID   = vendItemCost.vendorID
            ttVendItemCost.customerID = vendItemCost.customerID
            ttVendItemCost.estimateNo = vendItemCost.estimateNo
            ttVendItemCost.formNo     = vendItemCost.formNo
            ttVendItemCost.blankNo    = vendItemCost.blankNo
            .
    END. /* if not avail */
END. /* each venditemcost */

RUN system/VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

FOR EACH ttVendItemCost:
    EXPORT ttVendItemCost.
    RUN Vendor_ExpireOldCost IN hVendorCostProcs (
        ttVendItemCost.company, 
        ttVendItemCost.itemID,
        ttVendItemCost.itemType,
        ttVendItemCost.vendorID,
        ttVendItemCost.customerID,
        ttVendItemCost.estimateNo,
        ttVendItemCost.formNo,
        ttVendItemCost.blankNo
        ).
END. /* each ttVendItemCost */

IF VALID-HANDLE(hVendorCostProcs) THEN
DELETE PROCEDURE hVendorCostProcs.
