/*------------------------------------------------------------------------
    File        : api/CalculateTax.p
    Purpose     : API to fetch the tax amount for given items

    Syntax      :

    Description : API to fetch the tax amount for given items

    Author(s)   : Mithun Porandla
    Created     : Tue Jun 23 07:33:22 EDT 2020
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

/* Variables to store line items request data */
DEFINE VARIABLE lcLineItemsData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineItemsData AS LONGCHAR  NO-UNDO.

/* Variables to store flexible code field's request data */
DEFINE VARIABLE lcFlexiCodeData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatFlexiCodeData AS LONGCHAR  NO-UNDO.

/* Variables to store flexible numeric field's request data */
DEFINE VARIABLE lcFlexiNumericData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatFlexiNumericData AS LONGCHAR  NO-UNDO.

/* Variables to store flexible date field's request data */
DEFINE VARIABLE lcFlexiDateData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatFlexiDateData AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE cDocumentNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDocumentDate AS CHARACTER NO-UNDO.

DEFINE VARIABLE cTaxAreaID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustClassCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToAddr1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToAddr2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToCity    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToState   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToZip     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToCountry AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemClassCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemQuantity  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemPrice     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLineID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTermsCode     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFlexiFieldID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlexiCode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlexiNumeric AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlexiDate    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRequestDataType AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail.    
DEFINE BUFFER bf-APIOutboundDetail3 FOR APIOutboundDetail.
DEFINE BUFFER bf-APIOutboundDetail4 FOR APIOutboundDetail.
    
IF ipcRequestHandler NE "" THEN
    RUN VALUE(ipcRequestHandler) (
        INPUT TABLE  ttArgs,
        INPUT ipiAPIOutboundID,
        INPUT ipiAPIOutboundTriggerID,
        INPUT-OUTPUT ioplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
ELSE DO:
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID
         NO-ERROR.
    IF NOT AVAILABLE APIOutbound THEN DO:
        ASSIGN
            opcMessage = "No APIOutbound record found"
            oplSuccess = FALSE
            .
        RETURN.        
    END.             
    
    cRequestDataType = APIOutbound.requestDataType.
    
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "inv-head"
         NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "ar-inv"
             NO-ERROR.            
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid inv-head or ar-inv record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    END.
    
    FIND FIRST inv-head NO-LOCK
         WHERE ROWID(inv-head) = TO-ROWID(ttArgs.argValue)
         NO-ERROR.
    IF NOT AVAILABLE inv-head THEN DO:
        FIND FIRST ar-inv NO-LOCK
             WHERE ROWID(ar-inv) = TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE ar-inv THEN DO:            
            ASSIGN
                opcMessage = "Invalid inv-head or ar-inv ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    END.

    
    FIND FIRST bf-APIOutboundDetail1 NO-LOCK
         WHERE bf-APIOutboundDetail1.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail1.detailID      EQ "LineItems"
           AND bf-APIOutboundDetail1.parentID      EQ APIOutbound.apiID
         NO-ERROR.
    
    FIND FIRST bf-APIOutboundDetail2 NO-LOCK
         WHERE bf-APIOutboundDetail2.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail2.detailID      EQ "FlexibleCodeFields"
           AND bf-APIOutboundDetail2.parentID      EQ bf-APIOutboundDetail1.detailID
         NO-ERROR.

    FIND FIRST bf-APIOutboundDetail3 NO-LOCK
         WHERE bf-APIOutboundDetail3.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail3.detailID      EQ "FlexibleNumericFields"
           AND bf-APIOutboundDetail3.parentID      EQ bf-APIOutboundDetail1.detailID
         NO-ERROR.

    FIND FIRST bf-APIOutboundDetail4 NO-LOCK
         WHERE bf-APIOutboundDetail4.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail4.detailID      EQ "FlexibleDateFields"
           AND bf-APIOutboundDetail4.parentID      EQ bf-APIOutboundDetail1.detailID
         NO-ERROR.
    
    IF AVAILABLE inv-head THEN DO:
        IF AVAILABLE bf-APIOutboundDetail1 THEN DO:
            FOR EACH inv-line NO-LOCK 
                WHERE inv-line.r-no EQ inv-head.r-no:
                ASSIGN
                    lcLineItemsData = bf-APIOutboundDetail1.data
                    cTaxAreaID      = STRING(0)   /* TBD */                       
                    cShipToAddr1    = inv-head.sold-addr[1]
                    cShipToAddr2    = inv-head.sold-addr[2]
                    cShipToCity     = inv-head.sold-city
                    cShipToState    = inv-head.sold-state
                    cShipToZip      = inv-head.sold-zip
                    cShipToCountry  = "" /* Not available in inv-head */
                    cItemID         = inv-line.i-no
                    cCustomerID     = inv-head.cust-no
                    cItemQuantity   = STRING(inv-line.inv-qty, "->>>>>>>9.9<<")
                    cItemPrice      = STRING(inv-line.t-price, ">>>>>>>9.99<<<<") 
                    cLineID         = STRING(inv-line.line)
                    cTermsCode      = inv-head.terms
                    .
                
                RUN pGetProductClass (
                    INPUT  inv-line.company,
                    INPUT  inv-line.i-no,
                    OUTPUT cItemClassCode
                    ).
                    
                RUN pGetCustClassCode(
                    INPUT  inv-line.company,
                    INPUT  inv-head.cust-no,
                    OUTPUT cCustClassCode
                    ). 
                
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TaxAreaID", cTaxAreaID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToStreetAddress1", cShipToAddr1).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToStreetAddress2", cShipToAddr1).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToCity", cShipToCity).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToState", cShipToState).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToZip", cShipToZip).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToCountry", cShipToCountry).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemClassCode", cItemClassCode).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemID", cItemID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CustomerClassCode", cCustClassCode).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CustomerID", cCustomerID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemQuantity", cItemQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "LineID", cLineID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TermsCode", "FOB").
                
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleCodeFields$", lcConcatFlexiCodeData).
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleNumericFields$", lcConcatFlexiNumericData).
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleDateFields$", lcConcatFlexiDateData).
                
                lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData.
            END.
            
            RUN pUpdateDelimiter(
                INPUT-OUTPUT lcConcatLineItemsData,
                INPUT        cRequestDataType
                ).                
        END.
    END.
    ELSE IF AVAILABLE ar-inv THEN DO:
        RUN oe/custxship.p (
            INPUT  ar-inv.company,
            INPUT  ar-inv.cust-no,
            INPUT  ar-inv.ship-id,
            BUFFER shipto
            ).
        IF AVAILABLE shipTo THEN
            ASSIGN
                cShipToAddr1    = shipto.ship-addr[1]
                cShipToAddr2    = shipto.ship-addr[2]
                cShipToCity     = shipto.ship-city
                cShipToState    = shipto.ship-state
                cShipToZip      = shipto.ship-zip
                cShipToCountry  = shipto.country
                .
        
        IF AVAILABLE bf-APIOutboundDetail1 THEN DO:
            FOR EACH ar-invl NO-LOCK 
                WHERE ar-invl.x-no EQ ar-inv.x-no:
                ASSIGN
                    lcLineItemsData = bf-APIOutboundDetail1.data
                    cTaxAreaID      = STRING(0)   /* TBD */                       
                    cItemID         = ar-invl.i-no
                    cCustomerID     = ar-inv.cust-no
                    cItemQuantity   = STRING(ar-invl.inv-qty, "->>>>>>>9.9<<")
                    cItemPrice      = STRING(ar-invl.unit-pr, ">>>>>>>9.99<<<<") 
                    cLineID         = STRING(ar-invl.line)
                    cTermsCode      = ar-inv.terms
                    .
                
                RUN pGetProductClass (
                    INPUT  ar-invl.company,
                    INPUT  ar-invl.i-no,
                    OUTPUT cItemClassCode
                    ).
                    
                RUN pGetCustClassCode(
                    INPUT  ar-invl.company,
                    INPUT  ar-inv.cust-no,
                    OUTPUT cCustClassCode
                    ). 
                
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TaxAreaID", cTaxAreaID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToStreetAddress1", cShipToAddr1).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToStreetAddress2", cShipToAddr1).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToCity", cShipToCity).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToState", cShipToState).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToZip", cShipToZip).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ShipToCountry", cShipToCountry).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemClassCode", cItemClassCode).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemID", cItemID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CustomerClassCode", cCustClassCode).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "CustomerID", cCustomerID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemQuantity", cItemQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "ItemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "LineID", cLineID).
                RUN updateRequestData(INPUT-OUTPUT lcLineItemsData, "TermsCode", "FOB").
                
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleCodeFields$", lcConcatFlexiCodeData).
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleNumericFields$", lcConcatFlexiNumericData).
                lcLineItemsData = REPLACE(lcLineItemsData, "$FlexibleDateFields$", lcConcatFlexiDateData).
                
                lcConcatLineItemsData = lcConcatLineItemsData + lcLineItemsData.
            END.
            
            RUN pUpdateDelimiter(
                INPUT-OUTPUT lcConcatLineItemsData,
                INPUT        cRequestDataType
                ).                
        END.    
    END.    
    
    ASSIGN
        cDocumentNo   = STRING(fGetAPITransactionCounter(ipiAPIOutboundID))
        cDocumentDate = STRING(TODAY)
        .
            
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"DocumentNo",cDocumentNo).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"DocumentDate",cDocumentDate).
    
    ioplcRequestData = REPLACE(ioplcRequestData, "$LineItems$", lcConcatLineItemsData).
    
    ASSIGN   
        opcMessage       = "Success"
        oplSuccess       = TRUE
        .
END.        
        


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetCustClassCode PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustClassCode AS CHARACTER NO-UNDO.

    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ipcCompany 
           AND cust.cust-no EQ ipcCustomerID
         NO-ERROR.
    IF AVAILABLE cust THEN
        opcCustClassCode = cust.spare-char-2.
END PROCEDURE.

PROCEDURE pGetProductClass PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemClassCode AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany 
           AND itemfg.i-no    EQ ipcItemID
         NO-ERROR.
    IF AVAILABLE itemfg THEN
        opcItemClassCode = itemfg.productTaxClass.
END PROCEDURE.

