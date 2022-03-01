/*------------------------------------------------------------------------
    File        : api/SendVendor.p
    Purpose     : Returns the request data for vendor addition

    Syntax      :

    Description : Returns the request data for vendor addition

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
    
    DEFINE BUFFER bf-vend FOR vend.
    
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
    
    IF ipcRequestHandler NE "" THEN 
        RUN VALUE(ipcRequestHandler) (
            INPUT  TABLE ttArgs,
            INPUT  ipiAPIOutboundID,
            INPUT  ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "vend" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid vend record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST bf-vend NO-LOCK
             WHERE ROWID(bf-vend) = TO-ROWID(ttArgs.argValue) 
             NO-ERROR.
        IF NOT AVAILABLE bf-vend THEN DO:
            ASSIGN
                opcMessage = "Invalid vend ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GLAccountDescription",bf-vend.actdscr).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Active",STRING(bf-vend.active EQ "A")).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GLAccountNumber",bf-vend.actnum).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorStreetAddress1",bf-vend.add1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorStreetAddress2",bf-vend.add2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorAreaCode",bf-vend.area-code).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Buyer",bf-vend.buyer).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Carrier",bf-vend.carrier).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrencyCode",bf-vend.curr-code).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorCity",bf-vend.city).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Company",bf-vend.company).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Contact",bf-vend.contact).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorCountry",bf-vend.country).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CreditHold",STRING(bf-vend.cr-hold)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CreditLimit",STRING(bf-vend.cr-lim)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CreditRating",bf-vend.cr-rating).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DiscountPercent",STRING(bf-vend.disc-%)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DiscountDays",STRING(bf-vend.disc-days)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorEmail",bf-vend.email).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorFax",bf-vend.fax).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorFarAreaCode",bf-vend.fax-area).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorFaxCountry",bf-vend.fax-country).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorFaxPrefix",bf-vend.fax-prefix).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LeadTime",STRING(bf-vend.lead-time)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Location",bf-vend.loc).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorName",bf-vend.name).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorPhone",bf-vend.phone).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorPhoneCountry",bf-vend.phone-country).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorPhonePrefix",bf-vend.phone-prefix).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorPostalCode",bf-vend.Postal).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorZip",bf-vend.zip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OverPercent",STRING(bf-vend.over-pct)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToStreetAddress1",bf-vend.r-add1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToStreetAddress2",bf-vend.r-add2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToCity",bf-vend.r-city).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToCountry",bf-vend.r-country).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToPostalCode",bf-vend.r-postal).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToState",bf-vend.r-state).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToZip",bf-vend.r-zip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RebatePercent",STRING(bf-vend.rebate-%)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "RemitToID",bf-vend.remit).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SalesRepresentative",bf-vend.salesrep).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipDays",STRING(bf-vend.ship-days)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorState",bf-vend.state).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TaxGroup",bf-vend.tax-gr).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Terms",bf-vend.terms).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SalesTerritory",bf-vend.terr).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorType",bf-vend.type).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UnderPercent",STRING(bf-vend.under-pct)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "VendorID",bf-vend.vend-no).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PaymentType",bf-vend.payment-type).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankRouting",STRING(bf-vend.Bank-RTN)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankAccountNumber",bf-vend.Bank-Acct).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankSwiftBIC",bf-vend.SwiftBIC).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FreightPayCode",bf-vend.frt-pay).
            
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.
