/*------------------------------------------------------------------------
    File        : api/SendInvoice.p
    Purpose     : Returns the request data for X12 Invoice (810)

    Syntax      :

    Description : Returns the request data for X12 Invoice (810)

    Author(s)   : Wade Kaldawi
    Created     : Wed Apr 01 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{api/ttArgs.i}
{api/CommonAPIProcs.i}
{XMLOutput/ttNodes.i NEW}
    
DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
/* Variables to store invoice line's request data */
DEFINE VARIABLE lcLineData            AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineData      AS LONGCHAR  NO-UNDO.

/* Variables to store invoice Addon's request data */
DEFINE VARIABLE lcLineAddonData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineAddonData AS LONGCHAR  NO-UNDO.
    
/* Variables to store invoice address data */
DEFINE VARIABLE lcConcatAddressData   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcAddressData         AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcAddress2Data        AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatAddress2Data  AS LONGCHAR NO-UNDO.
    
/* Variables to store Tax request data */
DEFINE VARIABLE lcConcatTaxData       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcTaxData             AS LONGCHAR NO-UNDO.         
  
/* Variables to store Tax request data */ 
DEFINE VARIABLE lcConcatShiptoStreetData AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcShiptoStreetData       AS LONGCHAR NO-UNDO. 
DEFINE VARIABLE lcConcatBillToStreetData AS LONGCHAR NO-UNDO. 
DEFINE VARIABLE lcBillToStreetData       AS LONGCHAR NO-UNDO.  
       
/* Invoice Header Variables */
DEFINE VARIABLE cCompany              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentDate          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentTime          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceDate          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceNumber        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtInvoiceDate         AS DATE      NO-UNDO.
DEFINE VARIABLE cInvoiceDueDate       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceType          AS CHARACTER NO-UNDO.
           
DEFINE VARIABLE cTotalAmount          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dInvoiceTotalAmt      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLineTotalAmt         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cSELineCount          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dSELineCount          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFullDocument         AS LONGCHAR  NO-UNDO.
    
/* Invoice Line Variables */
DEFINE VARIABLE cItemLineNum          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemQty              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemUom              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemPrice            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemBuyerPart        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPartQualifier        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemDesc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustCountry          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvNotes             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLineCounter          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBolNum               AS INTEGER   NO-UNDO.
DEFINE VARIABLE cUomCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iQtyShipped           AS INTEGER   NO-UNDO.
DEFINE VARIABLE dUnitPrice            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQtyShipped           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCustPart             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoNum                AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLineCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLineTotal            AS CHARACTER NO-UNDO.
    
/* Invoice Addon Variables */
DEFINE VARIABLE cAddonAllowCharge     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscElem             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSacAgencyQualifier   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSacAgencyCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSacReferenceId       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAddonTaxType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTotalTaxDollars      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTaxPct               AS CHARACTER NO-UNDO.    
DEFINE VARIABLE iCurrentAddonNumber   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTaxRate              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTaxRateFreight       AS DECIMAL   NO-UNDO.
    
/* Invoice Address Variables */
DEFINE VARIABLE cN1Code               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAddressOrder         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCode                 LIKE ar-inv.sold-id NO-UNDO.
DEFINE VARIABLE cName                 LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE cAddress1             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAddress2             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCity                 LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE cState                LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE cZip                  LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE cCountry              LIKE shipto.country NO-UNDO.

DEFINE VARIABLE cWhsCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyPerPack           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPurchaseUnit         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestDataType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClientID             AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE iIndex                AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFirst                AS LOGICAL   NO-UNDO.

DEFINE VARIABLE gcCXMLIdentity        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLDeploymentMode  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLShipToPrefix    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLIdentityCust    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLPayloadID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLTimeStamp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCXMLSharedSecret    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cSuffix    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtInvDate  AS DATE      NO-UNDO.
DEFINE VARIABLE hdXMLProcs AS HANDLE    NO-UNDO.
    
DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail. 
DEFINE BUFFER bf-APIOutboundDetail3 FOR APIOutBoundDetail.
DEFINE BUFFER bf-APIOutboundDetail4 FOR APIOutBoundDetail.
DEFINE BUFFER bf-reftable1          FOR reftable.
DEFINE BUFFER bf-reftable2          FOR reftable.
DEFINE BUFFER bf-ar-invl            FOR ar-invl.
  
RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdXMLProcs).
   
DEFINE TEMP-TABLE ttAddons LIKE edivAddon.
    
DEFINE TEMP-TABLE ttN1Address
    FIELD N1Code       AS CHARACTER 
    FIELD AddressOrder AS INTEGER   
    FIELD AddressCode  LIKE ar-inv.sold-id
    FIELD AddressName  LIKE shipto.ship-name 
    FIELD Address1     AS CHARACTER 
    FIELD Address2     AS CHARACTER 
    FIELD City         LIKE shipto.ship-city 
    FIELD State        LIKE shipto.ship-state 
    FIELD Zip          LIKE shipto.ship-zip 
    FIELD Country      LIKE shipto.country 
    INDEX iOrder AddressOrder.
        
DEFINE TEMP-TABLE ttLines
    FIELD rLineDetailRow           AS ROWID 
    FIELD iLine                    AS INTEGER
    FIELD invoiceID                AS INTEGER   
    FIELD company                  AS CHARACTER 
    FIELD lineNo                   AS INTEGER   
    FIELD quantity                 AS INTEGER   
    FIELD quantityUOM              AS CHARACTER 
    FIELD quantityInvoiced         AS INTEGER   
    FIELD quantityInvoicedUOM      AS CHARACTER 
    FIELD quantityOrderOriginal    AS INTEGER   
    FIELD quantityOrderOriginalUOM AS CHARACTER 
    FIELD pricePerUOM              AS DECIMAL  
    FIELD pricePerEA               AS DECIMAL 
    FIELD priceUOM                 AS CHARACTER 
    FIELD priceTotal               AS DECIMAL   
    FIELD customerPartID           AS CHARACTER 
    FIELD itemID                   AS CHARACTER 
    FIELD itemName                 AS CHARACTER 
    FIELD amountTax                AS DECIMAL   
    FIELD amountTaxExFreightTax    AS DECIMAL   
    FIELD amountFreightTax         AS DECIMAL   
    FIELD amountTaxable            AS DECIMAL   
    FIELD amountTaxableExFreight   AS DECIMAL   
    FIELD amountTaxableFreight     AS DECIMAL   
    FIELD taxRate                  AS DECIMAL   
    FIELD amountFreight            AS DECIMAL   
    FIELD taxable                  AS LOGICAL   
    FIELD taxGroup                 AS CHARACTER 
    FIELD orderID                  AS INTEGER   
    FIELD orderLine                AS INTEGER   
    FIELD taxRateFreight           AS DECIMAL   
    FIELD customerPONo             AS CHARACTER           
    .
    
DEFINE TEMP-TABLE ttInv NO-UNDO 
    FIELD invoiceID                   AS INTEGER
    FIELD invoiceIDString             AS CHARACTER
    FIELD deploymentMode              AS CHARACTER
    FIELD company                     AS CHARACTER 
    FIELD invoiceDate                 AS DATE
    FIELD invoiceDateString           AS CHARACTER
    FIELD customerID                  AS CHARACTER
    FIELD customerEmail               AS CHARACTER
    FIELD customerName                AS CHARACTER
    FIELD customerAddress1            AS CHARACTER
    FIELD customerAddress2            AS CHARACTER 
    FIELD customerCity                AS CHARACTER 
    FIELD customerState               AS CHARACTER 
    FIELD customerPostalCode          AS CHARACTER 
    FIELD shiptoID                    AS CHARACTER
    FIELD shiptoName                  AS CHARACTER
    FIELD shiptoAddress1              AS CHARACTER
    FIELD shiptoAddress2              AS CHARACTER 
    FIELD shiptoCity                  AS CHARACTER 
    FIELD shiptoState                 AS CHARACTER 
    FIELD shiptoPostalCode            AS CHARACTER 
    FIELD termsDays                   AS INTEGER
    FIELD customerPO                  AS CHARACTER
    FIELD payloadID                   AS CHARACTER
    FIELD amountTotalLines            AS DECIMAL 
    FIELD amountTotalTax              AS DECIMAL 
    FIELD amountTotalTaxable          AS DECIMAL 
    FIELD amountTotalFreight          AS DECIMAL
    FIELD amountTotalTaxableFreight   AS DECIMAL 
    FIELD amountTotalTaxFreight       AS DECIMAL 
    FIELD amountTotalTaxExFreight     AS DECIMAL
    FIELD amountTotalTaxableExFreight AS DECIMAL
    FIELD amountTotal                 AS DECIMAL 
    FIELD taxGroup                    AS CHARACTER
    FIELD billFreight                 AS LOGICAL
    FIELD frtTaxRate                  AS DECIMAL
    .
  
/* ************************  Function Prototypes ********************** */

FUNCTION pGetPayloadID RETURNS CHARACTER PRIVATE
    (  ) FORWARD.          
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
ELSE 
DO:    
        
    FIND FIRST APIOutbound NO-LOCK
        WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID
        NO-ERROR.
    IF NOT AVAILABLE APIOutbound THEN 
    DO:
        ASSIGN
            opcMessage = "No APIOutbound record found"
            oplSuccess = FALSE
            .
        RETURN.        
    END. 
        
    FIND FIRST APIOutboundDetail NO-LOCK
        WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
        AND APIOutboundDetail.detailID      EQ "detail"
        AND APIOutboundDetail.parentID      EQ "SendInvoice"
        NO-ERROR.
   
    FIND FIRST bf-APIOutboundDetail1 NO-LOCK
        WHERE bf-APIOutboundDetail1.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail1.detailID      EQ "Tax"
        AND bf-APIOutboundDetail1.parentID      EQ "SendInvoice"
        NO-ERROR.
                     
    FIND FIRST bf-APIOutboundDetail2 NO-LOCK
        WHERE bf-APIOutboundDetail2.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail2.detailID      EQ "N1Addresses"
        AND bf-APIOutboundDetail2.parentID      EQ "SendInvoice"
        NO-ERROR.
             
    FIND FIRST bf-APIOutboundDetail3 NO-LOCK
        WHERE bf-APIOutboundDetail3.apiOutboundID EQ ipiAPIOutboundID
    AND bf-APIOutboundDetail3.detailID      EQ "Addons"
        AND bf-APIOutboundDetail3.parentID      EQ "SendInvoice"
        NO-ERROR.
             
    FIND FIRST bf-APIOutboundDetail4 NO-LOCK
        WHERE bf-APIOutboundDetail4.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail4.detailID      EQ "N3Address2"
        AND bf-APIOutboundDetail4.parentID      EQ "N1Addresses"
        NO-ERROR.             
             
    FIND FIRST ttArgs
        WHERE ttArgs.argType  EQ "ROWID"
        AND ttArgs.argKey   EQ "inv-head"
        NO-ERROR.
    IF NOT AVAIL ttArgs THEN 
        FIND FIRST ttArgs
            WHERE ttArgs.argType  EQ "ROWID"
            AND ttArgs.argKey   EQ "ar-inv"
            NO-ERROR.
                          
    IF NOT AVAILABLE ttArgs THEN 
    DO:
        ASSIGN
            opcMessage = "No valid inv-head or ar-inv record passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
        
    FIND FIRST inv-head NO-LOCK
        WHERE ROWID(inv-head) EQ TO-ROWID(ttArgs.argValue)
        NO-ERROR.
    IF NOT AVAIL inv-head THEN 
        FIND FIRST ar-inv NO-LOCK
            WHERE ROWID(ar-inv) EQ TO-ROWID(ttArgs.argValue)
            NO-ERROR.        
    IF NOT AVAILABLE inv-head AND NOT AVAIL ar-inv THEN 
    DO:
        ASSIGN
            opcMessage = "Invalid inv-head or ar-inv ROWID passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    FIND FIRST ttArgs
         WHERE ttArgs.argkey EQ "Suffix"
         NO-ERROR.
    IF AVAILABLE ttArgs THEN 
        cSuffix = ttArgs.argValue.
        
    FIND FIRST ttArgs
         WHERE ttArgs.argkey EQ "InvoiceDate"
         NO-ERROR.        
    IF AVAILABLE ttArgs THEN 
        dtInvDate = IF ttArgs.argValue EQ ? THEN TODAY ELSE DATE(ttArgs.argValue).
                
    IF AVAILABLE inv-head THEN 
    DO:
        IF NOT CAN-FIND(FIRST inv-line
            WHERE inv-line.r-no  EQ inv-head.r-no) THEN 
        DO:
            ASSIGN
                opcMessage = "No inv-line records available for invoice [ " + STRING(inv-head.inv-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    END. 
    ELSE 
    DO:
        IF NOT CAN-FIND(FIRST ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no) THEN 
        DO:
            ASSIGN
                opcMessage = "No ar-invl records available for invoice [ " + STRING(ar-inv.inv-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.            
    END.
        
    ASSIGN
        cClientID        = APIOutbound.clientID
        cRequestDataType = APIOutbound.requestDataType 
        .
    EMPTY TEMP-TABLE ttinv.
        
    IF AVAIL inv-head THEN 
    DO:
            
        ASSIGN 
            cCompany        = inv-head.company
            cInvoiceDueDate = STRING(DYNAMIC-FUNCTION("GetInvDueDate", inv-head.inv-date, inv-head.company ,inv-head.terms))
            cInvoiceType    = IF inv-head.t-inv-rev LT 0 THEN "CR" ELSE ""
            .
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ inv-head.company
            AND cust.cust-no EQ inv-head.cust-no
            NO-ERROR.
        IF AVAILABLE cust AND cust.country GT "" THEN 
            cCustCountry = cust.country.
        ELSE 
            cCustCountry = "US".
              
        FIND FIRST shipto NO-LOCK WHERE shipto.company EQ inv-head.company
            AND shipto.cust-no EQ inv-head.cust-no
            AND shipto.ship-id EQ IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to
            NO-ERROR.
        /*
        IF AVAILABLE shipto THEN 
                RUN pCreateAddress("ST", 4, IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to,
                                    shipto.ship-name, shipto.ship-addr[1], shipto.ship-addr[2], 
                                    shipto.ship-city, shipto.ship-state,
                                    shipto.ship-zip, shipto.country).
        ELSE */ 
        DO:
            RUN pCreateAddress("ST", 4, IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to,
                inv-head.sold-name, inv-head.sold-addr[1], inv-head.sold-addr[2], inv-head.sold-city,
                inv-head.sold-state,inv-head.sold-zip, cCustCountry ).
        END.    
        RUN pGetSettings(  
            INPUT inv-head.company,
            INPUT inv-head.cust-no,
            INPUT ""
            ).                                                           
        ASSIGN                 
            cInvoiceNumber = STRING(inv-head.inv-no)
            cInvoiceDate   = STRING(inv-head.inv-date)
            cTotalAmount   = TRIM(IF inv-head.t-inv-rev GT 0 THEN STRING(inv-head.t-inv-rev, ">>>>>>>>.99") ELSE "0")
            cTotalAmount   = REPLACE(ctotalAmount, ".", "")
            dtInvoiceDate  = inv-head.inv-date
            cShipToCode    = IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to.
        .
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceDate        = dtInvDate 
            ttInv.invoiceID          = inv-head.inv-no
            ttInv.customerID         = inv-head.cust-no
            ttInv.customerName       = inv-head.cust-name
            ttInv.customerAddress1   = inv-head.addr[1]
            ttInv.customerAddress2   = inv-head.addr[2]
            ttInv.customerCity       = inv-head.city
            ttInv.customerState      = inv-head.state
            ttInv.customerPostalCode = inv-head.zip
            ttInv.company            = inv-head.company
            ttInv.taxGroup           = inv-head.tax-gr
            ttInv.amountTotal        = inv-head.t-inv-rev
            ttInv.billFreight        = inv-head.f-bill
            ttInv.amountTotalFreight = IF ttInv.billFreight THEN inv-head.t-inv-freight ELSE 0
            ttInv.amountTotalTax     = inv-head.t-inv-tax
            .   
 
        RUN pAssignCommonHeaderData(
            BUFFER ttInv, 
            INPUT inv-head.company, 
            INPUT inv-head.cust-no, 
            INPUT inv-head.sold-no,
            INPUT inv-head.terms
            ).
        RUN pCreateAddress("BT", 1, inv-head.bill-to, inv-head.cust-name, inv-head.addr[1], 
            inv-head.addr[2], inv-head.city, inv-head.state, inv-head.zip, 
            cCustCountry).
        /* Fetch invoice notes from notes table */    
        FOR EACH notes NO-LOCK
            WHERE notes.rec_key EQ inv-head.rec_key:
            cInvNotes = cInvNotes + STRING(notes.note_text).
        END.
    END. /* end using inv-head */
    ELSE 
    DO: /* Using ar-inv */
        ASSIGN 
            cCompany        = ar-inv.company
            cInvoiceDueDate = STRING(ar-inv.due-date)
            .
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ar-inv.company
            AND cust.cust-no EQ ar-inv.cust-no
            NO-ERROR.
        IF AVAILABLE cust AND cust.country GT "" THEN 
            cCustCountry = cust.country.
        ELSE 
            cCustCountry = "US".
        /*
      FIND FIRST shipto NO-LOCK WHERE shipto.company EQ ar-inv.company
          AND shipto.cust-no EQ ar-inv.cust-no
          AND shipto.ship-id EQ IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.ship-id
          NO-ERROR.
      IF AVAILABLE shipto THEN 
              RUN pCreateAddress("ST", 4, IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.bill-to,
                                  shipto.ship-name, shipto.ship-addr[1], shipto.ship-addr[2], 
                                  shipto.ship-city, shipto.ship-state,
                                  shipto.ship-zip, shipto.country).
      ELSE */ 
        DO:
            RUN pCreateAddress("ST", 4, IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.bill-to,
                ar-inv.sold-name, ar-inv.sold-addr[1], ar-inv.sold-addr[2], ar-inv.sold-city,
                ar-inv.sold-state,ar-inv.sold-zip, cCustCountry ).
        END.    
         
        dLineTotalAmt = 0.
        FOR EACH bf-ar-invl NO-LOCK  
            WHERE bf-ar-invl.x-no = ar-inv.x-no
            AND (bf-ar-invl.inv-qty NE 0 OR bf-ar-invl.misc) :
            dLineTotalAmt = dLineTotalAmt + bf-ar-invl.amt.
        END.
        ASSIGN 
            cInvoiceNumber   = STRING(ar-inv.inv-no)
            dInvoiceTotalAmt = dLineTotalAmt + ar-inv.tax-amt + (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
            cTotalAmount     = TRIM(IF dInvoiceTotalAmt GT 0 THEN STRING(dInvoiceTotalAmt, ">>>>>>>>.99") ELSE "0")
            cTotalAmount     = REPLACE(ctotalAmount, ".", "")
            dtInvoiceDate    = ar-inv.inv-date
            cInvoiceType     = IF dInvoiceTotalAmt LT 0 THEN "CR" ELSE ""
            .
        RUN pGetSettings(
            INPUT ar-inv.company,
            INPUT ar-inv.cust-no,
            INPUT ar-inv.ship-id
            ).
             
        CREATE ttInv.
        ASSIGN             
            ttInv.invoiceDate        = dtInvDate 
            ttInv.invoiceID          = ar-inv.inv-no
            ttInv.customerID         = ar-inv.cust-no
            ttInv.customerName       = ar-inv.cust-name
            ttInv.customerAddress1   = ar-inv.addr[1]
            ttInv.customerAddress2   = ar-inv.addr[2]
            ttInv.customerCity       = ar-inv.city
            ttInv.customerState      = ar-inv.state
            ttInv.customerPostalCode = ar-inv.zip
            ttInv.company            = ar-inv.company
            ttInv.taxGroup           = ar-inv.tax-code
            ttInv.amountTotal        = ar-inv.t-sales 
            ttInv.billFreight        = ar-inv.f-bill
            ttInv.amountTotalFreight = IF ttInv.billFreight THEN ar-inv.freight ELSE 0
            ttInv.amountTotalTax     = ar-inv.tax-amt
            . 
        RUN pAssignCommonHeaderData(
            BUFFER ttInv, 
            INPUT ar-inv.company, 
            INPUT ar-inv.cust-no, 
            INPUT ar-inv.ship-id,
            INPUT ar-inv.terms
            ).                
        /*
        IF AVAIL cust THEN 
            RUN pCreateAddress("BT", 1, cust.cust-no, cust.name, cust.addr[1], 
                   cust.addr[2], cust.city, cust.state, cust.zip, 
                   cCustCountry).
        ELSE
        */         
        /* Testing */       
        FIND FIRST shipto NO-LOCK WHERE shipto.company EQ ar-inv.company
            AND shipto.cust-no EQ ar-inv.cust-no
            AND shipto.ship-id EQ ar-inv.cust-no
            NO-ERROR.
        RUN pCreateAddress("BT", 1, ar-inv.bill-to, IF AVAIL shipto THEN shipto.ship-name ELSE ar-inv.cust-name, 
            IF AVAIL shipto THEN shipto.ship-addr[1] ELSE ar-inv.addr[1], 
            IF AVAIL shipto THEN shipto.ship-addr[2] ELSE ar-inv.addr[2], 
            IF AVAIL shipto THEN shipto.ship-city ELSE ar-inv.city, 
            IF AVAIL shipto THEN shipto.ship-state ELSE ar-inv.state, 
            IF AVAIL shipto THEN shipto.ship-zip ELSE ar-inv.zip, 
            cCustCountry).            
        /* Fetch invoice notes from notes table */    
        FOR EACH notes NO-LOCK
            WHERE notes.rec_key EQ ar-inv.rec_key:
            cInvNotes = cInvNotes + STRING(notes.note_text).
        END.
    END.

    ASSIGN
        cInvoiceDate = STRING(dtInvoiceDate).
    cCurrentDate = STRING(TODAY).
    cCurrentTime = STRING(TIME)
        .

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ar-inv.company
         NO-ERROR.
         
        RUN pCreateAddress(
            INPUT "RI",
            INPUT 2,
            INPUT "0000",
            INPUT IF AVAILABLE company THEN company.name    ELSE "",
            INPUT IF AVAILABLE company THEN company.addr[1] ELSE "",
            INPUT IF AVAILABLE company THEN company.addr[2] ELSE "",
            INPUT IF AVAILABLE company THEN company.city    ELSE "",
            INPUT IF AVAILABLE company THEN company.state   ELSE "",
            INPUT IF AVAILABLE company THEN company.zip     ELSE "",
            INPUT "US"
            ).
        RUN pCreateAddress(
            INPUT "PE",
            INPUT 3,
            INPUT "0000",
            INPUT IF AVAILABLE company THEN company.name    ELSE "",
            INPUT IF AVAILABLE company THEN company.addr[1] ELSE "",
            INPUT IF AVAILABLE company THEN company.addr[2] ELSE "",
            INPUT IF AVAILABLE company THEN company.city    ELSE "",
            INPUT IF AVAILABLE company THEN company.state   ELSE "",
            INPUT IF AVAILABLE company THEN company.zip     ELSE "",
            INPUT "US"
            ).                
    // RUN pCreateAddress('RI', 2, '0000','PREMIER PACKAGING', '3254 RELIABLE PARKWAY', '', 'CHICAGO',
    //    'IL','60686', 'US' ).
                               
    /* Fetch Address Details for the invoice */
    IF AVAILABLE(bf-APIOutboundDetail2) THEN 
    DO: 
        ASSIGN 
            lcBillToStreetData = STRING(bf-APIOutboundDetail2.data)
            lcShipToStreetData = STRING(bf-APIOutboundDetail2.data)
            .
        RUN updateRequestData(INPUT-OUTPUT lcBillToStreetData, "Street",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerAddress1)).
        RUN updateRequestData(INPUT-OUTPUT lcShipToStreetData, "Street",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.ShiptoAddress1)).
        
        ASSIGN 
            lcConcatBillToStreetData =  lcBillToStreetData
            lcConcatShiptoStreetData =  lcShipToStreetData.
            .
        IF ttInv.customerAddress2 NE "" THEN DO:
            lcBillToStreetData = STRING(bf-APIOutboundDetail2.data).
            RUN updateRequestData(INPUT-OUTPUT lcBillToStreetData, "Street",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerAddress2)).
            lcConcatBillToStreetData   = lcConcatBillToStreetData + " " + lcBillToStreetData.
        END.
        IF ttInv.ShiptoAddress2 NE "" AND ttInv.shiptoAddress2 NE '345 Court Street' THEN DO:
            lcShipToStreetData = STRING(bf-APIOutboundDetail2.data).
            RUN updateRequestData(INPUT-OUTPUT lcShipToStreetData, "Street",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.ShiptoAddress2)).
            lcConcatShipToStreetData   = lcConcatShipToStreetData + " " + lcShipToStreetData.
        END.
        FOR EACH ttN1Address                                 
            :          
            /* Address section has 3 lines per iteration */        
            ASSIGN  
                lcAddressData = STRING(bf-APIOutboundDetail2.data)
                cN1code       = ttN1Address.N1Code      
                cCode         = ttN1Address.addressCode 
                cName         = ttN1Address.addressName 
                cAddress1     = ttN1Address.address1    
                cAddress2     = ttN1Address.address2   
                cState        = ttN1Address.state 
                cCity         = ttN1Address.city        
                cZip          = ttN1Address.zip         
                cCountry      = ttN1Address.country
                .    
            IF cAddress1 EQ "" AND cAddress2 GT "" THEN 
                ASSIGN cAddress1 = cAddress2
                    cAddress2 = ""
                    . 
            IF AVAILABLE bf-APIOutboundDetail4 THEN 
                lcAddress2Data = STRING(bf-APIOutboundDetail4.data).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N1Qual", cN1Code).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N1Name", cName).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N3Address1", cAddress1).                       
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4City", cCity).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4State", cState).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4Zip", cZip).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4Country", cCountry).
                
            IF cAddress2 NE "" THEN 
                RUN updateRequestData(INPUT-OUTPUT lcAddress2Data, "N3Address2", cAddress2).
            ELSE 
                lcAddress2Data = "".
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N3Address2nd", lcAddress2Data). 
            lcConcatAddressData = lcConcatAddressData + "" + lcAddressData.
        END.
    END.
    /* Fetch line details for the Invoice */         
    IF AVAIL inv-head THEN 
    DO:
            
        ASSIGN 
            lcConcatLineData = ""
            lFirst           = YES
            .
        EMPTY TEMP-TABLE ttLines.
        FOR EACH inv-line
            WHERE inv-line.r-no   EQ inv-head.r-no
            AND inv-line.inv-qty GT 0
            BY inv-line.line:    
                
            /* find order line for line number */
            FIND FIRST oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ inv-line.company
                AND oe-ordl.ord-no  EQ inv-line.ord-no 
                AND oe-ordl.i-no EQ inv-line.i-no
                NO-ERROR.
            IF NOT AVAIL oe-ordl THEN                 
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ inv-line.company
                    AND oe-ordl.po-no EQ inv-line.po-no
                    AND oe-ordl.i-no EQ inv-line.i-no 
                    USE-INDEX po-no
                    NO-ERROR.
            ASSIGN 
                iLineCounter = iLineCounter + 1.
            CREATE ttLines.
            ASSIGN 
                ttLines.rLineDetailRow         = ROWID(inv-line)
                ttLines.iLine                  = IF AVAILABLE oe-ordl THEN oe-ordl.line ELSE iLineCounter
                ttLines.invoiceID              = ttInv.invoiceID
                ttLines.company                = ttInv.company
                ttLines.lineNo                 = inv-line.line
                ttLines.orderID                = inv-line.ord-no
                ttLines.orderLine              = inv-line.line
                ttLines.quantityInvoiced       = inv-line.inv-qty
                ttLines.quantityInvoicedUOM    = "EA"
                ttLines.pricePerUOM            = inv-line.price * (1 - (inv-line.disc / 100))
                ttLines.priceUOM               = inv-line.pr-uom
                ttLines.customerPartID         = inv-line.part-no
                ttLines.itemID                 = inv-line.i-no
                ttLines.itemName               = inv-line.i-name
                ttLines.priceTotal             = inv-line.t-price
                ttLines.taxable                = inv-line.tax
                ttLines.amountTaxableExFreight = ttLines.priceTotal
                ttLines.amountTaxableFreight   = inv-line.t-freight
                ttLines.amountFreight          = inv-line.t-freight
                ttLines.customerPONo           = inv-line.po-no
                ttInv.amountTotalLines         = ttInv.amountTotalLines + inv-line.t-price
                . 
            IF ttLines.priceUOM NE "EA" OR ttLines.priceUOM NE "" THEN 
                RUN pConvertUnitPrice(
                    INPUT  ttLines.company,
                    INPUT  ttLines.itemID,
                    INPUT  ttLines.pricePerUOM,
                    INPUT  ttLines.priceUOM,
                    INPUT  "EA",
                    OUTPUT ttLines.pricePerEA 
                    ).    
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ inv-line.company
                   AND oe-ordl.i-no    EQ inv-line.i-no
                   AND oe-ordl.ord-no  EQ inv-line.ord-no
                 NO-ERROR.

            IF AVAILABLE oe-ordl THEN 
                ttLines.orderLine = oe-ordl.line.
            
            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttLines
                ).                                  
        END.             
        
        FOR EACH ttLines,
            FIRST inv-line
            WHERE ROWID(inv-line) EQ ttLines.rLineDetailRow
            BY ttLines.iLine:    
                
            ASSIGN 
                iLineCounter = iLineCounter + 1
                iBolNum      = INTEGER(inv-line.b-no)               
                dQtyShipped  = (IF inv-line.inv-qty NE 0 OR iBolNum GT 0 THEN inv-line.inv-qty ELSE 1)
                dUnitPrice   = inv-line.price
                cCustPart    = inv-line.part-no
                cUomCode     = (IF inv-line.pr-qty-uom > "" THEN
                        inv-line.pr-qty-uom
                        ELSE "EA"
                        )                     
                iLineCount = iLineCount + 1
                .
            IF cUomCode NE inv-line.pr-uom THEN 
                RUN pConvQtyPriceUOM ( 
                    INPUT inv-head.company,
                    INPUT inv-line.i-no,
                    INPUT inv-line.pr-uom,
                    INPUT cUomCode,
                    INPUT-OUTPUT dUnitPrice
                    ). 
            cLineTotal =  STRING(dUnitPrice * dQtyShipped).
            
            IF AVAILABLE APIOutboundDetail THEN DO:                    
                /* Line number from inbound 850 if available, otherwise incremented */
                ASSIGN
                    lcLineData     = APIOutboundDetail.data
                    cItemLineNum   = STRING(ttLInes.iLine)                     
                    cItemQty       = STRING(dQtyShipped)
                    cItemUom       = string(cUomCode)
                    cItemPrice     = STRING(dUnitPrice)
                    cItemID        = STRING(inv-line.i-no)
                    cItemBuyerPart = STRING(TRIM(inv-line.part-no))
                    cItemDesc      = STRING(inv-line.i-name)
                    cPoNum         = STRING(inv-line.po-no)
                    cPartQualifier = STRING(IF cItemBuyerPart NE "" THEN "BP" ELSE "")
                    .
                RUN pUpdateLineRequestData(
                    BUFFER ttLines,
                    INPUT-OUTPUT lcLineData,
                    INPUT-OUTPUT lFirst
                    ).
                   
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLineNum", cItemLineNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemQty", cItemQty).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemUOM", cItemUom).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PartQualifier", cPartQualifier).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "BuyerPart", cItemBuyerPart).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDescription", cItemDesc).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoNum", cPoNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "LineTotal",cLineTotal).
               
                    // RUN updateRequestData(INPUT-OUTPUT lcLineData, "linefeed", "~n").  
                lcConcatLineData = lcConcatLineData + "" + lcLineData. 
            END. /* If available APIOutboundDetail */
        END. /* Each tt lines */
      
    END. /* Process lines if using inv-head */
    ELSE 
    DO:
        /* Process lines if using ar-inv */
        ASSIGN 
            lcConcatLineData = ""
            lFirst           = YES
            .

        EMPTY TEMP-TABLE ttLines.
        FOR EACH ar-invl
            WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.inv-qty GT 0
            BY ar-invl.line:    
                
            /* find order line for line number */
            FIND FIRST oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ ar-invl.company
                AND oe-ordl.ord-no  EQ ar-invl.ord-no 
                AND oe-ordl.i-no EQ ar-invl.i-no
                NO-ERROR.
            IF NOT AVAIL oe-ordl THEN                 
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ ar-inv.company
                    AND oe-ordl.po-no EQ ar-invl.po-no
                    AND oe-ordl.i-no EQ ar-invl.i-no 
                    USE-INDEX po-no
                    NO-ERROR.
            ASSIGN 
                iLineCounter = iLineCounter + 1.
            CREATE ttLines.
            ASSIGN 
                ttLines.rLineDetailRow         = ROWID(ar-invl)
                ttLines.iLine                  = IF AVAILABLE oe-ordl THEN oe-ordl.line ELSE iLineCounter
                ttLines.invoiceID              = ttInv.invoiceID
                ttLines.company                = ttInv.company
                ttLines.lineNo                 = ar-invl.line
                ttLines.orderID                = ar-invl.ord-no
                ttLines.orderLine              = ar-invl.ord-line
                ttLines.quantityInvoiced       = ar-invl.inv-qty
                ttLines.quantityInvoicedUOM    = "EA"
                ttLines.pricePerUOM            = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                ttLines.priceUOM               = ar-invl.pr-uom
                ttLines.customerPartID         = ar-invl.part-no
                ttLines.itemID                 = ar-invl.i-no
                ttLines.itemName               = ar-invl.i-name
                ttLines.priceTotal             = ar-invl.amt
                ttLines.taxable                = ar-invl.tax
                ttLines.amountTaxableExFreight = ttLines.priceTotal
                ttLines.amountTaxableFreight   = ar-invl.t-freight
                ttLines.amountFreight          = ar-invl.t-freight
                ttLines.customerPONo           = ar-invl.po-no
                ttInv.amountTotalLines         = ttInv.amountTotalLines + ar-invl.amt
                .
            IF ttLines.priceUOM NE "EA" OR ttLines.priceUOM NE "" THEN 
                RUN pConvertUnitPrice(
                    INPUT  ttLines.company,
                    INPUT  ttLines.itemID,
                    INPUT  ttLines.pricePerUOM,
                    INPUT  ttLines.priceUOM,
                    INPUT  "EA",
                    OUTPUT ttLines.pricePerEA 
                    ). 
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ ar-invl.company
                   AND oe-ordl.i-no    EQ ar-invl.i-no
                   AND oe-ordl.ord-no  EQ ar-invl.ord-no
                 NO-ERROR.

            IF AVAILABLE oe-ordl THEN 
                ttLines.orderLine = oe-ordl.line.
            
            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttLines
                ). 

            ttInv.amountTotal = ttInv.amountTotalLines + ttInv.amountTotalTax + ttInv.amountTotalFreight.                              
        END.            
        FOR EACH ttLines,
            FIRST ar-invl
            WHERE ROWID(ar-invl) EQ ttLines.rLineDetailrow                    
            BY ttLines.iLine:    
                
            ASSIGN 
                iLineCounter = iLineCounter + 1
                iBolNum      = INTEGER(ar-invl.b-no)               
                cUomCode     = (IF ar-invl.pr-qty-uom > "" THEN
                                ar-invl.pr-qty-uom
                                ELSE "EA")
                dQtyShipped  = (IF ar-invl.inv-qty NE 0 OR iBolNum GT 0 THEN ar-invl.inv-qty ELSE 1)
                dUnitPrice   = ar-invl.unit-pr
                cCustPart    = ar-invl.part-no
                      // edivline.unit-price       = ar-invl.unit-pr
                cUomCode     = (IF ar-invl.pr-qty-uom > "" THEN
                        ar-invl.pr-qty-uom
                        ELSE "EA"
                        )                               
                iLineCount = iLineCount + 1
                .
            IF cUomCode NE ar-invl.pr-uom THEN  
                RUN pConvQtyPriceUOM ( 
                    INPUT ar-inv.company,
                    INPUT ar-invl.i-no,
                    INPUT ar-invl.pr-uom,
                    INPUT cUomCode,
                    INPUT-OUTPUT dQtyShipped
                    ). 
            cLineTotal =  STRING(dUnitPrice * dQtyShipped).                                                  
            IF AVAILABLE APIOutboundDetail THEN DO:
                /* Line number from inbound 850 if available, otherwise incremented */
                ASSIGN
                    lcLineData     = APIOutboundDetail.data
                    cItemLineNum   = STRING(ttLines.iLine)                     
                    cItemQty       = STRING(dQtyShipped)
                    cItemUom       = string(cUomCode)
                    cItemPrice     = STRING(dUnitPrice)
                    cItemID        = STRING(ar-invl.i-no)
                    cItemBuyerPart = STRING(ar-invl.part-no)
                    cItemDesc      = STRING(ar-invl.i-name)
                    cPoNum         = STRING(ar-invl.po-no)
                    cPartQualifier = STRING(IF cItemBuyerPart NE "" THEN "BP" ELSE "")
                    .
                IF NOT ar-invl.misc THEN      
                    RUN pUpdateLineRequestData(
                        BUFFER ttLines,
                        INPUT-OUTPUT lcLineData,
                        INPUT-OUTPUT lFirst
                        ).
                        
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLineNum", cItemLineNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemQty", cItemQty).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemUOM", cItemUom).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PartQualifier", cPartQualifier).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoNum", cPoNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "BuyerPart", cItemBuyerPart).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDescription", cItemDesc). 
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "LineTotal",cLineTotal).
                    // RUN updateRequestData(INPUT-OUTPUT lcLineData, "linefeed", "~n").
                    
                lcConcatLineData = lcConcatLineData + "" + lcLineData.  
            END. /* If available APIOutboundDetail */
        END.                 
    END.
        
    /* Added charges section, i.e. Freight, Tax, Etc */
    IF AVAIL inv-head THEN 
    DO:
        FOR EACH inv-misc OF inv-head NO-LOCK:
            CREATE ttLines.
            ASSIGN 
                ttLines.invoiceID              = ttInv.invoiceID
                ttLines.company                = ttInv.company
                ttLines.lineNo                 = inv-misc.line
                ttLines.orderID                = inv-misc.ord-no
                ttLines.orderLine              = inv-misc.line
                ttLines.priceTotal             = inv-misc.amt
                ttLines.taxable                = inv-misc.tax
                ttLines.amountTaxableExFreight = ttLines.priceTotal
                ttLines.customerPONo           = inv-misc.po-no
                ttInv.amountTotalLines         = ttInv.amountTotalLines + inv-misc.amt
                .  
            RUN pAssignCommonLineData(
                BUFFER ttInv, 
                BUFFER ttLines
                ). 
                                        
            IF NOT inv-misc.deleted  THEN 
                RUN pCreateAddonRecord (
                    RECID(inv-head), 
                    inv-misc.inv-line,      
                    inv-misc.charge,
                    inv-misc.Dscr,
                    inv-misc.amt,
                    0,             /* rate */
                    inv-misc.bill,
                    "ESTIMATE# " + inv-misc.est-no,
                    inv-head.cust-no,
                    iCurrentAddonNumber                    
                    ).
            
        END.    /* each inv-misc of inv-head */
        
        IF inv-head.t-inv-freight <> 0 AND inv-head.f-bill THEN 
        DO:
            RUN pCreateAddonRecord (
                RECID(inv-head), 
                0,          /* line # */
                "FRT",
                "FREIGHT",
                inv-head.t-inv-freight,
                0,            /* rate */
                "Y",
                "Invoice Level Freight Charge",
                inv-head.cust-no,
                iCurrentAddonNumber                    
                ).
        END.
        
        IF inv-head.t-inv-tax <> 0 THEN 
        DO:
            FIND stax-group
                WHERE stax-group.company = inv-head.company
                AND stax-group.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
            RUN ar/cctaxrt.p (INPUT inv-head.company, inv-head.tax-gr ,
                OUTPUT dTaxRate, OUTPUT dTaxRateFreight).   
            RUN pCreateAddonRecord (
                RECID(inv-head), 
                0,          /* line # */
                "TAX",
                (IF AVAILABLE stax-group THEN stax-group.tax-dscr ELSE "TAXES"),
                inv-head.t-inv-tax,
                dTaxRate,          /* rate */
                YES,
                "Invoice Level Taxes",
                inv-head.cust-no,
                iCurrentAddonNumber
                ).
        END.
    END.
    ELSE 
    DO:

        IF ar-inv.freight <> 0 AND ar-inv.f-bill THEN 
        DO:
            RUN pCreateAddonRecord (
                0 /* eddoc.rec */, 
                0,          /* line # */
                "FRT",
                "FREIGHT",
                ar-inv.freight,
                0,          /* rate */    
                "Y",
                "Invoice Level Freight Charge",
                ar-inv.cust-no,
                iCurrentAddonNumber
                ).
        END.
            
        IF ar-inv.tax-amt <> 0 THEN 
        DO:
            FIND stax-group NO-LOCK
                WHERE stax-group.company = ar-inv.company
                AND stax-group.tax-group = ar-inv.tax-code  
                NO-ERROR.
              
            RUN ar/cctaxrt.p (INPUT ar-inv.company, ar-inv.tax-code /* oe-ord.tax-gr */,
                OUTPUT dTaxRate, OUTPUT dTaxRateFreight).  
            RUN pCreateAddonRecord (
                0 /* eddoc.rec*/, 
                0,          /* line # */
                "TAX",
                (IF AVAILABLE stax-group THEN stax-group.tax-dscr ELSE "TAXES"),
                ar-inv.tax-amt,
                dTaxRate,          /* rate */ 
                "Y",
                "Invoice Level Taxes",
                ar-inv.cust-no,
                iCurrentAddonNumber
                ).
        END.
                
        /* Process the misc records that came from inv-misc */
        FOR EACH ar-invl 
            WHERE ar-invl.x-no = ar-inv.x-no
            AND  ar-invl.misc:
                    
                       
            RUN pCreateAddonRecord (
                0 /* eddoc.rec */, 
                0,      
                ar-invl.prep-charge,
                ar-invl.Dscr[1],
                ar-invl.amt,
                0,             /* rate */
                (IF ar-invl.billable THEN "Y" ELSE "N"),
                "ESTIMATE# " + ar-invl.est-no,
                ar-inv.cust-no,
                iCurrentAddonNumber
                ).            
        END.
    END.
        
    lcConcatLineAddonData = "".
    IF AVAILABLE bf-APIOutboundDetail3 THEN 
    DO:
        /* Fetch Addon details for the invoice addons */           
        FOR EACH ttAddons 
            WHERE ttAddons.Agency-code NE "TAX"                  
            :             

            ASSIGN
                lcLineAddonData     = STRING(bf-APIOutboundDetail3.data)
                cAddonAllowCharge   = "C"
                cMiscElem           = "D240"
                cSacAgencyQualifier = ""
                cSacAgencyCode      = ""
                cSacReferenceId     = STRING(ttAddons.Amount)
                .

            RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "AddonAllowCharge", cAddonAllowCharge).
            RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "AddonMiscElem", cMiscElem).
            RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "SacReferenceId", cSacReferenceId).
                // RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "linefeed", "~n").
            lcConcatLineAddonData = lcConcatLineAddonData + "" + lcLineAddonData.
        END.
                      
    END.
    lcConcatTaxData = "".
    IF AVAILABLE bf-APIOutboundDetail1 THEN 
    DO:
        FOR EACH ttAddons 
            WHERE ttAddons.Agency-code EQ "TAX"                  
            :

            ASSIGN     
                lcTaxData        = STRING(bf-APIOutboundDetail1.data)                    
                cAddonTaxType    = "ST"
                cTotalTaxDollars = STRING(ttAddons.amount)
                cTaxPct          = STRING(ttAddons.rate)
                .              
                
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", cAddonTaxType).
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", cTotalTaxDollars).
            RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPct", cTaxPct).
                // RUN updateRequestData(INPUT-OUTPUT lcTaxData, "linefeed", "~n").
            lcConcatTaxData = lcConcatTaxData + "" + lcTaxData.                    
                             
        END.  
    END.        
           
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrentDate", cCurrentDate ). 
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrentTime", cCurrentTime ).               
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDate", cInvoiceDate ). 
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceNum", cInvoiceNumber ). 
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Currency", "USD" ).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalAmount", cTotalAmount ).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", TRIM(ttInv.payloadID)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TimeStamp", gcCXMLTimeStamp ).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cXMLPayloadID", gcCXMLPayLoadID).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IdentityCust", gcCXMLIdentityCust).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Identity", gcCXMLIdentity).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SharedSecret", gcCXMLSharedSecret).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DeploymentMode", ttInv.deploymentMode).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDateString", ttInv.invoiceDateString).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceID", ttInv.invoiceIDString).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerName", DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerName)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerEmail", DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerEmai)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerCity", DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerCity)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerState",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerState)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerPostalCode",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.customerPostalcode)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoID", ttInv.shiptoID).    
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoName",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoName)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoCity", DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoCity)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoState",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoState)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShiptoPostalCode",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoPostalcode)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Terms", STRING(ttInv.termsDays)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OrderID", ttInv.customerPO).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", ttInv.payloadID).  
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SubtotalAmount", STRING(ttInv.amountTotalLines)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalTax", STRING(ttInv.amountTotalTax)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalSalesTaxableAmt", STRING(ttInv.amountTotalTaxableExFreight)). 
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalSalesTax", STRING(ttInv.amountTotalTaxExFreight)).  
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalShippingAmt", STRING(ttInv.amountTotalFreight)).      
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalGrossAmt", STRING(ttInv.amountTotal)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalNetAmt", STRING(ttInv.amountTotal)).         
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "invNotes", cInvNotes).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceDueDate", cInvoiceDueDate).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvoiceType", cInvoiceType).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LineCount",STRING(iLineCount)).
    
    /* If the previous section was not blank, it ended with CR so don't need to start with one */
    RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatLineData, cRequestDataType).
    RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatLineAddonData, cRequestDataType).
    RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatAddressData, cRequestDataType).
    RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatTaxData, cRequestDataType).        


    ioplcRequestData = REPLACE(ioplcRequestData, "$N1Addresses$", (IF lcConcatAddressData ne "" THEN "~n" ELSE "") + lcConcatAddressData).
          
    ioplcRequestData = REPLACE(ioplcRequestData, "$Detail$", (if lcConcatLineData ne "" THEN "~n" ELSE "") + lcConcatLineData).
                                                                 
    ioplcRequestData = REPLACE(ioplcRequestData, "$Addons$", (IF lcConcatLineAddonData ne "" THEN  "~n" ELSE "") + lcConcatLineAddonData).
        
    ioplcRequestData = REPLACE(ioplcRequestData, "$Tax$", (IF lcConcatTaxData ne "" THEN  "~n" ELSE "") + lcConcatTaxData).

    ioplcRequestData = REPLACE(ioplcRequestData, "$BillToStreetData$", lcConcatBillToStreetData).
    
    ioplcRequestData = REPLACE(ioplcRequestData, "$ShipToStreetData$", lcConcatShipToStreetData).
    ASSIGN 
        cFullDocument = ioplcRequestData
        dSELineCount = NUM-ENTRIES(cFullDocument, "~~") - 1   
        /* Subtract lines before ST and after SE segments */
        dSELineCount = dSELineCount - 4
        cSELineCount = STRING(dSELineCount)
        .
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", cSELineCount ).
    RELEASE bf-APIOutboundDetail1.
    RELEASE bf-APIOutboundDetail2.
    RELEASE bf-APIOutboundDetail3.
    
    IF VALID-HANDLE(hdXMLProcs) THEN DO:
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdXMLProcs).
        DELETE PROCEDURE hdXMLProcs. 
    END. 
    ASSIGN
        opcMessage = ""
        oplSuccess = TRUE
        .
END.
    
/* End of Main Code */
    
/* **********************  Internal Procedures  *********************** */


PROCEDURE pAssignCommonHeaderData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv FOR ttInv.
    
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTermsCode  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-company FOR company.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-shipto  FOR shipto.
    DEFINE BUFFER bf-terms   FOR terms.
    
    ASSIGN 
        ipbf-ttInv.deploymentMode    = gcCXMLDeploymentMode
        ipbf-ttInv.invoiceIDString   = STRING(ipbf-ttInv.invoiceID) + cSuffix
        ipbf-ttInv.invoiceDateString = STRING(YEAR(ipbf-ttInv.invoiceDate),'9999')
                                       + '-'
                                       + STRING(MONTH(ipbf-ttInv.invoiceDate),'99')
                                       + '-'
                                       + STRING(DAY(ipbf-ttInv.invoiceDate),'99')
                                       + 'T'
                                       + STRING(0,'hh:mm:ss')
                                       + '-05:00'
         .
                                       
    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipcCompany
           AND bf-cust.cust-no EQ ipcCustomerID
         NO-ERROR.
           
    IF AVAILABLE bf-cust THEN 
        ttInv.customerEmail = bf-cust.email.
                 
    FIND FIRST bf-shipto NO-LOCK 
         WHERE bf-shipto.company EQ ipcCompany
           AND bf-shipto.cust-no EQ ipcCustomerID
           AND bf-shipto.ship-id EQ ipcShipToID
         NO-ERROR.
    IF AVAILABLE bf-shipto THEN 
        ASSIGN 
            ipbf-ttInv.shipToID         = gcCXMLShipToPrefix + bf-shipto.ship-id
            ipbf-ttInv.shiptoName       = bf-shipto.ship-name
            ipbf-ttInv.shiptoAddress1   = bf-shipto.ship-addr[1]
            ipbf-ttInv.shiptoAddress2   = bf-shipto.ship-addr[2]
            ipbf-ttInv.shiptoCity       = bf-shipto.ship-city
            ipbf-ttInv.shiptoState      = bf-shipto.ship-state
            ipbf-ttInv.shiptoPostalCode = bf-shipto.ship-zip
            .

    FIND FIRST bf-terms NO-LOCK 
         WHERE bf-terms.company EQ ipcCompany
           AND bf-terms.t-code  EQ ipcTermsCode
        NO-ERROR.
    IF AVAILABLE bf-terms THEN 
        ipbf-ttInv.termsDays = bf-terms.net-days.

END PROCEDURE.

PROCEDURE pAssignCommonLineData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInv   FOR ttInv.
    DEFINE PARAMETER BUFFER ipbf-ttLines FOR ttLines.
    
    DEFINE           BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE           BUFFER bf-oe-ordl   FOR oe-ordl.
    
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    
    IF ipbf-ttInv.customerPO EQ "" THEN 
        ipbf-ttInv.customerPO = ipbf-ttLines.customerPO.
        
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipbf-ttInv.company
           AND bf-oe-ord.ord-no  EQ ipbf-ttLines.orderID
         NO-ERROR.
        
    IF AVAILABLE bf-oe-ord THEN DO:
        IF ipbf-ttInv.customerPO EQ "" THEN 
            ipbf-ttInv.customerPO = bf-oe-ord.po-no.  
                    
        IF ipbf-ttInv.payloadID EQ "" THEN 
            ipbf-ttInv.payloadID = bf-oe-ord.spare-char-3.
            
        FIND FIRST bf-oe-ordl NO-LOCK 
             WHERE bf-oe-ordl.company EQ bf-oe-ord.company
               AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
               AND bf-oe-ordl.i-no    EQ ipbf-ttLineS.itemID
               AND bf-oe-ordl.line    EQ ipbf-ttLineS.orderLine
             NO-ERROR.
            
        IF AVAILABLE bf-oe-ordl THEN DO: 
            ASSIGN 
                ipbf-ttLines.quantityOrderOriginal    = bf-oe-ordl.spare-dec-1
                ipbf-ttLines.quantityOrderOriginalUOM = bf-oe-ordl.spare-char-2
                .
            IF ipbf-ttLines.quantityOrderOriginalUOM NE ""
                AND ipbf-ttLines.quantityOrderOriginalUOM NE ipbf-ttLines.quantityInvoicedUOM THEN DO:
                     
                RUN Conv_QuantityFromUOMtoUOM(
                    INPUT  bf-oe-ordl.company,
                    INPUT  bf-oe-ordl.i-no,
                    INPUT  "FG", 
                    INPUT  ipbf-ttLines.quantityInvoiced,
                    INPUT  ipbf-ttLines.quantityInvoicedUOM,
                    INPUT  ipbf-ttLines.quantityOrderOriginalUOM, 
                    INPUT  0,
                    INPUT  0,
                    INPUT  0,
                    INPUT  0,
                    INPUT  bf-oe-ordl.cas-cnt, 
                    OUTPUT ipbf-ttLines.quantity,
                    OUTPUT lError,
                    OUTPUT cErrorMessage
                    ).
                IF ipbf-ttLines.quantity EQ 0 THEN 
                    ASSIGN 
                        ipbf-ttLines.quantity = ipbf-ttLines.quantityInvoiced.
                
                ipbf-ttLines.quantityUOM = ipbf-ttLines.quantityOrderOriginalUOM.
            END.    
        END.
    END.
    IF ipbf-ttLines.quantity EQ 0 THEN 
        ASSIGN 
            ipbf-ttLines.quantity    = ipbf-ttLines.quantityInvoiced
            ipbf-ttLines.quantityUOM = ipbf-ttLines.quantityInvoicedUOM
            .
            
    IF ttLines.taxable THEN                                
        ASSIGN     
            ipbf-ttLines.amountTax                 = ipbf-ttLines.amountTaxExFreight        + ipbf-ttLines.amountFreightTax
            ipbf-ttLines.amountTaxable             = ipbf-ttLines.amountTaxableExFreight    + ipbf-ttLines.amountTaxableFreight
            ipbf-ttInv.amountTotalTaxableExFreight = ipbf-ttInv.amountTotalTaxableExFreight + ipbf-ttLines.amountTaxableExFreight
            ipbf-ttInv.amountTotalTaxExFreight     = ipbf-ttInv.amountTotalTaxExFreight     + ipbf-ttLines.amountTaxExFreight
            ipbf-ttInv.amountTotalTaxable          = ipbf-ttInv.amountTotalTaxable          + ipbf-ttLines.amountTaxable
            .
END PROCEDURE.

PROCEDURE pConvertUnitPrice PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcItemID    AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdPrice     AS DECIMAL   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcFromUom   AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcToUom     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPriceInEA AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN Conv_ValueFromUOMtoUOM(
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  "FG", 
        INPUT  ipdPrice,
        INPUT  ipcFromUom,
        INPUT  ipcToUom, 
        INPUT  0,
        INPUT  0,
        INPUT  0,
        INPUT  0,
        INPUT  0,
        OUTPUT opdPriceInEA, 
        OUTPUT lError,
        OUTPUT cMessage
        ). 

END PROCEDURE.

PROCEDURE pConvQtyPriceUOM:
    /*------------------------------------------------------------------------------
     Purpose: Converts Price to Quantity UOM
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO. 
    DEFINE INPUT        PARAMETER ipcItemID    AS CHARACTER NO-UNDO.  
    DEFINE INPUT        PARAMETER ipcFromUom   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcToUom     AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice    AS DECIMAL   NO-UNDO.
                        
    RUN pConvertUnitPrice(
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  iopdPrice,
        INPUT  ipcFromUom,
        INPUT  ipcToUom,
        OUTPUT iopdPrice 
        ).                        
END PROCEDURE.

PROCEDURE pCreateAddonRecord:
    DEFINE INPUT PARAMETER rInvRecid AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER iInvLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipAgencyCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipChargeDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipRate AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipBillIableCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipReference AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipCustNum AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipNextAddonNum AS INTEGER NO-UNDO.

    DEFINE BUFFER bAddon FOR ttAddons.

    FIND FIRST ttAddons EXCLUSIVE-LOCK
        WHERE ttAddons.partner     = ipCustNum
        AND ttAddons.Description[1] = ipChargeDesc
        NO-ERROR.
    IF NOT AVAILABLE ttAddons THEN
    DO:
        ipNextAddonNum = 0.
        FOR each bAddon NO-LOCK 
            WHERE bAddon.partner = ipCustNum 
            :
            ipNextAddonNum = ipNextAddonNum + 1.
        END.
        ipNextAddonNum = ipNextAddonNum + 1.
        IF iInvLine EQ 0 THEN 
            iInvLine = ipNextAddonNum.

        /* ### if both of the above fail then this addon will be an orphan */
        CREATE ttAddons.
        ASSIGN
            ttAddons.partner    = ipCustNum
            /*            ttAddons.company    = edivtran.company   */
            /*            ttAddons.invoice-no = edivtran.invoice-no*/
            ttAddons.line       = ipNextAddonNum /* iInvLine */
            ttAddons.Addon-line = ipNextAddonNum
            .
    END.

    ASSIGN
        ttAddons.Description[1]   = ipChargeDesc
        ttAddons.Description[2]   = ""
        ttAddons.allow-charge     = IF ipAmount >= 0 THEN FALSE ELSE TRUE
        ttAddons.Amount           = ipAmount
        ttAddons.hand-meth        = IF ipBillIableCode = "Y" THEN "02" /* off invoice */ ELSE
    IF ipBillIableCode = "N" THEN "05" /* paid by vendor */
    ELSE "02"
        ttAddons.Agency-qual      = ""
        ttAddons.agency-code      = ipAgencyCode /* ### requires xlate */
        ttAddons.Ref-Num          = ipReference
        ttAddons.special-svc-code = ""
        ttAddons.Rate             = ipRate
        .

  
    RELEASE ttAddons.

END PROCEDURE.

PROCEDURE pCreateAddress PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcN1Code       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiAddressOrder AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCode         LIKE ar-inv.sold-id NO-UNDO.
    DEFINE INPUT PARAMETER ipcName         LIKE shipto.ship-name NO-UNDO.
    DEFINE INPUT PARAMETER ipcAddress1     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAddress2     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCity         LIKE shipto.ship-city NO-UNDO.
    DEFINE INPUT PARAMETER ipcState        LIKE shipto.ship-state NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip          LIKE shipto.ship-zip NO-UNDO.
    DEFINE INPUT PARAMETER ipcCountry      LIKE shipto.country NO-UNDO.
    
    CREATE ttN1Address.
    ASSIGN 
        ttN1Address.N1Code       = ipcN1code
        ttN1Address.addressOrder = ipiAddressOrder
        ttN1Address.addressCode  = ipcCode
        ttN1Address.addressName  = ipcName
        ttN1Address.address1     = ipcAddress1
        ttN1Address.address2     = ipcAddress2
        ttN1Address.state        = ipcState
        ttN1Address.city         = ipcCity
        ttN1Address.zip          = ipcZip
        ttN1Address.country      = (IF ipcCountry NE "" THEN ipcCountry ELSE "US")
        .
END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLIdentity",
        INPUT  "C",           /* Logical */
        INPUT  NO,           /* check by cust */
        INPUT  NO,           /* use cust not vendor */
        INPUT  ipcCustomerID, /* cust */
        INPUT  ipcShipToID,   /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
        
    gcCXMLIdentityCust = cReturn.
      
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLIdentity", 
        INPUT  "L",      /* Logical */
        INPUT  NO,      /* check by cust */
        INPUT  NO,      /* use cust not vendor */
        INPUT  "",      /* cust */
        INPUT  "",      /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
        
    gcCXMLDeploymentMode = IF cReturn EQ "YES" THEN "production" ELSE "test".
    
    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "cXMLShipToPrefix",
        INPUT  "C",            /* Logical */
        INPUT  YES,            /* check by cust */ 
        INPUT  YES,            /* use cust not vendor */
        INPUT  ipcCustomerID,  /* cust */
        INPUT  ipcShipToID,    /* ship-to*/
        OUTPUT cReturn, 
        OUTPUT lFound
        ).
    gcCXMLShipToPrefix = TRIM(cReturn).
    
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "cXMLSecret", 
        INPUT  "C",      /* Logical */
        INPUT  NO,      /* check by cust */
        INPUT  NO,      /* use cust not vendor */
        INPUT  "",      /* cust */
        INPUT  "",      /* ship-to*/
        OUTPUT cReturn,
        OUTPUT lFound
        ).
    gcCXMLSharedSecret = cReturn.  
      
    FIND FIRST sys-ctrl-shipto NO-LOCK
         WHERE sys-ctrl-shipto.company      EQ ipcCompany
           AND sys-ctrl-shipto.name         EQ "cXMLInvoice"
           AND sys-ctrl-shipto.cust-vend    EQ YES
           AND sys-ctrl-shipto.cust-vend-no EQ ipcCustomerID
         NO-ERROR.   
         
    IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN 
        gcCXMLIdentity  = sys-ctrl-shipto.char-fld.  
        
    gcCXMLTimeStamp = STRING(YEAR(TODAY),'9999')
                      + '-'
                      + STRING(MONTH(TODAY),'99')
                      + '-'
                      + STRING(DAY(TODAY),'99')
                      + 'T'
                      + STRING(TIME,'hh:mm:ss')
                      + '-05:00'
                      .
    gcCXMLpayloadID = pGetPayloadID().                         

END PROCEDURE.

PROCEDURE pUpdateLineRequestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLines FOR ttLines.
    
    DEFINE INPUT-OUTPUT PARAMETER ioplcLineData AS LONGCHAR.
    DEFINE INPUT-OUTPUT PARAMETER ioplgFirst    AS LOGICAL.
        
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "LineNumber",STRING(ipbf-ttLines.orderLine)). 
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "InvoiceQty",STRING(ipbf-ttLines.quantity)). 
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "UOM",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ipbf-ttLines.priceUOM)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "UnitPrice",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",STRING(ttLines.pricePerUOM))). 
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ItemNo",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ipbf-ttLines.customerPartID)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ItemName",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ipbf-ttLines.itemName)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "LineSubTotalAmount",STRING(ttLines.priceTotal)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ShiptoID",ttInv.shiptoID).    
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ShiptoName",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoName)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ShiptoCity",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoCity)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ShiptoState",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoState)).
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "ShiptoPostalCode",DYNAMIC-FUNCTION("fReplaceExceptionCharacters",ttInv.shiptoPostalcode)).
    IF ioplgFirst THEN DO:
        RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "Freight",STRING(ttInv.amountTotalFreight)).
        ioplgFirst = NO.    
    END.
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "Freight", "0").
    RUN updateRequestData(INPUT-OUTPUT ioplcLineData, "InvoiceUnitPrice",STRING(ipbf-ttLines.pricePerEA)).
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION pGetPayloadID RETURNS CHARACTER PRIVATE
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cPayLoadID AS CHARACTER NO-UNDO.
    
    ASSIGN
      cPayLoadID = STRING(NOW)
      cPayLoadID = REPLACE(cPayLoadID,'/','')
      cPayLoadID = REPLACE(cPayLoadID,' ','')
      cPayLoadID = REPLACE(cPayLoadID,' ','')
      cPayLoadID = REPLACE(cPayLoadID,':','')
      cPayLoadID = REPLACE(cPayLoadID,'-','')
      cPayLoadID = REPLACE(cPayLoadID,'.','')
      cPayLoadID = cPayLoadID + ''
      cPayLoadID = cPayLoadID + '.' + STRING(RANDOM(1000,9999),'9999') 
      .
    
    RETURN cPayLoadID.
END FUNCTION.

