/*------------------------------------------------------------------------
    File        : api/SendAdvancedShipNotice.p
    Purpose     : Returns the request data with BOL information

    Syntax      :

    Description : Returns the request data with BOL information

    Author(s)   : Mithun Porandla
    Created     : Tue Jan 28 07:33:22 EDT 2019
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
        
    DEFINE VARIABLE lcItemData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcItemConcatData  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcItemTempData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderConcatData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOrderTempData   AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE lAvailable AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOrderID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValidBOL  AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE cReturn    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound  AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cLocationID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEstimatedTimeOfArrival AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipID                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInStoreDate            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTotalPallets           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTotalWeight            AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCompanyIdentity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartnerIdentity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSharedSecret    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cNoticeDate   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLPrintDate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLShipDate  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLRelDate   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderDate    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDelivDate    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBuyerPart    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cPostalID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalStreet  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalStreet1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalStreet2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalCity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalState   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalZip     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalCountry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalEMail   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cPhoneCountryCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneCityCode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneNumber      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneExtension   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCustomerID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerStreet1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerStreet2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerCity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerState   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerZip     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerCountry AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dConsolidatedQty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuantity        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOM             AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCarrierDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderStatus     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iSECount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iHLCount AS INTEGER NO-UNDO INITIAL 2.
    DEFINE VARIABLE iTotalCases AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTotalCases      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
    
    DEFINE BUFFER bf-carrier  FOR carrier.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-loc      FOR loc.
    DEFINE BUFFER bf-location FOR location.
    DEFINE BUFFER bf-soldto   FOR soldto.
    
    /* ************************  Function Prototypes ********************** */
    FUNCTION GetPayLoadID RETURNS CHARACTER
    	( ipcProcessID AS CHARACTER ) FORWARD.

    /* ***************************  Main Block  *************************** */        

    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
    
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
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "oe-bolh" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid oe-bolh record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE ROWID(oe-bolh) = TO-ROWID(ttArgs.argValue) NO-ERROR.
        IF NOT AVAILABLE oe-bolh THEN DO:
            ASSIGN
                opcMessage = "Invalid oe-bolh ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        oAttribute = NEW system.Attribute().
        oAttribute:RequestDataType = gcRequestDataType.
            
        RUN GetCXMLIdentities (
            INPUT  oe-bolh.company,
            INPUT  oe-bolh.cust-no,
            INPUT  oe-bolh.ship-id,
            OUTPUT cCompanyIdentity,
            OUTPUT cPartnerIdentity,
            OUTPUT cSharedSecret,
            OUTPUT cPostalID
            ) NO-ERROR.

        RUN oe/custxship.p (
            INPUT  oe-bolh.company,
            INPUT  oe-bolh.cust-no,
            INPUT  oe-bolh.ship-id,
            BUFFER shipto
            ).
            
        FIND FIRST shipto NO-LOCK WHERE 
            shipto.company EQ oe-bolh.company AND 
            shipto.cust-no EQ oe-bolh.cust-no AND 
            shipto.ship-id EQ oe-bolh.ship-id
            NO-ERROR.
                
        IF AVAILABLE shipTo THEN DO:
            ASSIGN
                cPostalName       = shipto.ship-name
                cPostalStreet     = shipto.ship-addr[1] + shipto.ship-addr[2]
                cPostalStreet1    = shipto.ship-addr[1]
                cPostalStreet2    = shipto.ship-addr[2]
                cPostalCity       = shipto.ship-city
                cPostalState      = shipto.ship-state
                cPostalZip        = shipto.ship-zip
                cPostalCountry    = shipto.country
                cPostalEMail      = shipto.email
                cPhoneCountryCode = shipto.phone-country
                cPhoneCityCode    = shipto.area-code
                cPhoneNumber      = shipto.phone
                cPhoneExtension   = shipto.phone-prefix
                .
        END. 
        
        FIND FIRST loc NO-LOCK WHERE
            loc.company EQ oe-bolh.company AND 
            loc.loc EQ oe-bolh.loc
            NO-ERROR. 
        IF AVAIL loc THEN FIND FIRST location NO-LOCK WHERE 
            location.company EQ loc.company AND 
            location.locationCode EQ loc.loc
            NO-ERROR.

        ASSIGN
            cEstimatedTimeOfArrival = STRING(oe-bolh.bol-date + 2)
            cShipID                 = STRING(oe-bolh.ship-id)
            cInStoreDate            = STRING(oe-bolh.bol-date + 4)
            cTotalPallets           = STRING(oe-bolh.tot-pallets,">>>9")
            cTotalWeight            = STRING(INTEGER(oe-bolh.tot-wt))
            cNoticeDate             = STRING(TODAY)
            cBOLPrintDate           = STRING(oe-bolh.prt-date) + " " + STRING(oe-bolh.prt-time,"hh:mm:ss")
            cBOLRelDate             = STRING(oe-bolh.rel-date)
            cDelivDate              = STRING(oe-bolh.bol-date + 2)
            cPOID                   = oe-bolh.po-no
            .
            
            IF oe-bolh.ship-date EQ ? THEN 
              cBOLShipDate          = STRING(today) + " " + STRING(TIME, "hh:mm:ss").
            ELSE 
              cBOLPrintDate         = STRING(oe-bolh.prt-date) + " " + STRING(oe-bolh.prt-time,"hh:mm:ss").

        FIND FIRST bf-carrier NO-LOCK 
             WHERE bf-carrier.company EQ oe-bolh.company
               AND bf-carrier.carrier EQ oe-bolh.carrier         
             NO-ERROR.     
        IF AVAILABLE bf-carrier THEN
            cCarrierDescription = bf-carrier.dscr.

        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.company EQ oe-bolh.company
               AND bf-cust.cust-no EQ oe-bolh.cust-no
             NO-ERROR.
        IF AVAILABLE bf-cust THEN
            ASSIGN
                cCustomerID      = bf-cust.cust-no
                cCustomerName    = bf-cust.name
                cCustomerStreet1 = bf-cust.addr[1]
                cCustomerStreet2 = bf-cust.addr[2]
                cCustomerCity    = bf-cust.city
                cCustomerState   = bf-cust.state
                cCustomerZip     = bf-cust.zip
                cCustomerCountry = bf-cust.country
                .
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-cust:HANDLE).
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", GetPayLoadID("")).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "NoticeDate", cNoticeDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyIdentity", cCompanyIdentity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PartnerIdentity", cPartnerIdentity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CompanyIdentity", cCompanyIdentity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SharedSecret", cSharedSecret).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLID", STRING(oe-bolh.bol-no)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLPrintDate", cBOLPrintDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLShipDate", cBOLShipDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BOLRelDate", cBOLRelDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToName", cPostalName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressStreet", cPostalStreet).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressStreet1", cPostalStreet1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressStreet2", cPostalStreet2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressCity", cPostalCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressState", cPostalState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressPostalCode", cPostalZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressCountryCode", cPostalCountry).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToEmail", cPostalEMail).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPhoneCountryCode", cPhoneCountryCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPhoneAreaOrCityCode", cPhoneCityCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPhoneNumber", cPhoneNumber).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPhoneExtension", cPhoneExtension).            
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CarrierName", oe-bolh.carrier).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TrailerID", oe-bolh.trailer).      
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EstimatedTimeOfArrival", cEstimatedTimeOfArrival).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipID", cShipID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PostalID", cPostalID).     
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InStoreDate", cInStoreDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalPallets", cTotalPallets).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalWeight", cTotalWeight).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CarrierDescription", cCarrierDescription).

        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerID", cCustomerID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerName", cCustomerName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerAddress1", cCustomerStreet1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerAddress2", cCustomerStreet2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerCity", cCustomerCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerState", cCustomerState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerPostalCode", cCustomerZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerCountry", cCustomerCountry).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SCAC", IF AVAIL shipto THEN shipto.scac ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FreightTerms", oe-bolh.frt-pay).
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromName", IF AVAIL loc THEN loc.dscr ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromStreetAddress1", IF AVAIL location THEN location.streetAddr[1] ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromStreetAddress2", IF AVAIL location THEN location.streetAddr[2] ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromCity", IF AVAIL location THEN location.subCode3 ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromState", IF AVAIL location THEN location.subCode1 ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipFromPostCode", IF AVAIL location THEN location.subCode4 ELSE "").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SiteID", IF AVAIL shipto THEN shipto.siteid  ELSE "").
        
        
                
        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND APIOutboundDetail.detailID      EQ "OrderDetailID"
               AND APIOutboundDetail.parentID      EQ "SendAdvancedShipNotice"
             NO-ERROR.        
        IF AVAILABLE APIOutboundDetail THEN
            lcOrderTempData = APIOutboundDetail.data.

        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND APIOutboundDetail.detailID      EQ "ItemDetailID"
               AND APIOutboundDetail.parentID      EQ "SendAdvancedShipNotice"
             NO-ERROR.        
        IF AVAILABLE APIOutboundDetail THEN
            lcItemTempData = APIOutboundDetail.data.  
                
        iTotalCases = 0.
        
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
            BREAK BY oe-boll.ord-no
                  BY oe-boll.line:
            IF FIRST-OF(oe-boll.ord-no) THEN DO:
                cLocationID = oe-boll.loc.
                
                FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                     NO-ERROR.
                IF AVAILABLE oe-ord THEN DO:
                    lcOrderData = lcOrderTempData.
                    cOrderDate = STRING(oe-ord.ord-date).
                    RUN updateRequestData(INPUT-OUTPUT lcOrderData, "OrderReferenceID", oe-ord.po-no).
                    RUN updateRequestData(INPUT-OUTPUT lcOrderData, "OrderPayloadID", oe-ord.spare-char-3).
                    RUN updateRequestData(INPUT-OUTPUT lcOrderData, "OrderDate", cOrderDate).
                    
                    lcOrderConcatData = lcOrderConcatData + lcOrderData.
                    
                    FIND FIRST bf-soldto NO-LOCK
                         WHERE bf-soldto.company EQ oe-ord.company
                           AND bf-soldto.cust-no EQ oe-ord.cust-no
                           AND bf-soldto.sold-id EQ oe-ord.sold-id
                         NO-ERROR.                
                END.
            END.
                        
            
            lcItemData = lcItemTempData.
               
            RUN GetOriginalQuantity (
                INPUT  ROWID(oe-boll),
                OUTPUT dQuantity,
                OUTPUT cUOM
                ) NO-ERROR.                            

            cQuantity = TRIM(STRING(dQuantity, "->>>>>>>9")).
            
            iHLCount = iHLCount + 1.
            iTotalCases = iTotalCases + oe-boll.cases.
            

            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ oe-boll.company
                   AND bf-itemfg.i-no    EQ oe-boll.i-no
                 NO-ERROR.
            IF AVAILABLE bf-itemfg THEN
                cItemName = bf-itemfg.i-name.
            
            FIND FIRST oe-ordl NO-LOCK 
                 WHERE oe-ordl.company EQ oe-boll.company
                   AND oe-ordl.ord-no  EQ oe-boll.ord-no
                   AND oe-ordl.line    EQ oe-boll.line
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
                cBuyerPart = oe-ordl.part-no.
                                                 
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemLineID", STRING(oe-boll.line)).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemQuantity", cQuantity).                            
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemQuantityUOM", cUOM).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemID", oe-boll.i-no).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemName", cItemName).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "BuyerPart", cBuyerPart).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "HLCount", iHLCount).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "LineItemStatus", STRING(oe-boll.p-c)).

            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ oe-boll.company
                   AND oe-ordl.ord-no  EQ oe-boll.ord-no
                   AND oe-ordl.i-no    EQ oe-boll.i-no
                   AND oe-ordl.line    EQ oe-boll.line
                 NO-ERROR.                            
            
            lcItemData = oAttribute:ReplaceAttributes(lcItemData, BUFFER oe-ordl:HANDLE).
                        
            lcItemConcatData = lcItemConcatData + lcItemData.
        END.    

        cTotalCases = TRIM(STRING(iTotalCases,">>>>>>9")).
                
        FIND FIRST bf-loc NO-LOCK
             WHERE bf-loc.company EQ oe-bolh.company
               AND bf-loc.loc     EQ oe-bolh.loc
             NO-ERROR.
        IF AVAILABLE bf-loc THEN
            FIND FIRST bf-location NO-LOCK 
                 WHERE bf-location.company      EQ bf-loc.company
                   AND bf-location.locationCode EQ bf-loc.loc
                   AND bf-location.rec_key      EQ bf-loc.addrRecKey
                 NO-ERROR.
        
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-loc:HANDLE).                 
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-location:HANDLE).
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-soldto:HANDLE).
         
        RUN pUpdateDelimiter (INPUT-OUTPUT lcItemConcatData, "").
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LocationID", cLocationID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "HLCount", iHLCount).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalCases", cTotalCases).
            
        ioplcRequestData = REPLACE(ioplcRequestData,"$ItemDetailID$",lcItemConcatData).                
        ioplcRequestData = REPLACE(ioplcRequestData,"$OrderDetailID$",lcOrderConcatData).

        ASSIGN 
            iSECount = NUM-ENTRIES(ioplcRequestData, "~n") 
            /* Subtract lines before ST and after SE segments */
            iSECount = iSECount - 4
            .
            
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", STRING(iSECount)).
                
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE GetCXMLIdentities:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch the cXML related identities (Company, Partner, 
          SharedSecret)
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipID          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCompanyIdentity AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPartnerIdentity AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSharedSecret    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipToID        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cShipToPrefix AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-cust FOR cust.
    
    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipcCompany
           AND bf-cust.cust-no EQ ipcCustNo
         NO-ERROR.
         
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,      /* Company Code */
        INPUT  "cXMLIdentity",  /* sys-ctrl name */
        INPUT  "C",             /* Output return value */
        INPUT  NO,              /* Use ship-to */
        INPUT  NO,              /* ship-to vendor */
        INPUT  "",              /* ship-to vendor value */
        INPUT  "",              /* ship-id value */
        OUTPUT opcCompanyIdentity, 
        OUTPUT lRecFound
        ).
                    
    IF AVAILABLE bf-cust THEN DO:
        opcPartnerIdentity = bf-cust.ASNClientID.
        
        IF bf-cust.ASNClientID EQ "" THEN
            RUN sys/ref/nk1look.p (
                INPUT  ipcCompany,            /* Company Code */
                INPUT  "cXMLASN",             /* sys-ctrl name */
                INPUT  "C",                   /* Output return value */
                INPUT  YES,                   /* Use ship-to */
                INPUT  YES,                   /* ship-to vendor */
                INPUT  bf-cust.cust-no,       /* ship-to vendor value */
                INPUT  "",                    /* ship-id value */
                OUTPUT opcPartnerIdentity, 
                OUTPUT lRecFound
                ).                       

        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany,      /* Company Code */
            INPUT  "cXMLSecret",    /* sys-ctrl name */
            INPUT  "C",             /* Output return value */
            INPUT  YES,             /* Use ship-to */
            INPUT  YES,             /* ship-to vendor */
            INPUT  bf-cust.cust-no, /* ship-to vendor value */
            INPUT  "",              /* ship-id value */
            OUTPUT opcSharedSecret, 
            OUTPUT lRecFound
            ).       
            
        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany,      /* Company Code */
            INPUT  "cXMLShipToPrefix",  /* sys-ctrl name */
            INPUT  "C",             /* Output return value */
            INPUT  YES,             /* Use ship-to */
            INPUT  YES,             /* ship-to vendor */
            INPUT  bf-cust.cust-no, /* ship-to vendor value */
            INPUT  "",              /* ship-id value */
            OUTPUT cShipToPrefix, 
            OUTPUT lRecFound
            ).           
            
            opcShipToID = TRIM(cShipToPrefix) + ipcShipID.
  
                        
    END.

    IF opcSharedSecret EQ "" THEN DO:
        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany,      /* Company Code */
            INPUT  "cXMLSecret",    /* sys-ctrl name */
            INPUT  "C",             /* Output return value */
            INPUT  NO,              /* Use ship-to */
            INPUT  NO,              /* ship-to vendor */
            INPUT  "",              /* ship-to vendor value */
            INPUT  "",              /* ship-id value */
            OUTPUT opcSharedSecret, 
            OUTPUT lRecFound
            ).   
    END.
    
    RELEASE bf-cust.
END PROCEDURE.

PROCEDURE GetOriginalQuantity:
/*------------------------------------------------------------------------------
 Purpose: Procedure to get the original quantity of a BOL line
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriOeBoll  AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUOM      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCaseUOMList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    FIND FIRST bf-oe-boll NO-LOCK 
         WHERE ROWID(bf-oe-boll) EQ ipriOeBoll
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-boll THEN
        RETURN.
                 
    RUN sys/ref/nk1look.p (
        INPUT  oe-boll.company, /* Company Code */
        INPUT  "CaseUOMList",   /* sys-ctrl name */
        INPUT  "C",             /* Output return value */
        INPUT  NO,              /* Use ship-to */
        INPUT  NO,              /* ship-to vendor */
        INPUT  "",              /* ship-to vendor value */
        INPUT  "",              /* ship-id value */
        OUTPUT cCaseUOMList, 
        OUTPUT lRecFound
        ).
                         
    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ bf-oe-boll.company
           AND bf-oe-ordl.ord-no  EQ bf-oe-boll.ord-no
           AND bf-oe-ordl.i-no    EQ bf-oe-boll.i-no
           AND bf-oe-ordl.line    EQ bf-oe-boll.line
         NO-ERROR.                            
    IF AVAILABLE bf-oe-ordl THEN DO:
        ASSIGN
            opdQuantity = bf-oe-ordl.spare-dec-1
            opcUOM      = bf-oe-ordl.spare-char-2
            .
            
        IF (cUOM EQ 'CS' OR LOOKUP(opcUOM, cCaseUOMList) GT 0)
            AND opdQuantity        NE bf-oe-boll.qty 
            AND bf-oe-ordl.cas-cnt NE 0 THEN
            opdQuantity = bf-oe-boll.qty / bf-oe-ordl.cas-cnt.
        ELSE 
            opdQuantity = bf-oe-boll.qty.
    END.

    IF opdQuantity EQ 0 THEN
        opdQuantity = bf-oe-boll.qty.
                                        
    IF opcUOM EQ "" THEN 
        opcUOM = "EA".   
        
    RELEASE bf-oe-boll.
    RELEASE bf-oe-ordl.        
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION GetPayLoadID RETURNS CHARACTER
	(ipcProcessID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Generates a payload id to send in cXML ASN
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
        cPayLoadID = cPayLoadID + IF ipcProcessID NE '' THEN '.' + ipcProcessID ELSE ''
        cPayLoadID = cPayLoadID + '.' + STRING(RANDOM(1000,9999),'9999')
        cPayLoadID = cPayLoadID + "@PremPack.com"
        .
      
    RETURN cPayLoadID.
END FUNCTION.
