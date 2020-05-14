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
    
    DEFINE VARIABLE cPostalStreet  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalCity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalState   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalZip     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalCountry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPostalEMail   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cPhoneCountryCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneCityCode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneNumber      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhoneExtension   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuantity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOM         AS CHARACTER NO-UNDO.

    /* ************************  Function Prototypes ********************** */
    FUNCTION GetPayLoadID RETURNS CHARACTER
    	( ipcProcessID AS CHARACTER ) FORWARD.

    /* ***************************  Main Block  *************************** */        
    
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

        RUN GetCXMLIdentities (
            INPUT  oe-bolh.company,
            INPUT  oe-bolh.cust-no,
            OUTPUT cCompanyIdentity,
            OUTPUT cPartnerIdentity,
            OUTPUT cSharedSecret
            ) NO-ERROR.

        RUN oe/custxship.p (
            INPUT  oe-bolh.company,
            INPUT  oe-bolh.cust-no,
            INPUT  oe-bolh.ship-id,
            BUFFER shipto
            ).
        IF AVAILABLE shipTo THEN
            ASSIGN
                cPostalStreet     = shipto.ship-addr[1] + shipto.ship-addr[2]
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
        
        ASSIGN
            cEstimatedTimeOfArrival = STRING(oe-bolh.bol-date + 2)
            cShipID                 = STRING(oe-bolh.ship-id)
            cInStoreDate            = STRING(oe-bolh.bol-date + 4)
            cTotalPallets           = STRING(oe-bolh.tot-pallets,">>>9")
            cTotalWeight            = STRING(INTEGER(oe-bolh.tot-wt))
            cNoticeDate             = STRING(oe-bolh.bol-date)
            cBOLPrintDate           = STRING(oe-bolh.prt-date) + " " + STRING(oe-bolh.prt-time,"hh:mm:ss")
            cBOLShipDate            = STRING(oe-bolh.ship-date) + " " + STRING(oe-bolh.ship-time,"hh:mm:ss")
            cBOLRelDate             = STRING(oe-bolh.rel-date)
            .
        
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
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToPostalAddressStreet", cPostalStreet).
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
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InStoreDate", cInStoreDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalPallets", cTotalPallets).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalWeight", cTotalWeight).

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
                    
                    RUN updateRequestData(INPUT-OUTPUT lcOrderData, "OrderReferenceID", oe-ord.po-no).
                    RUN updateRequestData(INPUT-OUTPUT lcOrderData, "OrderPayloadID", oe-ord.spare-char-3).
                    
                    lcOrderConcatData = lcOrderConcatData + lcOrderData.                
                END.
            END.
                        
            lcItemData = lcItemTempData.
               
            RUN GetOriginalQuantity (
                INPUT  ROWID(oe-boll),
                OUTPUT dQuantity,
                OUTPUT cUOM
                ) NO-ERROR.                            

            cQuantity = TRIM(STRING(dQuantity, "->>>>>>>9")).
            
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemLineID", STRING(oe-boll.line)).
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemQuantity", cQuantity).                            
            RUN updateRequestData(INPUT-OUTPUT lcItemData, "ItemQuantityUOM", cUOM).
            
            lcItemConcatData = lcItemConcatData + lcItemData.
        END.    

        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LocationID", cLocationID).
            
        ioplcRequestData = REPLACE(ioplcRequestData,"$ItemDetailID$",lcItemConcatData).                
        ioplcRequestData = REPLACE(ioplcRequestData,"$OrderDetailID$",lcOrderConcatData).
        
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
    DEFINE OUTPUT PARAMETER opcCompanyIdentity AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPartnerIdentity AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSharedSecret    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.

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

FUNCTION FormatDateForCXML RETURNS CHARACTER 
	( ipdtDate AS DATE, ipiTime AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Formats a given date and time to cXML formatted date
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cFormattedDate AS CHARACTER NO-UNDO.
    
    IF ipdtDate EQ ? THEN
        cFormattedDate = "".
    ELSE
        cFormattedDate = STRING(YEAR(ipdtDate),"9999") + "-" 
                       + STRING(MONTH(ipdtDate),"99") + "-" 
                       + STRING(DAY(ipdtDate),"99")
                       + 'T'
                       + STRING(ipiTime,'hh:mm:ss')
                       + '-05:00'.
    RETURN cFormattedDate.
END FUNCTION.

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
