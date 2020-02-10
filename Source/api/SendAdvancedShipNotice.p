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
    
    /* Variables to store the header data */
    DEFINE VARIABLE lcHeaderData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderToData     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderFromData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderSenderData AS LONGCHAR NO-UNDO.
    
    /* Variables to store request tag data */
    DEFINE VARIABLE lcRequestData                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeRequestData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderContactData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderCommentData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipControlData             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticePortionData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeItemData          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeItemConcatData    AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE lAvailable    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOrderID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValidBOL     AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE cReturn       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cHeaderToID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeaderFromID AS CHARACTER NO-UNDO.
    
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
    DEFINE VARIABLE cCaseUomList AS CHARACTER NO-UNDO.
    
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
        
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:
            IF oe-boll.ord-no EQ 0 THEN DO:
                lValidBOL = FALSE.
                LEAVE.
            END.
            
            IF iOrderID NE 0 AND iOrderID NE oe-boll.ord-no THEN DO:
                lValidBOL = FALSE.
                LEAVE.
            END.
            
            iOrderID = oe-boll.ord-no.
        END.

        IF NOT lValidBOL THEN DO:
            ASSIGN
                opcMessage = IF iOrderID EQ 0 THEN
                                 "Invalid order exists in BOL" 
                             ELSE
                                 "Multiple orders exists in BOL"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-bolh.company
               AND oe-ord.ord-no  EQ iOrderID
             NO-ERROR.
        IF NOT AVAILABLE oe-ord THEN DO:
            ASSIGN
                opcMessage = "Invalid order number " + STRING(iOrderID) + " in BOL"
                oplSuccess = FALSE
                .
            RETURN.                   
        END.
        
        IF oe-ord.spare-char-3 EQ "" THEN DO:
            ASSIGN
                opcMessage = "Empty payloadID"
                oplSuccess = FALSE
                .
            RETURN.            
        END.

        RUN sys/ref/nk1look.p (
            INPUT  oe-bolh.company, /* Company Code */
            INPUT  "CaseUOMList",   /* sys-ctrl name */
            INPUT  "C",             /* Output return value */
            INPUT  NO,              /* Use ship-to */
            INPUT  NO,              /* ship-to vendor */
            INPUT  "",              /* ship-to vendor value */
            INPUT  "",              /* ship-id value */
            OUTPUT cCaseUomList, 
            OUTPUT lRecFound
            ).
                      
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", oe-ord.spare-char-3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "NoticeDate", STRING(YEAR(oe-bolh.bol-date),"9999") + "-" + STRING(MONTH(oe-bolh.bol-date),"99") + "-" + STRING(DAY(oe-bolh.bol-date),"99")).
        
        /* Header data update */        
        RUN pGetOutboundDetailData (
            INPUT  ipiAPIOutboundID,
            INPUT  "HeaderID",
            INPUT  "SendAdvancedShipNotice",
            OUTPUT lAvailable,
            OUTPUT lcHeaderData
            ).
        IF lAvailable THEN DO:            
            /* Header From data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderFromID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderFromData
                ).            
            IF lAvailable THEN DO:
                RUN sys/ref/nk1look.p (
                    INPUT  oe-bolh.company, /* Company Code */
                    INPUT  "cXMLIdentity",  /* sys-ctrl name */
                    INPUT  "C",             /* Output return value */
                    INPUT  NO,              /* Use ship-to */
                    INPUT  NO,              /* ship-to vendor */
                    INPUT  "",              /* ship-to vendor value */
                    INPUT  "",              /* ship-id value */
                    OUTPUT cReturn, 
                    OUTPUT lRecFound
                    ).
                
                cHeaderFromID = cReturn.
                
                RUN updateRequestData(INPUT-OUTPUT lcHeaderFromData, "HeaderFromIdentity", cHeaderFromID).
            END.
            
            /* Header To data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderToID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderToData
                ).
            IF lAvailable THEN DO:
                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ oe-ord.company
                       AND cust.cust-no EQ oe-ord.cust-no
                     NO-ERROR.
                IF AVAILABLE cust THEN DO:
                    IF cust.ASNClientID EQ "" THEN DO:
                        RUN sys/ref/nk1look.p (
                            INPUT  oe-bolh.company, /* Company Code */
                            INPUT  "cXMLASN",       /* sys-ctrl name */
                            INPUT  "C",             /* Output return value */
                            INPUT  YES,             /* Use ship-to */
                            INPUT  YES,             /* ship-to vendor */
                            INPUT  cust.cust-no,    /* ship-to vendor value */
                            INPUT  "",              /* ship-id value */
                            OUTPUT cReturn, 
                            OUTPUT lRecFound
                            ).
                        cHeaderToID = cReturn.                        
                    END.
                    ELSE
                        cHeaderToID = cust.ASNClientID.
                END.

                RUN updateRequestData(INPUT-OUTPUT lcHeaderToData, "HeaderToIdentity", cHeaderToID).
            END.    
                
            /* Header Sender data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderSenderID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderSenderData
                ).            
            IF lAvailable THEN DO:
                RUN updateRequestData(INPUT-OUTPUT lcHeaderSenderData, "HeaderSenderIdentity", cHeaderFromID).

                RUN sys/ref/nk1look.p (
                    INPUT  oe-bolh.company, /* Company Code */
                    INPUT  "cXMLSecret",    /* sys-ctrl name */
                    INPUT  "C",             /* Output return value */
                    INPUT  YES,             /* Use ship-to */
                    INPUT  YES,             /* ship-to vendor */
                    INPUT  cust.cust-no,    /* ship-to vendor value */
                    INPUT  "",              /* ship-id value */
                    OUTPUT cReturn, 
                    OUTPUT lRecFound
                    ).       
                IF NOT lRecFound OR cReturn EQ "" THEN DO:
                    RUN sys/ref/nk1look.p (
                        INPUT  oe-bolh.company, /* Company Code */
                        INPUT  "cXMLSecret",    /* sys-ctrl name */
                        INPUT  "C",             /* Output return value */
                        INPUT  NO,              /* Use ship-to */
                        INPUT  NO,              /* ship-to vendor */
                        INPUT  "",              /* ship-to vendor value */
                        INPUT  "",              /* ship-id value */
                        OUTPUT cReturn, 
                        OUTPUT lRecFound
                        ).   
                END.
                RUN updateRequestData(INPUT-OUTPUT lcHeaderSenderData, "HeaderSharedSecret", cReturn).
            END.
        END.

        /* Request tag data update */
        RUN pGetOutboundDetailData (
            INPUT  ipiAPIOutboundID,
            INPUT  "RequestID",
            INPUT  "SendAdvancedShipNotice",
            OUTPUT lAvailable,
            OUTPUT lcRequestData
            ).
        IF lAvailable THEN DO:
            /* ShipNoticeRequest tag data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "ShipNoticeRequestID",
                INPUT  "RequestID",
                OUTPUT lAvailable,
                OUTPUT lcShipNoticeRequestData
                ).              
            IF lAvailable THEN DO:
                /* ShipNoticeHeader tag data */
                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipNoticeHeaderID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipNoticeHeaderData
                    ).
                IF lAvailable THEN DO:
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderData, "ShipNoticeHeaderShipmentID", STRING(oe-bolh.bol-no)).
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderData, "ShipNoticeHeaderNoticeDate", STRING(YEAR(oe-bolh.prt-date),"9999") + "-" + STRING(MONTH(oe-bolh.prt-date),"99") + "-" + STRING(DAY(oe-bolh.prt-date),"99")).
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderData, "ShipNoticeHeaderShipmentDate", STRING(YEAR(oe-bolh.ship-date),"9999") + "-" + STRING(MONTH(oe-bolh.ship-date),"99") + "-" + STRING(DAY(oe-bolh.ship-date),"99")).
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderData, "ShipNoticeHeaderDeliveryDate", STRING(YEAR(oe-bolh.rel-date),"9999") + "-" + STRING(MONTH(oe-bolh.rel-date),"99") + "-" + STRING(DAY(oe-bolh.rel-date),"99")).
                    
                    RUN pGetOutboundDetailData (
                        INPUT  ipiAPIOutboundID,
                        INPUT  "ShipNoticeHeaderContactID",
                        INPUT  "ShipNoticeHeaderID",
                        OUTPUT lAvailable,
                        OUTPUT lcShipNoticeHeaderContactData
                        ).     
                    IF lAvailable THEN DO:
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
                                                
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPostalAddressStreet", cPostalStreet).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPostalAddressCity", cPostalCity).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPostalAddressState", cPostalState).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPostalAddressPostalCode", cPostalZip).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPostalAddressCountryCode", cPostalCountry).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactEmail", cPostalEMail).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPhoneCountryCode", cPhoneCountryCode).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPhoneAreaOrCityCode", cPhoneCityCode).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPhoneNumber", cPhoneNumber).
                        RUN updateRequestData(INPUT-OUTPUT lcShipNoticeHeaderContactData, "ContactPhoneExtension", cPhoneExtension).
                    END.
                    
                    RUN pGetOutboundDetailData (
                        INPUT  ipiAPIOutboundID,
                        INPUT  "ShipNoticeHeaderCommentID",
                        INPUT  "ShipNoticeHeaderID",
                        OUTPUT lAvailable,
                        OUTPUT lcShipNoticeHeaderCommentData
                        ). 
                END.
                
                ASSIGN
                    lcShipNoticeHeaderData = REPLACE(lcShipNoticeHeaderData,"$ShipNoticeHeaderContactID$",lcShipNoticeHeaderContactData)
                    lcShipNoticeHeaderData = REPLACE(lcShipNoticeHeaderData,"$ShipNoticeHeaderCommentID$",lcShipNoticeHeaderCommentData)
                    .

                /* ShipNoticeRequest tag data */
                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipControlID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipControlData
                    ).                
                IF lAvailable THEN DO:
                    RUN updateRequestData(INPUT-OUTPUT lcShipControlData, "ShipControlCarrierIndetifierCompanyName", oe-bolh.carrier).
                    RUN updateRequestData(INPUT-OUTPUT lcShipControlData, "ShipControlShipmentIdentifier", oe-bolh.trailer).
                END.
                
                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipNoticePortionID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipNoticePortionData
                    ).                    
                IF lAvailable THEN DO:
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticePortionData, "ShipNoticePortionOrderReferenceID", STRING(oe-bolh.po-no)).
                    RUN updateRequestData(INPUT-OUTPUT lcShipNoticePortionData, "ShipNoticePortionDocumentReferenceID", oe-ord.spare-char-3).
                    
                    FOR EACH oe-boll EXCLUSIVE-LOCK
                        WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.b-no    EQ oe-bolh.b-no:                    
                        RUN pGetOutboundDetailData (
                            INPUT  ipiAPIOutboundID,
                            INPUT  "ShipNoticeItemID",
                            INPUT  "ShipNoticePortionID",
                            OUTPUT lAvailable,
                            OUTPUT lcShipNoticeItemData
                            ).                    
                        IF lAvailable THEN DO:
                            dQuantity = oe-boll.qty.
                                 
                            FIND FIRST oe-ordl NO-LOCK
                                 WHERE oe-ordl.company EQ oe-boll.company
                                   AND oe-ordl.ord-no  EQ oe-boll.ord-no
                                   AND oe-ordl.i-no    EQ oe-boll.i-no
                                   AND oe-ordl.line    EQ oe-boll.line
                                 NO-ERROR.                            
                            IF AVAILABLE oe-ordl THEN DO:
                                ASSIGN
                                    cQuantity = TRIM(STRING(oe-ordl.spare-dec-1,"->>>>>>>9"))
                                    cUOM      = oe-ordl.spare-char-2
                                    .
                                    
                                IF (cUOM EQ 'CS' OR LOOKUP(cUOM, cCaseUOMList) GT 0)
                                    AND DECIMAL(cQuantity) NE dQuantity 
                                    AND oe-ordl.cas-cnt    NE 0 THEN
                                    cQuantity = TRIM(STRING(dQuantity / oe-ordl.cas-cnt, "->>>>>>>9")).
                                ELSE 
                                    cQuantity = TRIM(STRING(dQuantity, "->>>>>>>9")).
                            END.

                            IF DECIMAL(cQuantity) EQ 0 THEN
                                cQuantity = TRIM(STRING(dQuantity, "->>>>>>>9")).
                                                                
                            IF cUOM EQ "" THEN 
                                cUOM = "EA".
                                                            
                            RUN updateRequestData(INPUT-OUTPUT lcShipNoticeItemData, "ShipNoticePortionItemLineID", STRING(oe-boll.line)).
                            RUN updateRequestData(INPUT-OUTPUT lcShipNoticeItemData, "ShipNoticePortionItemQuantity", cQuantity).                            
                            RUN updateRequestData(INPUT-OUTPUT lcShipNoticeItemData, "ShipNoticePortionUnitOfMeasure", cUOM).
                        END.
                        
                        lcShipNoticeItemConcatData = lcShipNoticeItemConcatData + lcShipNoticeItemData.
                    END.                    
                END.
                
                lcShipNoticePortionData = REPLACE(lcShipNoticePortionData,"$ShipNoticeItemID$",lcShipNoticeItemConcatData).                
            END.
            
            ASSIGN
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipNoticeHeaderID$",lcShipNoticeHeaderData)
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipControlID$",lcShipControlData)
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipNoticePortionID$",lcShipNoticePortionData)
                .    
        END.

        ASSIGN
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderToID$",lcHeaderToData)
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderFromID$",lcHeaderFromData)
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderSenderID$",lcHeaderSenderData)
            lcRequestData = REPLACE(lcRequestData,"$ShipNoticeRequestID$",lcShipNoticeRequestData)
            .

        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData,"$HeaderID$",lcHeaderData)
            ioplcRequestData = REPLACE(ioplcRequestData,"$RequestID$",lcRequestData)
            .
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetOutboundDetailData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the request data struction for given inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailable     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData         AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
  
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.        
    IF AVAILABLE bf-APIOutboundDetail THEN
        ASSIGN
            oplAvailable = TRUE
            oplcData     = bf-APIOutboundDetail.data
            .

    RELEASE bf-APIoutboundDetail.
END PROCEDURE.
