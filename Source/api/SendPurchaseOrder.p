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
    {system/FormulaProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantityInEA     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInM      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInSF     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostInMSF        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    
    /* Variables to store order line's request data */
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineData AS LONGCHAR  NO-UNDO.

    /* Variables to store order line adder's request data */
    DEFINE VARIABLE lcLineAdderData         AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineAdderData   AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcLineAdderDataGP       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineAdderDataGP AS LONGCHAR  NO-UNDO.    

    /* Variables to store order line scores request data */
    DEFINE VARIABLE lcLineScoresData         AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineScoresData   AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcLineScoresDataGP       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineScoresDataGP AS LONGCHAR  NO-UNDO.    
    
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
    DEFINE VARIABLE cShipToCityState   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToZip         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToAddressFull AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCarrierID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCarrierDesc       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightTerms      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightFOB        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBuyer             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNotes           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNotesHRMS       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPONotesGP         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToCompanyName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTotalCost         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentDateTime   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGPPartnerID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGPPurchasedBy     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGPPlantID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGPBillto          AS CHARACTER NO-UNDO.
    
    /* Purchase Order Line Variables */
    DEFINE VARIABLE cPoLine                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityOrdered         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityInSF            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityInM             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemType                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemTypeShort           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc1               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc2               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemIDVendor            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOverPct                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUnderPct                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineDueDate           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWidth               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemLength              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDepth               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWidthTrunc          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemLengthTrunc         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDepthTrunc          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWidth16ths          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemLength16ths         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDepth16ths          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostPerUOM              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostInMSF               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostUOM                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostSetup               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostDiscount            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderNo                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperationID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStackHeight             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletWidth             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletLength            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletHeight            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUnitPallet              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID2                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobConcat               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobConcatHRMS           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDFormNo             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobIDBlankNo            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineStatus            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityReceived        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineDueDate             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStyle                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStyleDesc               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemBasisWeight         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFlute                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineNotes             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyINEA                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOLowQty                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoHighQty               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormattedScoresWestrock AS CHARACTER NO-UNDO.
        
    /* Purchase Order Line adder Variables */
    DEFINE VARIABLE cAdderItemID                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdderItemName                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWithAdders                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWithAddersHRMS             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWithAddersX4               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWithAddersX10              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemWithAddersX10WithoutConcat AS CHARACTER NO-UNDO.
    
    /* Purchase Order Line scores Variables */
    DEFINE VARIABLE cScoreOn                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSize                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreType                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSize16ths             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSizeDecimal           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dScoreSize16ths             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dScoreSizeDecimal           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dScoreSizeArray             AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE VARIABLE cScoreTypeArray             AS CHARACTER NO-UNDO EXTENT 20.
    DEFINE VARIABLE dSizeFactor                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSizeFormat                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDecimalLog                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lReftableFound              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cScoreSize16thsWestRock     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSizeDecimalHRMS1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSizeDecimalHRMS2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreSizeDecimalAlliFlutes AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cWhsCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyPerPack        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurchaseUnit      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex1            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustName          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustAddress1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustAddress2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustCity          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustState         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustCityState     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustZip           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustAreaCode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustPhone         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXName         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXAddress1     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXAddress2     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXCity         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXState        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXCityState    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXZip          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXAreaCode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustXPhone        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTempAdder         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBoardWeight       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdder             AS CHARACTER NO-UNDO EXTENT 7.    
    DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAssignedCustID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOExport          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValue       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iGPEDI             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cClientID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestDataType   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail.    
    DEFINE BUFFER bf-hrms-reftable      FOR reftable.
    DEFINE BUFFER bf-job-mat            FOR job-mat.
    DEFINE BUFFER bf-item               FOR item.
    
    DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    DEFINE VARIABLE hdFormulaProcs AS HANDLE NO-UNDO.
    RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
    
    DEFINE VARIABLE hdPOProcs AS HANDLE NO-UNDO.
    RUN po/POProcs.p PERSISTENT SET hdPOProcs.
    
    DEFINE VARIABLE cRequestFile     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestFilePath AS CHARACTER NO-UNDO.
    
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

        RUN sys/ref/nk1look.p (
            INPUT po-ord.company, /* Company Code */ 
            INPUT "POEXPORT",     /* sys-ctrl name */
            INPUT "C",            /* Output return value */
            INPUT NO,             /* Use ship-to */
            INPUT NO,             /* ship-to vendor */
            INPUT "",             /* ship-to vendor value */
            INPUT "",             /* shi-id value */
            OUTPUT cPOExport, 
            OUTPUT lRecFound
            ).
        RUN sys/ref/nk1look.p (
            INPUT po-ord.company, /* Company Code */ 
            INPUT "GP",     /* sys-ctrl name */
            INPUT "I",            /* Output return value */
            INPUT NO,             /* Use lship-to */
            INPUT NO,             /* ship-to vendor */
            INPUT "",             /* ship-to vendor value */
            INPUT "",             /* shi-id value */
            OUTPUT cReturnValue, 
            OUTPUT lRecFound
            ).
        IF lRecFound THEN
            iGPEDI = INTEGER(cReturnValue).    
        
        IF NOT CAN-FIND(FIRST po-ordl
             WHERE po-ordl.company EQ po-ord.company
               AND po-ordl.po-no   EQ po-ord.po-no) THEN DO:
            ASSIGN
                opcMessage = "No po-ordl records available for po [ " + STRING(po-ord.po-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST sys-ctrl-shipto NO-LOCK 
             WHERE sys-ctrl-shipto.company      EQ po-ord.company 
               AND sys-ctrl-shipto.name         EQ "GP" 
               AND sys-ctrl-shipto.cust-vend    EQ FALSE
               AND sys-ctrl-shipto.cust-vend-no EQ po-ord.vend-no
             NO-ERROR.
        IF iGPEDI EQ 1 AND AVAIL sys-ctrl-shipto THEN 
            ASSIGN
                cGPPartnerID   = ENTRY(1,sys-ctrl-shipto.char-fld,"|")
                cGPPurchasedBY = CAPS(ENTRY(1, sys-ctrl-shipto.char-fld,"|"))
                cGPPlantID     = ENTRY(2,sys-ctrl-shipto.char-fld,"|")
                cGPBillto      = ENTRY(1,sys-ctrl-shipto.char-fld,"|") + " " + ENTRY(2,sys-ctrl-shipto.char-fld,"|")
                .
        FIND FIRST company
             WHERE company.company EQ po-ord.company
             NO-LOCK.
             
        ASSIGN
            cCompany           = STRING(po-ord.company)
            cShipToCompanyName = IF AVAILABLE company THEN company.name ELSE ""
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
            cShipToCityState   = cShipToCity + " " + cShipToState
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
            cFOBCode           = IF po-ord.fob-code EQ "DEST" THEN 
                                     "DESTINATION" 
                                 ELSE 
                                     "ORIGIN"
            cTotalCost         = STRING(po-ord.t-cost)
            cCurrentDateTime   = STRING(DATETIME(TODAY,MTIME))
            .
        
        ASSIGN
            cClientID        = APIOutbound.clientID
            cRequestDataType = APIOutbound.requestDataType 
            .
        
        FIND FIRST cust NO-LOCK 
             WHERE cust.company EQ po-ord.company
               AND cust.active  EQ "X"
             NO-ERROR.
        IF AVAILABLE cust THEN DO:
            ASSIGN
                cCustXName      = cust.name
                cCustXAddress1  = cust.addr[1]
                cCustXAddress2  = cust.addr[2]
                cCustXCity      = cust.city
                cCustXState     = cust.state
                cCustXZip       = cust.zip
                cCustXCityState = cCustXCity + " " + TRIM(cCustXState)
                cCustXAreaCode  = STRING(cust.area-code)  
                cCustXPhone     = STRING(cust.phone)
                .
        END.

    
        FIND FIRST carrier NO-LOCK 
             WHERE carrier.company EQ po-ord.company
               AND carrier.carrier EQ po-ord.carrier         
             NO-ERROR.     
        IF AVAILABLE carrier THEN
            cCarrierDesc = carrier.dscr.

        /* Fetch purchase order notes from notes table */    
        FOR EACH notes NO-LOCK
           WHERE notes.rec_key EQ po-ord.rec_key:
            ASSIGN 
                cPoNotes   = cPoNotes + " " + STRING(notes.note_text)
                cPoNotesGP = cPoNotesGP + IF cPoNotesGP EQ "" THEN "" ELSE ".**" + TRIM(notes.note_text).
        END.
        
        cPoNotes = REPLACE(cPoNotes, "~n", "").
        
        cPoNotesHRMS = cPoNotes.
        
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

        RUN GetSizeFactor IN hdFormulaProcs (
            INPUT  po-ord.company,
            OUTPUT dSizeFactor,
            OUTPUT cSizeFormat,
            OUTPUT lDecimalLog
            ).
                    
        /* Fetch line details for the purchase order */         
        FOR EACH po-ordl
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:                                   

            cScoreOn = IF po-ordl.spare-char-1 = "LENGTH" THEN
                           "Length" 
                       ELSE 
                           "Width".

            lReftableFound = FALSE.
            
            RUN PO_GetLineScoresAndTypes IN hdPOProcs (
                INPUT  po-ordl.company,
                INPUT  po-ordl.po-no,
                INPUT  po-ordl.line,
                OUTPUT dScoreSizeArray,
                OUTPUT cScoreTypeArray
                ).

            /* Scores not found on reftable. Find in panel tables */
            IF dScoreSizeArray[1] EQ 0 THEN           
                RUN GetPanelScoreAndTypeForPO IN hdFormulaProcs (
                    INPUT  cCompany,
                    INPUT  po-ordl.po-no,
                    INPUT  po-ordl.line,
                    INPUT  SUBSTRING(cScoreOn,1,1),
                    OUTPUT dScoreSizeArray,
                    OUTPUT cScoreTypeArray
                    ).
            ELSE
                lReftableFound = TRUE.

            IF dScoreSizeArray[1] EQ 0 THEN
                ASSIGN
                    cStyle     = "2"
                    cStyleDesc = "TRIMMED SHEET"
                    .
            ELSE
                ASSIGN
                    cStyle     = "1"
                    cStyleDesc = "SCORED SHEET"
                    .            
            
            ASSIGN
                lcLineData                  = STRING(APIOutboundDetail.data)
                lcConcatLineAdderData       = ""
                lcConcatLineScoresData      = ""
                lcConcatLineScoresDataGP    = ""
                lcConcatLineAdderDataGP     = ""
                cPoLine                     = STRING(po-ordl.line)
                cQuantityOrdered            = TRIM(STRING(po-ordl.ord-qty,"->>>>>>>>9.9<<<<<"))
                cQuantityUOM                = STRING(po-ordl.pr-qty-uom)
                cItemType                   = STRING(po-ordl.item-type)
                cItemTypeShort              = IF po-ordl.item-type THEN
                                                  "RM"
                                              ELSE
                                                  "FG"
                cItemID                     = STRING(po-ordl.i-no)
                cItemName                   = STRING(po-ordl.i-name)
                cItemDesc1                  = STRING(po-ordl.dscr[1])
                cItemDesc2                  = STRING(po-ordl.dscr[2])
                cItemIDVendor               = STRING(po-ordl.vend-i-no)
                cOverPct                    = TRIM(STRING(po-ordl.over-pct,">>9.99"))
                cUnderPct                   = TRIM(STRING(po-ordl.under-pct,">>9.99"))
                cPoLineDueDate              = STRING(po-ordl.due-date)
                cItemWidth                  = TRIM(STRING(po-ordl.s-wid,">>9.9999"))
                cItemLength                 = TRIM(STRING(po-ordl.s-len,">>9.9999"))
                cItemDepth                  = TRIM(STRING(po-ordl.s-dep,">>9.9999"))
                cItemWidthTrunc             = STRING(TRUNCATE(po-ordl.s-wid,0))
                cItemLengthTrunc            = STRING(TRUNCATE(po-ordl.s-len,0))
                cItemDepthTrunc             = STRING(TRUNCATE(po-ordl.s-dep,0))
                cItemWidth16ths             = TRIM(STRING((po-ordl.s-wid - TRUNCATE(po-ordl.s-wid,0)) * 16))
                cItemLength16ths            = TRIM(STRING((po-ordl.s-len - TRUNCATE(po-ordl.s-len,0)) * 16))
                cItemDepth16ths             = TRIM(STRING((po-ordl.s-dep - TRUNCATE(po-ordl.s-dep,0)) * 16))
                cCostPerUOM                 = TRIM(STRING(po-ordl.cost,"->>>>>>9.99<<<<"))
                cCostUOM                    = STRING(po-ordl.pr-uom)
                cCostSetup                  = STRING(po-ordl.setup)
                cCostDiscount               = TRIM(STRING(po-ordl.disc,"->>>>>9.99"))
                cCustomerID                 = STRING(po-ordl.cust-no)
                cOrderNo                    = STRING(po-ordl.ord-no)
                cPoLineStatus               = STRING(po-ordl.stat)
                cOperationID                = ""
                cQtyPerPack                 = ""
                cPoLineNotes                = ""
                cStackHeight                = "0"
                cPalletWidth                = "0.00"
                cPalletHeight               = "0.00"
                cPalletLength               = "0.00"
                cUnitPallet                 = "0"
                cPurchaseUnit               = STRING(po-ordl.pr-qty-uom)
                cJobID                      = TRIM(STRING(po-ordl.job-no, "X(6)"))
                cJobID2                     = STRING(po-ordl.job-no2, ">9")
                cJobConcat                  = IF po-ordl.job-no EQ "" THEN
                                                  ""
                                              ELSE
                                                  cJobID + "-" + cJobID2
                cJobConcatHRMS              = IF po-ordl.job-no EQ "" THEN
                                                  ""
                                              ELSE
                                                  STRING(po-ordl.job-no, "X(6)") + "-" + STRING(po-ordl.job-no2, "99")                
                cJobIDFormNo                = STRING(po-ordl.s-num)
                cJobIDBlankNo               = STRING(po-ordl.b-num)
                cQuantityReceived           = TRIM(STRING(po-ordl.t-rec-qty, "->>>>>>>>9.9<<<<<"))
                cLineDueDate                = STRING(po-ordl.due-date)
                dCostInMSF                  = po-ordl.cost
                dQuantityInSF               = po-ordl.ord-qty
                cScoreSizeDecimal           = ""
                cScoreSize16ths             = ""
                cItemWithAdders             = "" 
                cItemWithAddersHRMS         = ""
                cFormattedScoresWestrock    = ""
                cScoreSize16thsWestRock     = ""
                cScoreSizeDecimalHRMS1      = ""
                cScoreSizeDecimalHRMS2      = ""
                cCostInMSF                  = ""
                cQuantityInSF               = ""
                dItemBasisWeight            = 0 
                cScoreSizeDecimalAlliFlutes = ""
                cItemWithAddersX4           = ""
                cItemWithAddersX10          = ""
                .

            FIND FIRST item NO-LOCK
                 WHERE item.company  EQ po-ordl.company
                   AND item.i-no     EQ po-ordl.i-no
                   AND item.mat-type EQ "B"
                 NO-ERROR.

            IF AVAILABLE item THEN
                ASSIGN
                    dItemBasisWeight = item.basis-w
                    cItemBasisWeight = STRING(item.basis-w)
                    cFlute           = item.flute   
                    .                

            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ po-ordl.company
                   AND itemfg.i-no    EQ po-ordl.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg AND NOT po-ordl.item-type THEN
                ASSIGN
                    cQtyPerPack   = TRIM(STRING(itemfg.case-count))
                    cStackHeight  = TRIM(STRING(itemfg.stackHeight))
                    cPalletWidth  = TRIM(STRING(itemfg.unitWidth, ">>>>9.99"))
                    cPalletHeight = TRIM(STRING(itemfg.unitHeight, ">>>>9.99"))
                    cPalletLength = TRIM(STRING(itemfg.unitLength, ">>>>9.99"))
                    cUnitPallet   = TRIM(STRING(itemfg.case-pall))
                    .
            
            IF po-ordl.pr-qty-uom NE "EA" THEN         
                RUN Conv_QtyToEA (
                    INPUT  po-ordl.company,
                    INPUT  po-ordl.i-no,
                    INPUT  po-ordl.ord-qty,
                    INPUT  po-ordl.pr-qty-uom,
                    INPUT  IF AVAILABLE itemfg THEN itemfg.case-count ELSE 0,
                    OUTPUT dQuantityInEA
                    ).
            ELSE 
                dQuantityInEA = po-ordl.ord-qty.        

            IF dQuantityInEA - TRUNCATE(dQuantityInEA,0) GT 0 THEN
                dQuantityInEA = TRUNCATE(dQuantityInEA,0) + 1.

            IF dQuantityInEA GT 99999999 THEN 
                dQuantityInEA = 99999999.
                 
            ASSIGN 
                cQtyInEA   = TRIM(STRING(dQuantityInEA,"->>>>>>>>9")) 
                cPoLowQty  = TRIM(STRING(dQuantityInEA * (1 - (po-ordl.under-pct / 100)),"->>>>>>>>9"))
                cPoHighQty = TRIM(STRING(dQuantityInEA * (1 + (po-ordl.over-pct / 100)),"->>>>>>>>9")) 
                .
                            
            IF po-ordl.pr-uom NE "MSF" THEN
                RUN Conv_ValueFromUOMToUOM (
                    INPUT  po-ordl.company,
                    INPUT  po-ordl.i-no,
                    INPUT  cItemTypeShort,
                    INPUT  po-ordl.cost,
                    INPUT  po-ordl.pr-uom, 
                    INPUT  "MSF",
                    INPUT  dItemBasisWeight,
                    INPUT  po-ordl.s-len,
                    INPUT  po-ordl.s-wid,
                    INPUT  po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dCostInMSF,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
                
            cCostInMSF = STRING(dCostInMSF).    

            IF po-ordl.pr-qty-uom NE "SF" THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  po-ordl.company,
                    INPUT  po-ordl.i-no,
                    INPUT  cItemTypeShort,
                    INPUT  po-ordl.ord-qty,
                    INPUT  po-ordl.pr-qty-uom, 
                    INPUT  "SF",
                    INPUT  dItemBasisWeight,
                    INPUT  po-ordl.s-len,
                    INPUT  po-ordl.s-wid,
                    INPUT  po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityInSF,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).

            IF dQuantityInSF - TRUNCATE(dQuantityInSF,0) GT 0 THEN
                dQuantityInSF = TRUNCATE(dQuantityInSF,0) + 1.

            cQuantityInSF = STRING(dQuantityInSF).                
            
            IF dQuantityInEA NE 0 THEN
                dQuantityInM = dQuantityInSF / (dQuantityInEA / 1000).
            
            cQuantityInM = STRING(dQuantityInM).
            
            /* Fetch first operation id (job-mch.m-code) for the order line */
            IF TRIM(po-ordl.job-no) NE "" THEN
                RUN GetOperation IN hdJobProcs (
                    INPUT        po-ordl.company,
                    INPUT        po-ordl.job-no,
                    INPUT        INTEGER(po-ordl.job-no2),
                    INPUT        INTEGER(po-ordl.s-num),
                    INPUT        "First",
                    INPUT-OUTPUT cOperationID
                    ).

            /* Fetch purchase order notes from notes table */    
            FOR EACH notes NO-LOCK
                WHERE notes.rec_key EQ po-ordl.rec_key:
                cPoLineNotes = cPoLineNotes + " " + STRING(notes.note_text).
            END.
            
            cPoLineNotes = REPLACE(cPoLineNotes, "~n", "").
            
            cPoNotesHRMS = cPoNotesHRMS + " " + cPoLineNotes.
            
            IF cPoLineNotes EQ "" THEN
                cPoLineNotes = cPoNotes.
            
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poID", cPoNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLine", cPoLine).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLineStatus", cPoLineStatus).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityOrdered", cQuantityOrdered).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityInSF", cQuantityInSF).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityInM", cQuantityInM).            
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
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "costInMSF", cCostInMSF).
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
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "custID", cAssignedCustID).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "lineDueDate", cLineDueDate).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWidthTrunc", cItemWidthTrunc).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLengthTrunc", cItemLengthTrunc).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDepthTrunc", cItemDepthTrunc).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWidth16ths", cItemWidth16ths).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLength16ths", cItemLength16ths).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDepth16ths", cItemDepth16ths).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "style", cStyle).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "styleDesc", cStyleDesc).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemBasisWeight", cItemBasisWeight).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "flute", cFlute).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobConcat", cJobConcat).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "jobConcatHRMS", cJobConcatHRMS).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poLineNotes", cPoLineNotes).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poNotesHRMS", cPoNotesHRMS).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "SPACE", " ").
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "poNo",cPoNo).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "shipToAddress1", cshipToAddress1).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "shipToAddress2", cshipToAddress2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "shipToCity", cShipToCity).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "ShipToName", cShipToName).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "ShipToCompanyName", cShipToCompanyName).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "shipToState", cShipToState).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "shipToZip", cShipToZip).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "quantityOrderedInEA", cQtyInEA).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoLowQty", cPoLowQty).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoHighQty", cPoHighQty).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "CurrentDate",cCurrentDateTime).  
            cItemWithAdders = cItemID.
            /* Fetch adder details for the purchase order line */
            FOR EACH po-ordl-add NO-LOCK
               WHERE po-ordl-add.company EQ po-ordl.company
                 AND po-ordl-add.po-no   EQ po-ordl.po-no  
                 AND po-ordl-add.line    EQ po-ordl.line:
                IF AVAILABLE bf-APIOutboundDetail1 THEN DO:                                
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
                                
                    lcConcatLineAdderData = lcConcatLineAdderData + lcLineAdderData.
                END.
                
                cItemWithAdders = cItemWithAdders + ", " + po-ordl-add.adder-i-no.
            END.
            
            /* Fetch Adders for HRMS */
            FIND FIRST job NO-LOCK 
                 WHERE job.company EQ po-ordl.company
                   AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) + TRIM(po-ordl.job-no) 
                   AND job.job-no2 EQ po-ordl.job-no2
                 NO-ERROR.            
            IF AVAILABLE job THEN DO:
                FOR EACH job-mat NO-LOCK 
                    WHERE job-mat.company  EQ job.company 
                      AND job-mat.job      EQ job.job 
                      AND job-mat.job-no   EQ job.job-no 
                      AND job-mat.job-no2  EQ job.job-no2 
                      AND job-mat.i-no     EQ po-ordl.i-no 
                      AND job-mat.frm      EQ po-ordl.s-num
                    USE-INDEX job
                    BREAK BY job-mat.blank-no DESCENDING:
                    IF LAST(job-mat.blank-no) OR job-mat.blank-no EQ po-ordl.b-num THEN
                        LEAVE.
                END.
    
                IF AVAILABLE job-mat THEN DO:
                    FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "util/b-hrms-x.w"
                           AND reftable.company  EQ po-ordl.company
                           AND reftable.code2    EQ po-ordl.i-no
                         NO-ERROR.
                    IF AVAILABLE reftable THEN
                        cItemWithAddersHRMS = STRING(INT(reftable.code),"9999") NO-ERROR.
                    ELSE 
                        cItemWithAddersHRMS = STRING("0000","X(4)").

                    ASSIGN 
                        iIndex  = 1
                        iIndex1 = 0
                        .

                    FOR EACH bf-job-mat NO-LOCK 
                        WHERE bf-job-mat.company  EQ job-mat.company
                          AND bf-job-mat.job      EQ job-mat.job 
                          AND bf-job-mat.job-no   EQ job-mat.job-no 
                          AND bf-job-mat.job-no2  EQ job-mat.job-no2 
                          AND bf-job-mat.frm      EQ job-mat.frm 
                          AND bf-job-mat.blank-no EQ job-mat.blank-no 
                          AND bf-job-mat.i-no     NE job-mat.i-no,
                        FIRST bf-item NO-LOCK 
                        WHERE bf-item.company  EQ bf-job-mat.company 
                          AND bf-item.i-no     EQ bf-job-mat.i-no 
                          AND bf-item.mat-type EQ "A":                             
                        FIND FIRST bf-hrms-reftable NO-LOCK 
                             WHERE bf-hrms-reftable.reftable EQ "util/b-hrms-x.w" 
                               AND bf-hrms-reftable.company  EQ bf-item.company 
                               AND bf-hrms-reftable.code2    EQ bf-item.i-no
                               NO-ERROR.        
                        IF AVAILABLE bf-hrms-reftable THEN DO:
                            iIndex = iIndex + 1.
                            IF iIndex LE 6 THEN DO:            
                                cItemWithAddersHRMS = cItemWithAddersHRMS + STRING(INT(bf-hrms-reftable.code),"9999") NO-ERROR.
                                IF ERROR-STATUS:ERROR THEN 
                                    cItemWithAddersHRMS = cItemWithAddersHRMS + STRING("0000","9999").
                            END.
                        END.
                       iIndex1 = iIndex1 + 1.
                        IF iIndex1 LE 6 THEN DO:
                            IF AVAILABLE bf-APIOutboundDetail1 AND bf-item.i-no NE ""  THEN DO:                                
                                lcLineAdderDataGP = STRING(bf-APIOutboundDetail1.data).
                                RUN updateRequestData(INPUT-OUTPUT lcLineAdderDataGP, "ItemWithaddersX10WithoutConcat", STRING(bf-item.i-no,"X(10)")).
                                lcConcatLineAdderDataGP = lcConcatLineAdderDataGP + lcLineAdderDataGP.    
                                                              
                            END.    
                            ASSIGN  
                                cItemWithAddersX4  = cItemWithAddersX4 + STRING(bf-item.i-no,"X(4)")
                                cItemWithAddersX10 = cItemWithAddersX10 + STRING(bf-item.i-no,"X(10)")
                                .  
                        END.           
                    END.                                 
                END.
            END.
            RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatLineAdderData, cRequestDataType).
            
            DO iIndex = 1 TO 20:
                IF dScoreSizeArray[iIndex] EQ 0 THEN
                    LEAVE.
                
                ASSIGN
                    cScoreSize = STRING(dScoreSizeArray[iIndex])
                    cScoreType = cScoreTypeArray[iIndex]
                    .
                
                IF AVAILABLE bf-APIOutboundDetail2 THEN DO:
                    lcLineScoresData = STRING(bf-APIOutboundDetail2.data).
                    
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreOn", cScoreOn).
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreSize", cScoreSize).
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresData, "scoreType", cScoreType).
                    
                    lcConcatLineScoresData = lcConcatLineScoresData + lcLineScoresData.
                END.

                IF lReftableFound THEN DO:                    
                    RUN SwitchPanelSizeFormat IN hdFormulaProcs (
                        INPUT  cSizeFormat,
                        INPUT  "16th's",
                        INPUT  dScoreSizeArray[iIndex],
                        OUTPUT dScoreSize16ths
                        ).
                        
                    RUN SwitchPanelSizeFormat IN hdFormulaProcs (
                        INPUT  cSizeFormat,
                        INPUT  "Decimal",
                        INPUT  dScoreSizeArray[iIndex],
                        OUTPUT dScoreSizeDecimal
                        ).
                END.                
                ELSE DO:
                    RUN SwitchPanelSizeFormat IN hdFormulaProcs (
                        INPUT  "Decimal",
                        INPUT  "16th's",
                        INPUT  dScoreSizeArray[iIndex],
                        OUTPUT dScoreSize16ths
                        ).
                        
                    dScoreSizeDecimal = dScoreSizeArray[iIndex].
                END.
                IF AVAILABLE bf-APIOutboundDetail2 AND dScoreSize16ths NE 0 THEN DO:
                    lcLineScoresDataGP = STRING(bf-APIOutboundDetail2.data).
                    
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresDataGP, "scoreType", cScoreType).
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresDataGP, "ScoreSequence", STRING(iIndex)).
                    RUN updateRequestData(INPUT-OUTPUT lcLineScoresDataGP, "scoreSize16thsWithoutConcat", STRING(dScoreSizeDecimal)).
                    
                    lcConcatLineScoresDataGP = lcConcatLineScoresDataGP + lcLineScoresDataGP.
                    
                END.     
                ASSIGN
                    cScoreSizeDecimal       = IF cScoreSizeDecimal EQ "" THEN 
                                                  TRIM(STRING(dScoreSizeDecimal, ">>>>>>>9.99<<<<"))
                                              ELSE
                                                  cScoreSizeDecimal + " X " + TRIM(STRING(dScoreSizeDecimal, ">>>>>>>9.99<<<<"))                                
                    cScoreSize16ths         = IF cScoreSize16ths EQ "" THEN 
                                                TRIM(STRING(dScoreSize16ths, ">>>>>>>9.99<<<<"))
                                              ELSE
                                                  cScoreSize16ths + " X " + TRIM(STRING(dScoreSize16ths, ">>>>>>>9.99<<<<"))
                    
                    cScoreSize16thsWestRock = IF cScoreSize16thsWestrock EQ "" THEN 
                                                  TRIM(STRING(dScoreSize16ths, ">>>>>>>9.99<<<<"))
                                              ELSE
                                                  cScoreSize16thsWestrock + " x " + TRIM(STRING(dScoreSize16ths, ">>>>>>>9.99<<<<"))
                    cFormattedScoresWestrock = IF cFormattedScoresWestrock EQ "" THEN 
                                                   TRIM(STRING(dScoreSize16ths, ">>>>>9999.99<<<<"))
                                               ELSE
                                                   cFormattedScoresWestrock + "x "  + TRIM(STRING(dScoreSize16ths, ">>>>>9999.99<<<<"))
                    .
                
                    
                    cScoreSizeDecimalAlliFlutes = IF cScoreSizeDecimalAlliFlutes EQ "" THEN 
                                                    (STRING(dScoreSize16ths, ">>>.99")) + cScoreType
                                                 ELSE
                                                    cScoreSizeDecimalAlliFlutes + (STRING(dScoreSize16ths, ">>>.99")) + cScoreType
                                                 .                                               
                IF iIndex LE 9 THEN 
                    cScoreSizeDecimalHRMS1 = IF cScoreSizeDecimalHRMS1 EQ "" THEN 
                                                 (STRING(dScoreSize16ths, ">>>.99")) + cScoreType
                                             ELSE
                                                 cScoreSizeDecimalHRMS1 + (STRING(dScoreSize16ths, ">>>.99")) + cScoreType.
                ELSE
                    cScoreSizeDecimalHRMS2 = IF cScoreSizeDecimalHRMS2 EQ "" THEN 
                                                 (STRING(dScoreSize16ths, ">>>.99")) + cScoreType
                                             ELSE
                                                cScoreSizeDecimalHRMS2 + (STRING(dScoreSize16ths, ">>>.99")) + cScoreType.                
            END.
            
            ASSIGN 
                cFormattedScoresWestrock    = REPLACE(cFormattedScoresWestrock,".", "")
                cScoreSize16ths             = REPLACE(cScoreSize16ths,".", ":")
                cScoreSizeDecimalHRMS1      = REPLACE(cScoreSizeDecimalHRMS1,".",":")
                cScoreSizeDecimalHRMS2      = REPLACE(cScoreSizeDecimalHRMS2,".",":")
                cScoreSizeDecimalAlliFlutes = REPLACE(cScoreSizeDecimalAlliFlutes,".",":")
                .
            
            RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatLineScoresData, cRequestDataType).            
            
            lcLineData = REPLACE(lcLineData, "$lineAdder$", lcConcatLineAdderData).
            
            lcLineData = REPLACE(lcLineData, "$lineScores$", lcConcatLineScoresData).
            
            lcLineData =  REPLACE(lcLineData, "$LineAdderGP$", lcConcatLineAdderDataGP).
            
            lcLineData =  REPLACE(lcLineData, "$lineScoresGP$", lcConcatLineScoresDataGP).

            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSize16ths", cScoreSize16ths).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSizeDecimal", cScoreSizeDecimal).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWithAdders", cItemWithAdders).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWithAddersHRMS", cItemWithAddersHRMS).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSize16thsWestrock", cScoreSize16thsWestrock).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSizeDecimalHRMS1", cScoreSizeDecimalHRMS1).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSizeDecimalHRMS2", cScoreSizeDecimalHRMS2).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "FormattedScoring", cFormattedScoresWestrock).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "scoreSizeDecimalAlliFlutes", cScoreSizeDecimalAlliFlutes).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWithAddersX4", cItemWithAddersX4).
            RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemWithAddersX10", cItemWithAddersX10).
            
            lcConcatLineData = lcConcatLineData + lcLineData.
        END.      
        
        RUN pUpdateDelimiter (INPUT-OUTPUT lcConcatLineData, cRequestDataType).

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
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "shipToCityState", cShipToCityState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "carrierID", cCarrierID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "carrierDesc", cCarrierDesc).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "freightTerms", cFreightTerms).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "freightFOB", cFreightFOB).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "buyer", cBuyer).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poNotes", cPoNotes).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "fobCode", cFOBCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custAreaCode", cCustAreaCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custPhone", cCustPhone).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custID", cAssignedCustID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custName", cCustName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custAddress1", cCustAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custAddress2", cCustAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custCity", cCustCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custState", cCustState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custZip", cCustZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custCityState", cCustCityState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXAreaCode", cCustXAreaCode).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXPhone", cCustXPhone).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXName", cCustXName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXAddress1", cCustXAddress1).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXAddress2", cCustXAddress2).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXCity", cCustXCity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXState", cCustXState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXZip", cCustXZip).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "custXCityState", cCustXCityState).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SPACE", " ").
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipToCompanyName", cShipToCompanyName).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrentDate",STRING(TODAY)).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalCost",cTotalCost). 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GPPartnerID",cGPPartnerID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GPPurchasedBy",cGPPurchasedBy).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GPPlantID",cGPPlantID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GPBillto",cGPBillto).  
        
        
        /* This replace is required for replacing nested JSON data */
        ioplcRequestData = REPLACE(ioplcRequestData, "$detail$", lcConcatLineData).
        
        RELEASE bf-APIOutboundDetail1.
        RELEASE bf-APIOutboundDetail2.

        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
    
    
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE PROCEDURE hdJobProcs.

    IF VALID-HANDLE(hdFormulaProcs) THEN
        DELETE PROCEDURE hdFormulaProcs.

    IF VALID-HANDLE(hdPOProcs) THEN
        DELETE PROCEDURE hdPOProcs.

        