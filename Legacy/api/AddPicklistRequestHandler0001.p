/*------------------------------------------------------------------------
    File        : api/AddPicklistRequestHandler0001.p
    Purpose     : Returns the request data for picklist addition

    Syntax      :

    Description : Returns the request data for picklist addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    
    DEFINE INPUT        PARAMETER TABLE            FOR ttArgs.  
    DEFINE INPUT        PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcDetailData       AS   LONGCHAR   NO-UNDO.
    DEFINE VARIABLE lcConcatDetailData AS   LONGCHAR   NO-UNDO.
    DEFINE VARIABLE salesUnit          AS   CHARACTER  NO-UNDO  INITIAL  "Each".
    DEFINE VARIABLE lotNumber          AS   CHARACTER  NO-UNDO  INITIAL  "190002".
    DEFINE VARIABLE docType            AS   CHARACTER  NO-UNDO  INITIAL  "SO".
    DEFINE VARIABLE docStatus          AS   CHARACTER  NO-UNDO  INITIAL  "1".
    DEFINE VARIABLE reqDate            AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".
    DEFINE VARIABLE numatCard          AS   CHARACTER  NO-UNDO  INITIAL  "987".
    DEFINE VARIABLE docDueDate         AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:0:00".
    DEFINE VARIABLE routeID            AS   CHARACTER  NO-UNDO  INITIAL  "MON01".
    DEFINE VARIABLE cCalcAddress       AS   CHARACTER  NO-UNDO.
    DEFINE VARIABLE cCardName          AS   CHARACTER  NO-UNDO.
    DEFINE VARIABLE slpCode            AS   CHARACTER  NO-UNDO  INITIAL  "898".
    DEFINE VARIABLE priority           AS   CHARACTER  NO-UNDO  INITIAL  "2".
    
    FIND FIRST APIOutboundDetail NO-LOCK
         WHERE APIOutboundDetail.detailID EQ "detail"
           AND APIOutboundDetail.parentID EQ ipcParentID
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
        
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no:        
         ASSIGN
            lcDetailData = STRING(APIOutboundDetail.data)
            lcDetailData = REPLACE(lcDetailData, "TBD7", lotNumber)
            lcDetailData = REPLACE(lcDetailData, "TBD6", salesUnit)
            lcDetailData = REPLACE(lcDetailData, "TBD5", STRING(oe-rell.qty-case))
            lcDetailData = REPLACE(lcDetailData, "TBD4", oe-rell.loc)
            lcDetailData = REPLACE(lcDetailData, "TBD3", STRING(oe-rell.qty))
            lcDetailData = REPLACE(lcDetailData, "TBD2", STRING(oe-rell.i-no))
            lcDetailData = REPLACE(lcDetailData, "TBD1", STRING(oe-rell.line))
            .
            
        lcConcatDetailData = lcConcatDetailData + "," + lcDetailData.
    END.      
    
    lcConcatDetailData = TRIM(lcConcatDetailData,",").
    
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ oe-relh.company
           AND cust.cust-no EQ oe-relh.cust-no 
         NO-ERROR.
    IF AVAILABLE cust THEN
        cCardName = cust.name.

    FIND FIRST shipto NO-LOCK
         WHERE shipto.company = oe-relh.company
           AND shipto.cust-no = oe-relh.cust-no
           AND shipto.ship-no = oe-relh.ship-no 
         NO-ERROR.        
    IF AVAILABLE shipto THEN
        cCalcAddress     = shipto.ship-addr[1] + " " + shipto.ship-addr[2] + " " + shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip.
    
    ASSIGN
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD16", lcDetailData)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD15", priority)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD14", slpCode)        
        ioplcRequestData = REPLACE(ioplcRequestData, "calcAddress", cCalcAddress)
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.ship-id", STRING(oe-relh.ship-id))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD11", docDueDate)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD10", numatCard)
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD9", cCardName)
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.cust-no", STRING(oe-relh.cust-no))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD7", reqDate)
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.rel-date", STRING(oe-relh.rel-date))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD5", docStatus)
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.release#", STRING(oe-relh.release#))
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.r-no", STRING(oe-relh.r-no))
        ioplcRequestData = REPLACE(ioplcRequestData, "TBD2", docType)
        ioplcRequestData = REPLACE(ioplcRequestData, "oe-relh.company", oe-relh.company)
        .
    
    ASSIGN
        opcMessage = ""
        oplSuccess = TRUE
        .     
