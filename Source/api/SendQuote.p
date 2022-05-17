/*------------------------------------------------------------------------
    File        : api/SendQuote.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Apr 20 12:46:35 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/
{api/ttArgs.i}
{api/CommonAPIProcs.i}
{est/ttQuoteMaster.i}    
{est/ttQuoteHeader.i}
{est/ttQuoteItem.i}
{est/ttQuoteQuantity.i}
{est/ttQuoteMisc.i}

DEFINE TEMP-TABLE tt-quote 
    FIELD row-id  AS ROWID
    FIELD tt-seq  AS INTEGER   INIT 999999999
    FIELD cust-no AS CHARACTER
    INDEX row-id row-id
    INDEX tt-seq tt-seq
    .
    
DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdQuoteProcs               AS HANDLE   NO-UNDO.
DEFINE VARIABLE mptrTTQuote                AS MEMPTR   NO-UNDO.
DEFINE VARIABLE hdTTHandle                 AS HANDLE   NO-UNDO.
DEFINE VARIABLE lcReportHeader             AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcReportFooter             AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageHeader               AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageFooter               AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteHeaderGroupHeader   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteHeaderGroupFooter   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteHeader              AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteItemGroupHeader     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteItemGroupFooter     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteItem                AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteQuantityGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteQuantityGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteQuantity            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteMiscGroupHeader     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteMiscGroupFooter     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteMisc                AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcQuoteHeaderData   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteItemData     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteQuantityData AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcQuoteMiscData     AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcConcatQuoteHeader   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatQuoteItem     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatQuoteQuantity AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatQuoteMisc     AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcBoxDesignHeader       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcBoxDesignHeaderData   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatBoxDesignHeader AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcBoxDesignLine       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcBoxDesignLineData   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatBoxDesignLine AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcSetComponent       AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcSetComponentData   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatSetComponent AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lAvailQuoteHeader   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAvailQuoteItem     AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAvailQuoteQuantity AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAvailQuoteMisc     AS LOGICAL NO-UNDO.

DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iNumLinesInPage AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValid          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.

DEFINE VARIABLE cBuisnessFormLogo           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirstQuote                 AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE lFirstItem                  AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE lFirstQuantity              AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE lFirstMisc                  AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE lPrint2ndItemDescription    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintBoxDesign             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintSetComponents         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFirstComponent             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lLastCustomer               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iItemQuantityCount          AS INTEGER   NO-UNDO.
DEFINE VARIABLE dLengthMetric               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dWidthMetric                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDepthMetric                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtendedPrice              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cItemID                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAdders                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBoxDesignImage             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWidthScoreMetric           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWidthCumulativeScoreMetric AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-quotehd  FOR quotehd.
DEFINE BUFFER bf-quoteitm FOR quoteitm.
DEFINE BUFFER bf-quoteqty FOR quoteqty.
DEFINE BUFFER bf-quotechg FOR quotechg.
DEFINE BUFFER bf-eb       FOR eb.

RUN est/QuoteProcs.p PERSISTENT SET hdQuoteProcs.

RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN pGetNumLinesInPage(ipiAPIOutboundID, OUTPUT iNumLinesInPage).

ASSIGN
    lPrint2ndItemDescription = LOGICAL(system.SharedConfig:Instance:GetValue("SendQuote_Print2ndItemDescription"))
    lPrintBoxDesign          = LOGICAL(system.SharedConfig:Instance:GetValue("SendQuote_PrintBoxDesign"))
    lPrintSetComponents      = LOGICAL(system.SharedConfig:Instance:GetValue("SendQuote_PrintSetComponents"))
    NO-ERROR.
    
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
           AND ttArgs.argKey   = "TTQuote"
         NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid temp-table handle"
            .
        
        RETURN.
    END.
    
    hdTTHandle = HANDLE(ttArgs.argValue).
    
    IF NOT VALID-HANDLE (hdTTHandle) THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid temp-table handle"
            .
        
        RETURN.        
    END.

    oAttribute = NEW system.Attribute().
    oAttribute:RequestDataType = gcRequestDataType.

    RUN sys/ref/nk1look.p (cCompany, "BusinessFormLogo", "C", NO, YES, "", "", OUTPUT cBuisnessFormLogo, OUTPUT lRecFound).
    
    /* Code to send data from dynamic temp-table handle to static temp-table */
    hdTTHandle:WRITE-XML("MEMPTR", mptrTTQuote).
    
    TEMP-TABLE tt-quote:READ-XML("MEMPTR", mptrTTQuote, "EMPTY", ?, FALSE).

    RUN pGetRequestData ("QuoteHeader", OUTPUT lcQuoteHeaderData).
    RUN pGetRequestData ("QuoteHeaderGroupHeader", OUTPUT lcQuoteHeaderGroupHeader).
    RUN pGetRequestData ("QuoteHeaderGroupFooter", OUTPUT lcQuoteHeaderGroupFooter).

    RUN pGetRequestData ("QuoteItem", OUTPUT lcQuoteItemData).
    RUN pGetRequestData ("QuoteItemGroupHeader", OUTPUT lcQuoteItemGroupHeader).
    RUN pGetRequestData ("QuoteItemGroupFooter", OUTPUT lcQuoteItemGroupFooter).

    RUN pGetRequestData ("QuoteQuantity", OUTPUT lcQuoteQuantityData).
    RUN pGetRequestData ("QuoteQuantityGroupHeader", OUTPUT lcQuoteQuantityGroupHeader).
    RUN pGetRequestData ("QuoteQuantityGroupFooter", OUTPUT lcQuoteQuantityGroupFooter).

    RUN pGetRequestData ("QuoteMisc", OUTPUT lcQuoteMiscData).
    RUN pGetRequestData ("QuoteMiscGroupHeader", OUTPUT lcQuoteMiscGroupHeader).
    RUN pGetRequestData ("QuoteMiscGroupFooter", OUTPUT lcQuoteMiscGroupFooter).

    RUN pGetRequestData ("BoxDesignHeader", OUTPUT lcBoxDesignHeaderData).
    RUN pGetRequestData ("BoxDesignLine", OUTPUT lcBoxDesignLineData).
    
    RUN pGetRequestData ("SetComponent", OUTPUT lcSetComponentData).
    
    RUN pGetRequestData ("PageFooter", OUTPUT lcPageFooter).

    ASSIGN
        lAvailQuoteHeader   = FALSE
        lAvailQuoteItem     = FALSE
        lAvailQuoteQuantity = FALSE
        lAvailQuoteMisc     = FALSE
        .
                    
    FOR EACH tt-quote
        BREAK BY tt-quote.cust-no:
        lLastCustomer = LAST-OF(tt-quote.cust-no).
                        
        FIND FIRST bf-quotehd NO-LOCK
             WHERE ROWID(bf-quotehd) EQ tt-quote.row-id 
             NO-ERROR.
        IF NOT AVAILABLE bf-quotehd THEN DO:
            ASSIGN
                opcMessage = "Invalid quotehd ROWID"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        IF lFirstQuote EQ ? THEN
            lFirstQuote = TRUE.
        ELSE IF lFirstQuote EQ TRUE THEN
            lFirstQuote = FALSE.
            
        RUN Quote_BuildQuote IN hdQuoteProcs (
            INPUT  bf-quotehd.company,
            INPUT  bf-quotehd.q-no,
            INPUT-OUTPUT TABLE ttQuoteMaster BY-REFERENCE,
            INPUT-OUTPUT TABLE ttQuoteHeader BY-REFERENCE,
            INPUT-OUTPUT TABLE ttQuoteItem BY-REFERENCE,
            INPUT-OUTPUT TABLE ttQuoteQuantity BY-REFERENCE,
            INPUT-OUTPUT TABLE ttQuoteMisc BY-REFERENCE
            ).
        
        FOR EACH ttQuoteHeader,
            FIRST bf-quotehd NO-LOCK
            WHERE ROWID(bf-quotehd) EQ ttQuoteHeader.riQuotehd:
            
            ASSIGN
                lcQuoteHeader     = lcQuoteHeaderData
                lcConcatQuoteItem = ""
                lFirstQuote       = ?
                .
            
            FIND FIRST est NO-LOCK
                 WHERE est.company EQ ttQuoteHeader.company
                   AND est.est-no  EQ ttQuoteHeader.estimateID 
                 NO-ERROR.
            FIND FIRST sman NO-LOCK 
                 WHERE sman.company EQ ttQuoteHeader.company
                   AND sman.sman    EQ ttQuoteHeader.salesMan
                 NO-ERROR.
            FIND FIRST carrier NO-LOCK
                 WHERE carrier.company EQ ttQuoteHeader.company
                   AND carrier.carrier EQ ttQuoteHeader.carrier
                 NO-ERROR.
            FIND FIRST terms NO-LOCK
                 WHERE terms.company EQ ttQuoteHeader.company
                   AND terms.t-code  EQ ttQuoteHeader.terms
                 NO-ERROR.
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ ttQuoteHeader.company
                   AND cust.cust-no EQ ttQuoteHeader.customerID
                 NO-ERROR.

            FOR EACH ttQuoteItem
                WHERE ttQuoteItem.company    EQ ttQuoteHeader.company
                  AND ttQuoteItem.locationID EQ ttQuoteHeader.locationID
                  AND ttQuoteItem.quoteID    EQ ttQuoteHeader.quoteID,
                FIRST bf-quoteitm NO-LOCK
                WHERE ROWID(bf-quoteitm) EQ ttQuoteItem.riQuoteitm:

                FIND FIRST eb NO-LOCK
                     WHERE eb.company EQ ttQuoteItem.company
                       AND eb.est-no  EQ ttQuoteHeader.estimateID
                       AND eb.part-no EQ ttQuoteItem.partID
                       AND eb.form-no NE 0
                     NO-ERROR.
                IF NOT AVAILABLE eb THEN
                    FIND FIRST eb NO-LOCK
                         WHERE eb.company EQ ttQuoteItem.company
                           AND eb.est-no  EQ ttQuoteHeader.estimateID
                           AND eb.form-no NE 0
                         NO-ERROR.

                FIND FIRST style NO-LOCK
                     WHERE style.company EQ ttQuoteItem.company
                       AND style.style   EQ ttQuoteItem.style  
                     NO-ERROR.
                                                                         
                IF lFirstItem EQ ? THEN
                    lFirstItem = TRUE.
                ELSE IF lFirstItem EQ TRUE THEN
                    lFirstItem = FALSE.
                
                ASSIGN
                    lFirstQuantity        = ?
                    lcConcatQuoteQuantity = ""
                    lcQuoteItem           = lcQuoteItemData
                    iItemQuantityCount    = 0
                    .

                IF AVAILABLE est AND est.est-type EQ 6 THEN
                    FIND FIRST bf-eb NO-LOCK 
                         WHERE bf-eb.company EQ ttQuoteHeader.company
                           AND bf-eb.est-no  EQ ttQuoteHeader.estimateID
                           AND bf-eb.form-no EQ 0
                         NO-ERROR.

                    cItemID = IF AVAILABLE est AND est.est-type EQ 6 AND AVAILABLE bf-eb THEN 
                                  bf-eb.stock-no
                              ELSE IF AVAILABLE eb THEN 
                                  eb.stock-no
                              ELSE 
                                  ttQuoteItem.itemID.
                                                      
                FOR EACH ttQuoteQuantity
                    WHERE ttQuoteQuantity.company    EQ ttQuoteItem.company
                      AND ttQuoteQuantity.locationID EQ ttQuoteItem.locationID
                      AND ttQuoteQuantity.quoteID    EQ ttQuoteItem.quoteID
                      AND ttQuoteQuantity.lineID     EQ ttQuoteItem.lineID,
                    FIRST bf-quoteqty NO-LOCK
                    WHERE ROWID(bf-quoteqty) EQ ttQuoteQuantity.riQuoteqty:

                    IF lFirstQuantity EQ ? THEN
                        lFirstQuantity = TRUE.
                    ELSE IF lFirstQuantity EQ TRUE THEN
                        lFirstQuantity = FALSE.
                    
                    ASSIGN
                        lFirstMisc         = ?
                        lcConcatQuoteMisc  = ""
                        lcQuoteQuantity    = lcQuoteQuantityData
                        iItemQuantityCount = iItemQuantityCount + 1 
                        .
                        
                    FOR EACH ttQuoteMisc
                        WHERE ttQuoteMisc.company    EQ ttQuoteQuantity.company
                          AND ttQuoteMisc.locationID EQ ttQuoteQuantity.locationID
                          AND ttQuoteMisc.quoteID    EQ ttQuoteQuantity.quoteID
                          AND ttQuoteMisc.lineID     EQ ttQuoteQuantity.lineID
                          AND ttQuoteMisc.quantity   EQ ttQuoteQuantity.quantity,
                        FIRST bf-quotechg NO-LOCK
                        WHERE ROWID(bf-quotechg) EQ ttQuoteItem.riQuoteitm:
                        
                        IF lFirstMisc EQ ? THEN
                            lFirstMisc = TRUE.
                        ELSE IF lFirstMisc EQ TRUE THEN
                            lFirstMisc = FALSE.
                        
                        ASSIGN
                            lcQuoteMisc = lcQuoteMiscData
                            .
                        
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteMisc, "FirstQuoteMisc", STRING(lFirstMisc)).
                        
                        lcQuoteMisc = oAttribute:ReplaceAttributes(lcQuoteMisc, BUFFER bf-quotechg:HANDLE).
                        
                        lcConcatQuoteMisc = lcConcatQuoteMisc + lcQuoteMisc. 
                    END.

                    RUN pGetExtendedPrice (
                        BUFFER bf-quoteqty,
                        BUFFER eb,
                        INPUT  ttQuoteHeader.estimateID,
                        INPUT  IF AVAILABLE est THEN est.est-type ELSE 0,
                        INPUT  cItemID,
                        OUTPUT dExtendedPrice
                        ).

                    lcQuoteQuantity = REPLACE(lcQuoteQuantity, "$QuoteMiscs$", lcConcatQuoteMisc).

                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteQuantity, "FirstQuoteQuantity", STRING(lFirstQuantity)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteQuantity, "ExtendedPrice", STRING(dExtendedPrice)).
                    
                    lcQuoteQuantity = oAttribute:ReplaceAttributes(lcQuoteQuantity, BUFFER eb:HANDLE).
                    lcQuoteQuantity = oAttribute:ReplaceAttributes(lcQuoteQuantity, BUFFER bf-quoteqty:HANDLE).
                    
                    lcConcatQuoteQuantity = lcConcatQuoteQuantity + lcQuoteQuantity. 
                END.

                IF AVAILABLE eb THEN DO:
                    ASSIGN
                        dLengthMetric = eb.len * 25.4
                        dWidthMetric  = eb.wid * 25.4
                        dDepthMetric  = eb.dep * 25.4
                        .

                    {sys/inc/roundup.i dLengthMetric}
                    {sys/inc/roundup.i dWidthMetric}
                    {sys/inc/roundup.i dDepthMetric}
                END.
                
                lcConcatSetComponent = "".
                /* Set Components */
                IF AVAILABLE est AND (est.est-type EQ 6 OR est.est-type EQ 2) AND est.form-qty GT 1 THEN DO:
                    FOR EACH ef NO-LOCK 
                        WHERE ef.company EQ est.company
                          AND ef.est-no  EQ est.est-no,      
                         EACH bf-eb OF ef NO-LOCK 
                         BREAK BY ef.form-no:
                        ASSIGN
                            lFirstComponent = FIRST(ef.form-no)
                            lcSetComponent  = lcSetComponentData
                            .
                            
                        FIND FIRST style NO-LOCK
                             WHERE style.company EQ bf-eb.company
                               AND style.style   EQ bf-eb.style 
                             NO-ERROR.
        
                        ASSIGN
                            dLengthMetric = bf-eb.len * 25.4
                            dWidthMetric  = bf-eb.wid * 25.4
                            dDepthMetric  = bf-eb.dep * 25.4
                            .
        
                        {sys/inc/roundup.i dLengthMetric}
                        {sys/inc/roundup.i dWidthMetric}
                        {sys/inc/roundup.i dDepthMetric}
                        
                        cAdders = "".
                        
                        DO iIndex = 1 TO 6:
                            IF ef.adder[iIndex] NE "" THEN
                                cAdders = cAdders + ef.adder[iIndex] + ",".
                        END.
                        
                        cAdders = TRIM(cAdders, ",").
                        
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "FirstComponent", STRING(lFirstComponent)).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "Adders", cAdders).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "LengthMetric", STRING(dLengthMetric)).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "WidthMetric", STRING(dWidthMetric)).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "DepthMetric", STRING(dDepthMetric)).
                        
                        lcSetComponent = oAttribute:ReplaceAttributes(lcSetComponent, BUFFER style:HANDLE).
                        lcSetComponent = oAttribute:ReplaceAttributes(lcSetComponent, BUFFER bf-eb:HANDLE).
                        lcSetComponent = oAttribute:ReplaceAttributes(lcSetComponent, BUFFER ef:HANDLE).
                        lcSetComponent = oAttribute:ReplaceAttributes(lcSetComponent, BUFFER est:HANDLE).
                        
                        lcConcatSetComponent = lcConcatSetComponent + lcSetComponent.
                    END.
                END.
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "LengthMetric", STRING(dLengthMetric)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "WidthMetric", STRING(dWidthMetric)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "DepthMetric", STRING(dDepthMetric)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "PrintSetComponents", STRING(lPrintSetComponents)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "Print2ndItemDescription", STRING(lPrint2ndItemDescription)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "FirstQuoteItem", STRING(lFirstItem)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "ItemQuantityCount", STRING(iItemQuantityCount)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteItem, "ItemID", cItemID).
                                        
                lcQuoteItem = REPLACE(lcQuoteItem, "$QuoteQuantities$", lcConcatQuoteQuantity).
                lcQuoteItem = REPLACE(lcQuoteItem, "$SetComponents$", lcConcatSetComponent).
                
                lcQuoteItem = oAttribute:ReplaceAttributes(lcQuoteItem, BUFFER style:HANDLE).
                lcQuoteItem = oAttribute:ReplaceAttributes(lcQuoteItem, BUFFER eb:HANDLE).
                lcQuoteItem = oAttribute:ReplaceAttributes(lcQuoteItem, BUFFER bf-quoteitm:HANDLE).
                
                lcConcatQuoteItem = lcConcatQuoteItem + lcQuoteItem. 
            END.


            oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteHeader, "FirstQuoteHeader", STRING(lFirstQuote)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcQuoteHeader, "LastCustomer", STRING(lLastCustomer)).
            
            lcQuoteHeader = REPLACE(lcQuoteHeader, "$QuoteItems$", lcConcatQuoteItem).
            
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER bf-quotehd:HANDLE).
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER sman:HANDLE).
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER cust:HANDLE).
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER terms:HANDLE).
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER carrier:HANDLE).
            lcQuoteHeader = oAttribute:ReplaceAttributes(lcQuoteHeader, BUFFER est:HANDLE).

            RUN pGetRequestData ("PageHeader", OUTPUT lcPageHeader).

            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER bf-quotehd:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER sman:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER cust:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER terms:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER carrier:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER est:HANDLE).
            
            RUN pInsertPageHeaderFooter (INPUT iNumLinesInPage, INPUT-OUTPUT lcQuoteHeader, INPUT lcPageHeader, INPUT lcPageFooter).
            
            lcConcatQuoteHeader = lcConcatQuoteHeader + lcQuoteHeader. 
            
            /* Box Design */
            IF lPrintBoxDesign THEN DO:
                FOR EACH ef NO-LOCK
                    WHERE ef.company EQ ttQuoteHeader.company
                      AND ef.est-no  EQ ttQuoteHeader.estimateID:
                    FOR EACH eb NO-LOCK
                        WHERE eb.company EQ ef.company
                          AND eb.est-no  EQ ef.est-no
                          AND eb.form-no EQ ef.form-no:
                        FIND FIRST box-design-hdr NO-LOCK
                             WHERE box-design-hdr.design-no EQ 0
                               AND box-design-hdr.company   EQ eb.company 
                               AND box-design-hdr.est-no    EQ eb.est-no
                               AND box-design-hdr.form-no   EQ eb.form-no
                               AND box-design-hdr.blank-no  EQ eb.blank-no
                             NO-ERROR.
                
                        FIND FIRST item NO-LOCK
                             WHERE item.company EQ ef.company
                               AND item.i-no    EQ ef.board 
                             NO-ERROR.
                                        
                        FIND FIRST itemfg NO-LOCK
                             WHERE itemfg.company EQ eb.company 
                               AND itemfg.i-no    EQ eb.stock-no
                             NO-ERROR.
                                        
                        FIND FIRST style NO-LOCK 
                             WHERE style.company EQ eb.company
                               AND style.style   EQ eb.style
                             NO-ERROR.
                        IF (NOT AVAILABLE box-design-hdr) AND AVAILABLE style THEN
                            FIND FIRST box-design-hdr NO-LOCK
                                 WHERE box-design-hdr.design-no EQ style.design-no
                                 NO-ERROR. 
                        
                        cBoxDesignImage = box-design-hdr.box-image.
                        RUN FileSys_ValidateFile(cBoxDesignImage, OUTPUT lValid, OUTPUT cMessage).
                        IF lValid THEN DO:
                            lcBoxDesignHeader = lcBoxDesignHeaderData.
                            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBoxDesignHeader, "BoxDesignImage", cBoxDesignImage).
                            
                            lcConcatBoxDesignLine = "".
                            
                            IF AVAILABLE box-design-hdr THEN DO:
                                FOR EACH box-design-line OF box-design-hdr NO-LOCK:
                                    lcBoxDesignLine = lcBoxDesignLineData.
                                    ASSIGN
                                        cWidthCumulativeScoreMetric = ""
                                        cWidthScoreMetric           = ""
                                        .
                                        
                                    IF box-design-line.wscore NE "" THEN
                                        cWidthScoreMetric = STRING(ROUND(DECIMAL(box-design-line.wscore), 0) * 25.4, ">>>9.99<<").
                
                                    IF box-design-line.wcum-score NE "" THEN
                                        cWidthCumulativeScoreMetric = STRING(ROUND(DECIMAL(box-design-line.wcum-score), 0) * 25.4, ">>>9.99<<").
                                    
                                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBoxDesignLine, "Metric", IF AVAILABLE est THEN STRING(est.metric) ELSE "").
                                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBoxDesignLine, "WidthScoreMetric", cWidthScoreMetric).
                                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBoxDesignLine, "WidthCumulativeScoreMetric", cWidthCumulativeScoreMetric).
                                                                                                                            
                                    lcBoxDesignLine = oAttribute:ReplaceAttributes(lcBoxDesignLine, BUFFER box-design-line:HANDLE).
                                    
                                    lcConcatBoxDesignLine = lcConcatBoxDesignLine + lcBoxDesignLine.
                                END.                    
                            END.
                            
                            lcBoxDesignHeader = REPLACE(lcBoxDesignHeader, "$BoxDesignLines$", lcConcatBoxDesignLine).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER style:HANDLE).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER ef:HANDLE).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER eb:HANDLE).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER item:HANDLE).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER itemfg:HANDLE).
                            lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER box-design-hdr:HANDLE).
                            
                            lcConcatBoxDesignHeader = lcConcatBoxDesignHeader + lcBoxDesignHeader.
                        END.   
                    END.
                END.
            END.        
        END.
    END.

    RUN pGetRequestData(INPUT "QuoteHeaderGroupHeader", OUTPUT lcQuoteHeaderGroupHeader).
    RUN pGetRequestData(INPUT "QuoteHeaderGroupFooter", OUTPUT lcQuoteHeaderGroupFooter).
    RUN pGetRequestData(INPUT "ReportHeader", OUTPUT lcReportHeader).
    RUN pGetRequestData(INPUT "ReportFooter", OUTPUT lcReportFooter).

    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintBoxDesign", STRING(lPrintBoxDesign)).

    ioplcRequestData = REPLACE(ioplcRequestData, "$QuoteHeaders$", lcConcatQuoteHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$QuoteHeaderGroupHeader$", lcQuoteHeaderGroupHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$QuoteHeaderGroupFooter$", lcQuoteHeaderGroupFooter).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportHeader$", lcReportHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportFooter$", lcReportFooter).
    ioplcRequestData = REPLACE(ioplcRequestData, "$BoxDesignHeaders$", lcConcatBoxDesignHeader).
    
    RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
    
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BusinessFormLogo", cBuisnessFormLogo).
    
    ASSIGN   
        opcMessage       = ""
        oplSuccess       = TRUE
        .
END.        

FINALLY:
    IF VALID-HANDLE (hdQuoteProcs) THEN
        DELETE PROCEDURE hdQuoteProcs.
END FINALLY.        


/* **********************  Internal Procedures  *********************** */


PROCEDURE pGetExtendedPrice PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-quoteqty FOR quoteqty.
    DEFINE PARAMETER BUFFER ipbf-eb       FOR eb.
    DEFINE INPUT  PARAMETER ipcEstimateID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEstimateType  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdExtendedPrice AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE iCaseCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlank     AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    IF ipbf-quoteqty.uom EQ "L" THEN
        opdExtendedPrice = ipbf-quoteqty.price.
    ELSE IF ipbf-quoteqty.uom EQ "CS" THEN DO:
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipbf-quoteqty.company
               AND bf-itemfg.i-no    EQ ipcItemID
             NO-ERROR.

        IF ipiEstimateType EQ 6 THEN
            FIND FIRST bf-eb NO-LOCK
                 WHERE bf-eb.company EQ ipbf-quoteqty.company 
                   AND bf-eb.est-no  EQ ipcEstimateID 
                   AND bf-eb.form-no EQ 0
                 NO-ERROR.

        IF (AVAILABLE bf-eb AND bf-eb.cas-no NE "") OR AVAILABLE ipbf-eb THEN
            RUN est/getcscnt.p (
                INPUT  (IF AVAILABLE bf-eb AND bf-eb.cas-no NE "" THEN ROWID(bf-eb) ELSE ROWID(ipbf-eb)),
                OUTPUT iCaseCount,
                OUTPUT iBlank
                ).
        ELSE
            iCaseCount = 0.

        opdExtendedPrice = ipbf-quoteqty.qty / (IF iCaseCount NE 0 THEN iCaseCount ELSE IF AVAILABLE bf-itemfg AND bf-itemfg.case-count NE 0 THEN bf-itemfg.case-count ELSE 1) * ipbf-quoteqty.price.
    END.
    ELSE IF ipbf-quoteqty.uom EQ "C" THEN
        opdExtendedPrice = ((ipbf-quoteqty.qty / 100) * ipbf-quoteqty.price).
    ELSE IF ipbf-quoteqty.uom EQ "M" THEN
        opdExtendedPrice = ((ipbf-quoteqty.qty / 1000) * ipbf-quoteqty.price).
    ELSE
        opdExtendedPrice = (ipbf-quoteqty.qty * ipbf-quoteqty.price).

END PROCEDURE.

PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.
        
    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ "SendQuote"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Header"
           AND bf-APIOutboundDetail.parentID      EQ "SendQuote"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcHeader = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Header" + "$", lcHeader).
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Footer"
           AND bf-APIOutboundDetail.parentID      EQ "SendQuote"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcFooter = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Footer" + "$", lcFooter).
    END.    
END PROCEDURE.
