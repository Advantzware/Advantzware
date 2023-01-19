/*------------------------------------------------------------------------
    File        : api/PrintPurchaseOrder.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Jun 13 12:46:35 EDT 2022
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

DEFINE VARIABLE hdPOProcs                  AS HANDLE   NO-UNDO.
DEFINE VARIABLE hdTTHandle                 AS HANDLE   NO-UNDO.

DEFINE VARIABLE lcReportHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcReportFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageHeader   AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPageFooter   AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcPurchaseOrderGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrder            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderHeader      AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderFooter      AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcPurchaseOrderLineGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLine            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineHeader      AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineFooter      AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcPurchaseOrderLineAdderGroupHeader AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineAdderGroupFooter AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineAdder            AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineAdderHeader      AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineAdderFooter      AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcPurchaseOrderData          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineData      AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcPurchaseOrderLineAdderData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE lcConcatPurchaseOrder          AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatPurchaseOrderLine      AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcConcatPurchaseOrderLineAdder AS LONGCHAR NO-UNDO.

DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iNumLinesInPage AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNotesLength    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValid          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNotes          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLineItemRecKey AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReportTermID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintMetric    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cContentValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintCustomerCode  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintFirstResource AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintGrandTotalMSF AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintScoreType     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintPrintPrices   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMachCode           AS CHARACTER NO-UNDO .

/* Calculate Fields */
DEFINE VARIABLE dPOLineQuantity            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPOLinePurchaseQuantityUOM AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPOLinePurchasedUOM        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dPOLineCost                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPOLineStatusDescription   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFGItemPartDescription3    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dPOLineWidth               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPOLineLength              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPOLineDepth               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cScoreSize16ths1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScoreSize16ths2           AS CHARACTER NO-UNDO.
DEFINE VARIABLE dScoreSize                 AS DECIMAL   NO-UNDO EXTENT 20.
DEFINE VARIABLE cScoreType                 AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE cPOLineNotes               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPONotes                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPOLineSpecNotes           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAdderAvailable            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dTotalSquareFeet           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQuantityInMSF             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPOLineTotal               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPOSubTotal                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPOLineFlute               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPOLineTest                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirstPurchaseOrder        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lLastPurchaseOrder         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cDisplayCompanyAddress     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBusinessFormLogo          AS CHARACTER NO-UNDO.

RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN pGetNumLinesInPage(ipiAPIOutboundID, OUTPUT iNumLinesInPage).

RUN pGetContentValue(ipiAPIOutboundID, "NotesLength", OUTPUT cContentValue).
iNotesLength = INTEGER(cContentValue) NO-ERROR.
    
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
         WHERE ttArgs.argType  EQ "ROWID"
           AND ttArgs.argKey   EQ "ReportTermID"
         NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            opcMessage = "No valid report id passed"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    cReportTermID = ttArgs.argValue.
    
    FOR EACH report NO-LOCK
        WHERE report.term-id EQ cReportTermID,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY po-ord.vend-no BY po-ord.po-no BY report.term-id:
        
        ASSIGN
            lFirstPurchaseOrder = FIRST(report.term-id)
            lLastPurchaseOrder  = LAST(report.term-id)
            .  
        
        IF lFirstPurchaseOrder THEN
            system.SharedConfig:Instance:SetValue("APIOutboundEvent_UserField1", STRING(po-ord.po-no)).
        
        IF lLastPurchaseOrder THEN
            system.SharedConfig:Instance:SetValue("APIOutboundEvent_UserField2", STRING(po-ord.po-no)).
                                    
        RUN po/POProcs.p PERSISTENT SET hdPOProcs.
    
        lPrintMetric = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintMetric")) NO-ERROR.
        IF lPrintMetric EQ ? THEN
            lPrintMetric = YES.
        lPrintCustomerCode = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintCustomerCode")) NO-ERROR.
        IF lPrintCustomerCode EQ ? THEN
            lPrintCustomerCode = YES.
        lPrintFirstResource = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintFirstResource")) NO-ERROR.
        IF lPrintFirstResource EQ ? THEN
            lPrintFirstResource = YES.
        lPrintGrandTotalMSF = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintGrandTotalMSF")) NO-ERROR.
        IF lPrintGrandTotalMSF EQ ? THEN
            lPrintGrandTotalMSF = YES.
        lPrintScoreType = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintScoreTypes")) NO-ERROR.
        IF lPrintScoreType EQ ? THEN
            lPrintScoreType = YES.
        lPrintPrintPrices = LOGICAL(system.SharedConfig:Instance:GetValue("PrintPurchaseOrder_PrintPrintPrices")) NO-ERROR.
        IF lPrintPrintPrices EQ ? THEN
            lPrintPrintPrices = YES.
        
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrder", OUTPUT lcPurchaseOrderData).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderHeader", OUTPUT lcPurchaseOrderHeader).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderGroupHeader", OUTPUT lcPurchaseOrderGroupHeader).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderGroupFooter", OUTPUT lcPurchaseOrderGroupFooter).
        
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLine", OUTPUT lcPurchaseOrderLineData).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLineGroupHeader", OUTPUT lcPurchaseOrderLineGroupHeader).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLineGroupFooter", OUTPUT lcPurchaseOrderLineGroupFooter).
        
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLineAdder", OUTPUT lcPurchaseOrderLineAdderData).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLineAdderGroupHeader", OUTPUT lcPurchaseOrderLineAdderGroupHeader).
        RUN pGetRequestData ("PrintPurchaseOrder", "PurchaseOrderLineAdderGroupFooter", OUTPUT lcPurchaseOrderLineAdderGroupFooter).
        
        RUN pGetRequestData ("PrintPurchaseOrder", "PageFooter", OUTPUT lcPageFooter).
        RUN pGetRequestData ("PrintPurchaseOrder", "PageHeader", OUTPUT lcPageHeader).
    
        oAttribute = NEW system.Attribute().
        oAttribute:RequestDataType = gcRequestDataType.
        
        ASSIGN
            lcPurchaseOrder           = lcPurchaseOrderData
            lcConcatPurchaseOrderLine = ""
            dTotalSquareFeet          = 0
            cPONotes                  = ""
            .
        
        FOR EACH po-ordl NO-LOCK    
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:
            
            ASSIGN
                lcConcatPurchaseOrderLineAdder = ""
                lcPurchaseOrderLine            = lcPurchaseOrderLineData
                cScoreSize16ths1               = ""
                cScoreSize16ths2               = ""
                dScoreSize                     = 0
                cScoreType                     = ""
                cPOLineNotes                   = ""
                cPOLineSpecNotes               = ""
                lAdderAvailable                = FALSE
                .
                 
            FOR EACH po-ordl-add NO-LOCK    
                WHERE po-ordl-add.company EQ po-ord.company
                  AND po-ordl-add.po-no   EQ po-ordl.po-no
                  AND po-ordl-add.line    EQ po-ordl.line:
                ASSIGN
                    lcPurchaseOrderLineAdder = lcPurchaseOrderLineAdderData
                    lAdderAvailable          = TRUE
                    .
    
                FIND FIRST item NO-LOCK
                     WHERE item.company EQ po-ordl-add.company
                       AND item.i-no    EQ po-ordl-add.adder-i-no
                     NO-ERROR.
                lcPurchaseOrderLineAdder = oAttribute:ReplaceAttributes(lcPurchaseOrderLineAdder, BUFFER item:HANDLE).
                
                lcPurchaseOrderLineAdder = oAttribute:ReplaceAttributes(lcPurchaseOrderLineAdder, BUFFER po-ordl-add:HANDLE).
                    
                lcConcatPurchaseOrderLineAdder = lcConcatPurchaseOrderLineAdder + lcPurchaseOrderLineAdder.
            END.
            
            ASSIGN
                dPOLineQuantity            = po-ordl.ord-qty
                dPOLineCost                = po-ordl.cost
                cPOLinePurchasedUOM        = po-ordl.pr-uom
                cPOLinePurchaseQuantityUOM = po-ordl.pr-qty-uom
                cFGItemPartDescription3    = ""
                dPOLineWidth               = po-ordl.s-wid
                dPOLineLength              = po-ordl.s-len
                dPOLineDepth               = po-ordl.s-dep
                cLineItemRecKey            = ""
                .
            
            IF po-ordl.item-type THEN DO:
                FIND FIRST item NO-LOCK
                     WHERE item.company EQ po-ordl.company
                       AND item.i-no    EQ po-ordl.i-no
                     NO-ERROR.
                lcPurchaseOrderLine = oAttribute:ReplaceAttributes(lcPurchaseOrderLine, BUFFER item:HANDLE).
                
                IF AVAILABLE item THEN DO:
                    cLineItemRecKey = item.rec_key.
                    
                    IF dPOLineDepth EQ 0 THEN
                        dPOLineDepth = IF item.mat-type = "C" THEN 
                                           item.case-d
                                       ELSE
                                           item.s-dep.
                    
                    IF item.mat-type EQ "B" AND item.industry EQ "2" THEN
                        ASSIGN
                            cPOLineFlute = item.flute
                            cPOLineTest  = item.reg-no
                            .
                END.    
            END.
            ELSE DO:
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ po-ordl.company
                       AND itemfg.i-no    EQ po-ordl.i-no
                     NO-ERROR.
                lcPurchaseOrderLine = oAttribute:ReplaceAttributes(lcPurchaseOrderLine, BUFFER itemfg:HANDLE).
                
                IF po-ordl.spare-int-1 EQ 1 AND AVAILABLE itemfg THEN
                    ASSIGN 
                        dPOLineQuantity            = dPOLineQuantity / itemfg.case-count
                        cPOLinePurchaseQuantityUOM = "CS"
                        .            
    
                IF po-ordl.spare-int-2 EQ 1 AND AVAILABLE itemfg THEN
                    ASSIGN
                        dPOLineCost         = dPOLineCost * itemfg.case-count
                        cPOLinePurchasedUOM = "CS"
                        .                    
    
                IF po-ordl.pr-qty-uom EQ "LF" THEN
                    {sys/inc/roundup.i dPOLineQuantity}
                
                IF AVAILABLE itemfg THEN
                    ASSIGN
                        cFGItemPartDescription3 = itemfg.part-dscr3
                        cLineItemRecKey         = itemfg.rec_key
                        .                    
            END.
    
            IF po-ordl.stat EQ "A" THEN 
                cPOLineStatusDescription = "Added".
            ELSE IF po-ordl.stat EQ "U" THEN 
                cPOLineStatusDescription = "Updated".
            ELSE IF po-ordl.stat EQ "O" THEN 
                cPOLineStatusDescription = "Open".
            ELSE IF po-ordl.stat EQ "P" THEN 
                cPOLineStatusDescription = "Partial".
            ELSE IF po-ordl.stat EQ "C" THEN 
                cPOLineStatusDescription = "Closed".
    
            IF po-ordl.deleted EQ YES THEN 
                cPOLineStatusDescription = "Deleted".
        
            RUN PO_GetLineScoresAndTypes IN hdPOProcs (
                INPUT  po-ordl.company,
                INPUT  po-ordl.po-no,
                INPUT  po-ordl.line,
                INPUT  "",
                OUTPUT dScoreSize,
                OUTPUT cScoreType
                ).
            
            DO iIndex = 1 TO 20:
                IF iIndex LE 10 THEN DO:
                    IF dScoreSize[iIndex] NE 0 THEN
                        cScoreSize16ths1 = cScoreSize16ths1 + STRING(dScoreSize[iIndex], ">>9.99") + STRING(cScoreType[iIndex],"X") + " ".
                    ELSE
                        cScoreSize16ths1 = cScoreSize16ths1 + "        ".  
                END.
                ELSE DO:
                    IF dScoreSize[iIndex] NE 0 THEN
                        cScoreSize16ths2 = cScoreSize16ths2 + STRING(dScoreSize[iIndex], ">>9.99") + STRING(cScoreType[iIndex],"X") + " ".
                    ELSE
                        cScoreSize16ths2 = cScoreSize16ths2 + "        ".  
                END.
                
            END.
    
            FOR EACH notes NO-LOCK
                WHERE notes.rec_key EQ po-ordl.rec_key:
                cNotes = TRIM(notes.note_text).
                
                RUN pReplaceNotesText(INPUT-OUTPUT cNotes, iNotesLength).
                cPOLineNotes = cPOLineNotes + cNotes + CHR(10).
            END.                        
            cPOLineNotes = TRIM(cPOLineNotes, CHR(10)).
            
            FOR EACH notes NO-LOCK
                WHERE notes.rec_key EQ cLineItemRecKey:
                cNotes = TRIM(notes.note_text).
                
                RUN pReplaceNotesText(INPUT-OUTPUT cNotes, iNotesLength).
                cPOLineSpecNotes = cPOLineSpecNotes + cNotes + CHR(10).
            END.                        
            cPOLineSpecNotes = TRIM(cPOLineSpecNotes, CHR(10)).
            
            dQuantityInMSF = po-ordl.ord-qty.
            IF po-ordl.pr-qty-uom NE "MSF" THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  po-ordl.company,
                    INPUT  po-ordl.i-no,
                    INPUT  IF po-ordl.item-type THEN "RM" ELSE "FG",
                    INPUT  po-ordl.ord-qty,
                    INPUT  po-ordl.pr-qty-uom, 
                    INPUT  "MSF",
                    INPUT  0,
                    INPUT  po-ordl.s-len,
                    INPUT  po-ordl.s-wid,
                    INPUT  IF po-ordl.item-type AND AVAILABLE item THEN item.s-dep ELSE 0,
                    INPUT  0,
                    OUTPUT dQuantityInMSF,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
            
            dTotalSquareFeet = dTotalSquareFeet + (dQuantityInMSF * 1000).
            dPOLineTotal = po-ordl.t-cost - po-ordl.setup.
            
            
            IF lPrintMetric THEN
                ASSIGN
                    dPOLineLength = ROUND(dPOLineLength * 25.4, 0)
                    dPOLineWidth  = ROUND(dPOLineWidth * 25.4, 0)
                    dPOLineDepth  = ROUND(dPOLineDepth * 25.4, 0)
                    .
                    
            IF lPrintFirstResource THEN DO:
                cMachCode = "" .
                FOR EACH job-mch WHERE job-mch.company EQ po-ordl.company
                    AND job-mch.job-no EQ po-ordl.job-no
                    AND job-mch.job-no2 EQ po-ordl.job-no2
                    AND job-mch.frm EQ po-ordl.s-num use-index line-idx NO-LOCK:

                    ASSIGN cMachCode = job-mch.m-code .
                    LEAVE.
                END.
            END.
                
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineCost", STRING(dPOLineCost)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineQuantity", STRING(dPOLineQuantity)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLinePurchasedUOM", cPOLinePurchasedUOM).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLinePurchaseQuantityUOM", cPOLinePurchaseQuantityUOM).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineStatusDescription", cPOLineStatusDescription).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "FGItemPartDescription3", cFGItemPartDescription3).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineLength", STRING(dPOLineLength)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineWidth", STRING(dPOLineWidth)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineDepth", STRING(dPOLineDepth)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "ScoreSize16ths1", cScoreSize16ths1).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "ScoreSize16ths2", cScoreSize16ths2).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineNotes", cPOLineNotes).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineSpecNotes", cPOLineSpecNotes).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineFlute", cPOLineFlute).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineTest", cPOLineTest).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "AdderAvailable", STRING(lAdderAvailable)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineQuantityInMSF", STRING(dQuantityInMSF)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "POLineTotal", STRING(dPOLineTotal)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrderLine, "MachCode", cMachCode).
            
            lcPurchaseOrderLine = oAttribute:ReplaceAttributes(lcPurchaseOrderLine, BUFFER po-ordl:HANDLE).
    
            lcPurchaseOrderLine = REPLACE(lcPurchaseOrderLine, "$PurchaseOrderLineAdders$", lcConcatPurchaseOrderLineAdder).
            lcPurchaseOrderLine = REPLACE(lcPurchaseOrderLine, "$PurchaseOrderLineAdderGroupHeader$", lcPurchaseOrderLineAdderGroupHeader).
            lcPurchaseOrderLine = REPLACE(lcPurchaseOrderLine, "$PurchaseOrderLineAdderGroupFooter$", lcPurchaseOrderLineAdderGroupFooter).
            
            lcConcatPurchaseOrderLine = lcConcatPurchaseOrderLine + lcPurchaseOrderLine.        
        END.
        
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ po-ord.company
               AND vend.vend-no EQ po-ord.vend-no
             NO-ERROR.
        lcPurchaseOrder = oAttribute:ReplaceAttributes(lcPurchaseOrder, BUFFER vend:HANDLE).        
        lcPurchaseOrderHeader = oAttribute:ReplaceAttributes(lcPurchaseOrderHeader, BUFFER vend:HANDLE).        
    
        FIND FIRST terms NO-LOCK
             WHERE terms.company EQ po-ord.company 
               AND terms.t-code  EQ po-ord.terms 
             NO-ERROR.
        lcPurchaseOrder = oAttribute:ReplaceAttributes(lcPurchaseOrder, BUFFER terms:HANDLE).
        lcPurchaseOrderHeader = oAttribute:ReplaceAttributes(lcPurchaseOrderHeader, BUFFER terms:HANDLE).
    
        FIND FIRST carrier NO-LOCK
             WHERE carrier.company EQ po-ord.company 
               AND carrier.carrier EQ po-ord.carrier 
             NO-ERROR.
        lcPurchaseOrder = oAttribute:ReplaceAttributes(lcPurchaseOrder, BUFFER carrier:HANDLE).
        lcPurchaseOrderHeader = oAttribute:ReplaceAttributes(lcPurchaseOrderHeader, BUFFER carrier:HANDLE).
        
        lcPurchaseOrder = oAttribute:ReplaceAttributes(lcPurchaseOrder, BUFFER po-ord:HANDLE).
        lcPurchaseOrderHeader = oAttribute:ReplaceAttributes(lcPurchaseOrderHeader, BUFFER po-ord:HANDLE).
        
        lcPurchaseOrder = REPLACE(lcPurchaseOrder, "$PurchaseOrderLines$", lcConcatPurchaseOrderLine).
        lcPurchaseOrder = REPLACE(lcPurchaseOrder, "$PurchaseOrderLineGroupHeader$", lcPurchaseOrderLineGroupHeader).
        lcPurchaseOrder = REPLACE(lcPurchaseOrder, "$PurchaseOrderLineGroupFooter$", lcPurchaseOrderLineGroupFooter).
    
        RUN pInsertPageHeaderFooter (INPUT iNumLinesInPage, INPUT-OUTPUT lcPurchaseOrder, INPUT lcPageHeader + lcPurchaseOrderHeader, INPUT lcPageFooter).
                
        ASSIGN
            dTotalSquareFeet = dTotalSquareFeet / 1000
            dPOSubTotal      = po-ord.t-cost - po-ord.tax
            .

        FOR EACH notes NO-LOCK
            WHERE notes.rec_key EQ po-ord.rec_key:
            cNotes = TRIM(notes.note_text).
            
            RUN pReplaceNotesText(INPUT-OUTPUT cNotes, iNotesLength).
            cPONotes = cPONotes + cNotes + CHR(10).
        END.    
        cPONotes = TRIM(cPONotes, CHR(10)).
        
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrder, "TotalSquareFeet", STRING(dTotalSquareFeet)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrder, "POSubTotal", STRING(dPOSubTotal)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrder, "FirstPurchaseOrder", STRING(lFirstPurchaseOrder)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrder, "LastPurchaseOrder", STRING(lLastPurchaseOrder)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPurchaseOrder, "PONotes", cPONotes).
        
        lcConcatPurchaseOrder = lcConcatPurchaseOrder + lcPurchaseOrder.
    END.

    RUN pGetRequestData("PrintPurchaseOrder", "PurchaseOrderGroupHeader", OUTPUT lcPurchaseOrderGroupHeader).
    RUN pGetRequestData("PrintPurchaseOrder", "PurchaseOrderGroupFooter", OUTPUT lcPurchaseOrderGroupFooter).
    RUN pGetRequestData("PrintPurchaseOrder", "ReportHeader", OUTPUT lcReportHeader).
    RUN pGetRequestData("PrintPurchaseOrder", "ReportFooter", OUTPUT lcReportFooter).

    ioplcRequestData = REPLACE(ioplcRequestData, "$PurchaseOrders$", lcConcatPurchaseOrder).
    ioplcRequestData = REPLACE(ioplcRequestData, "$PurchaseOrderGroupHeader$", lcPurchaseOrderGroupHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$PurchaseOrderGroupFooter$", lcPurchaseOrderGroupFooter).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportHeader$", lcReportHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportFooter$", lcReportFooter).
    
    RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").

    RUN sys/ref/nk1look.p (cCompany, "POPRINT", "L" , YES, YES, "" , "", OUTPUT cDisplayCompanyAddress, OUTPUT lRecFound).
    RUN FileSys_GetBusinessFormLogo(cCompany, "" /* cust */ , "" /* Location */ , OUTPUT cBusinessFormLogo, OUTPUT lValid, OUTPUT cMessage).
    
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BusinessFormLogo", cBusinessFormLogo).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DisplayCompanyAddress", cDisplayCompanyAddress).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintMetric", STRING(lPrintMetric)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintGrandTotalMSF", STRING(lPrintGrandTotalMSF)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintCustomerCode", STRING(lPrintCustomerCode)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintScoreType", STRING(lPrintScoreType)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "PrintPrintPrices", STRING(lPrintPrintPrices)).
    
    ASSIGN   
        opcMessage       = ""
        oplSuccess       = TRUE
        .
END.        

FINALLY:
    IF VALID-HANDLE (hdPOProcs) THEN
        DELETE PROCEDURE hdPOProcs.
END FINALLY.        


/* **********************  Internal Procedures  *********************** */


PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcParentID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.

    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Header"
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcHeader = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Header" + "$", lcHeader).
    END.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Footer"
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcFooter = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Footer" + "$", lcFooter).
    END. 
END PROCEDURE.

PROCEDURE pReplaceNotesText:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcNoteText      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER        ipiTextLineLength AS INTEGER   NO-UNDO.
    
    IF ipiTextLineLength EQ ? OR ipiTextLineLength EQ 0 THEN
        ipiTextLineLength = 80.
        
    DEFINE VARIABLE cTempNote       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIterations     AS INTEGER   NO-UNDO.

    ASSIGN
        iopcNoteText = REPLACE(iopcNoteText, CHR(10) + CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(10), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, "  ", " ")
        .
    
    IF LENGTH(iopcNoteText) GT ipiTextLineLength THEN DO:
        iIterations = (LENGTH(iopcNoteText) / ipiTextLineLength) + INT(LENGTH(iopcNoteText) MOD ipiTextLineLength GT 0).
        DO iIndex = 1 TO iIterations:
            cTempNote = cTempNote + SUBSTRING(iopcNoteText, iIndex * ipiTextLineLength - (ipiTextLineLength - 1), ipiTextLineLength) + CHR(10). 
        END.
    END.
    ELSE
        cTempNote = iopcNoteText.
    
    cTempNote = TRIM(cTempNote, CHR(10)).
    
    iopcNoteText = cTempNote.
END PROCEDURE.