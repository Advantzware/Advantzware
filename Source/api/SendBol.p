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
{oe/ttBolHeader.i}
{oe/ttBolItem.i}
{oerep/r-bolx.i NEW}

DEFINE TEMP-TABLE tt-bol 
    FIELD row-id  AS ROWID      
    FIELD cust-no AS CHARACTER
    INDEX row-id row-id    
    .
    
DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdBolProcs              AS HANDLE           NO-UNDO.
DEFINE VARIABLE mptrTTBol               AS MEMPTR           NO-UNDO.
DEFINE VARIABLE hdTTHandle              AS HANDLE           NO-UNDO.
DEFINE VARIABLE lcReportHeader          AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcReportFooter          AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcPageHeader            AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcPageFooter            AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolHeaderGroupHeader  AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolHeaderGroupFooter  AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolHeader             AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItemGroupHeader    AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItemGroupFooter    AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItem               AS LONGCHAR         NO-UNDO.  
DEFINE VARIABLE lcBolItemDetail         AS LONGCHAR         NO-UNDO. 
DEFINE VARIABLE lcBolItemSummary        AS LONGCHAR         NO-UNDO.

DEFINE VARIABLE lcBolHeaderData         AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItemData           AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItemDetailData     AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBolItemSummaryData    AS LONGCHAR         NO-UNDO. 

DEFINE VARIABLE lcConcatBolHeader       AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatBolItem         AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatQuoteQuantity   AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatQuoteMisc       AS LONGCHAR         NO-UNDO.

DEFINE VARIABLE lcBoxDesignHeader       AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcBoxDesignHeaderData   AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatBoxDesignHeader AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcSpecInstrctn          AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcSpecInstrctnData      AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatSpecInstrctn    AS LONGCHAR         NO-UNDO.

DEFINE VARIABLE lcPrintBarCode          AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcPrintBarCodeData      AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatPrintBarCode    AS LONGCHAR         NO-UNDO.

DEFINE VARIABLE lcSetComponent          AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcSetComponentData      AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE lcConcatSetComponent    AS LONGCHAR         NO-UNDO.

DEFINE VARIABLE lAvailBolHeader         AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lAvailBolItem           AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lAvailQuoteQuantity     AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lAvailQuoteMisc         AS LOGICAL          NO-UNDO.

DEFINE VARIABLE oAttribute              AS system.Attribute NO-UNDO.
DEFINE VARIABLE cCompany                AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lRecFound               AS LOGICAL          NO-UNDO.
DEFINE VARIABLE iNumLinesInPage         AS INTEGER          NO-UNDO.
DEFINE VARIABLE lValid                  AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER        NO-UNDO.
DEFINE VARIABLE dValue                  AS DECIMAL          NO-UNDO.

DEFINE VARIABLE cBusinessFormLogo       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lFirstBol               AS LOGICAL          NO-UNDO INITIAL ?.  
DEFINE VARIABLE lPrintAssembleComponent AS LOGICAL          NO-UNDO.     
DEFINE VARIABLE lLastCustomer           AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE cLogoColor              AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cDisplayCompanyAddress  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lPrintBarCodeByPartNo   AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE lPrintDeptNotes         AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE lPrintShipNotes         AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE lPrintSpecNotes         AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE lPrintUnAssembleComp    AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE lPrintPalletNumber      AS LOGICAL          NO-UNDO. 
DEFINE VARIABLE cPrintDeptCode          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cNoteText               AS CHARACTER        NO-UNDO.
DEFINE VARIABLE iBarLine                AS INTEGER          NO-UNDO.
DEFINE VARIABLE iBarCount               AS INTEGER          NO-UNDO.     
DEFINE VARIABLE iLineOfTags             AS INTEGER          NO-UNDO.


DEFINE VARIABLE dPartQty                AS DECIMAL          NO-UNDO. 
DEFINE VARIABLE cBolFmtIntegerValue     AS CHARACTER        NO-UNDO.

DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
DEFINE BUFFER bf-oe-boll FOR oe-boll.   
DEFINE BUFFER xitemfg    FOR itemfg.
DEFINE BUFFER bf-cust    FOR cust.

RUN oe/BolProcs.p PERSISTENT SET hdBolProcs.

RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN pGetNumLinesInPage(ipiAPIOutboundID, OUTPUT iNumLinesInPage).

ASSIGN
    lPrintBarCodeByPartNo   = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintBarCodeByPartNo"))
    lPrintDeptNotes         = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintDeptNotes"))
    lPrintAssembleComponent = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintAssembleComponent"))       
    lPrintShipNotes         = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintShipNotes"))
    lPrintSpecNotes         = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintSpecNotes"))
    lPrintUnAssembleComp    = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintUnAssembleComp"))
    lPrintPalletNumber      = LOGICAL(system.SharedConfig:Instance:GetValue("SendBol_PrintPalletNumber"))
    cPrintDeptCode          = system.SharedConfig:Instance:GetValue("SendBol_PrintDeptCode")    
    NO-ERROR.
    
    RUN GetPrintBarTag IN SOURCE-PROCEDURE (OUTPUT TABLE tt-bolx) NO-ERROR.
    
IF ipcRequestHandler NE "" THEN
    RUN VALUE(ipcRequestHandler) (
        INPUT TABLE  ttArgs,
        INPUT ipiAPIOutboundID,
        INPUT ipiAPIOutboundTriggerID,
        INPUT-OUTPUT ioplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
ELSE 
DO:
    FIND FIRST ttArgs
        WHERE ttArgs.argType  = "ROWID"
        AND ttArgs.argKey   = "TTBol"
        NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN 
    DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid temp-table handle"
            .          
        RETURN.
    END.
    
    hdTTHandle = HANDLE(ttArgs.argValue).
    
    IF NOT VALID-HANDLE (hdTTHandle) THEN 
    DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid temp-table handle"
            .         
        RETURN.        
    END.

    oAttribute = NEW system.Attribute().
    oAttribute:RequestDataType = gcRequestDataType.

    RUN FileSys_GetBusinessFormLogo(cCompany, "" /* cust */ , "" /* Location */ , OUTPUT cBusinessFormLogo, OUTPUT lValid, OUTPUT cMessage).
    RUN sys/ref/nk1look.p (cCompany, "LOGOCOLR", "C", NO, YES, "", "", OUTPUT cLogoColor, OUTPUT lRecFound).
    RUN sys/ref/nk1look.p (cCompany, "BOLFMT", "L" , YES, YES, "" , "", OUTPUT cDisplayCompanyAddress, OUTPUT lRecFound).
    RUN sys/ref/nk1look.p (cCompany, "BOLFMT", "I" , YES, YES, "" , "", OUTPUT cBolFmtIntegerValue, OUTPUT lRecFound).
    IF cLogoColor EQ "" THEN "BLACK".    
      
    /* Code to send data from dynamic temp-table handle to static temp-table */
    hdTTHandle:WRITE-XML("MEMPTR", mptrTTBol).
    
    TEMP-TABLE tt-bol:READ-XML("MEMPTR", mptrTTBol, "EMPTY", ?, FALSE).

    RUN pGetRequestData ("BolHeader", OUTPUT lcBolHeaderData).
    RUN pGetRequestData ("BolHeaderGroupHeader", OUTPUT lcBolHeaderGroupHeader).
    RUN pGetRequestData ("BolHeaderGroupFooter", OUTPUT lcBolHeaderGroupFooter).  
    RUN pGetRequestData ("BolItem", OUTPUT lcBolItemData).          
    RUN pGetRequestData ("BolItemDetail", OUTPUT lcBolItemDetailData).     
    RUN pGetRequestData ("BolItemSummary", OUTPUT lcBolItemSummaryData).      
    RUN pGetRequestData ("BolItemNotes", OUTPUT lcSpecInstrctnData).       
    RUN pGetRequestData ("BolBarCode", OUTPUT lcPrintBarCodeData).       
    RUN pGetRequestData ("BolItemGroupHeader", OUTPUT lcBolItemGroupHeader).     
    RUN pGetRequestData ("BolItemGroupFooter", OUTPUT lcBolItemGroupFooter).    
    RUN pGetRequestData ("SetComponent", OUTPUT lcSetComponentData).       
    RUN pGetRequestData ("PageFooter", OUTPUT lcPageFooter).

    ASSIGN
        lAvailBolHeader     = FALSE
        lAvailBolItem       = FALSE
        lAvailQuoteQuantity = FALSE
        lAvailQuoteMisc     = FALSE
        .
                    
    FOR EACH tt-bol            
        BREAK BY tt-bol.cust-no:
        
        lLastCustomer = LAST-OF(tt-bol.cust-no).
                        
        FIND FIRST bf-oe-bolh NO-LOCK
            WHERE ROWID(bf-oe-bolh) EQ tt-bol.row-id 
            NO-ERROR.
        IF NOT AVAILABLE bf-oe-bolh THEN 
        DO:
            ASSIGN
                opcMessage = "Invalid Bol ROWID"
                oplSuccess = FALSE
                .
            RETURN.
        END.        
        
        RUN Bol_BuildBol IN hdBolProcs (
            INPUT  bf-oe-bolh.company,
            INPUT  bf-oe-bolh.b-no,             
            INPUT-OUTPUT TABLE ttBolHeader BY-REFERENCE,
            INPUT-OUTPUT TABLE ttBolItem BY-REFERENCE            
            ).               
               
        FOR EACH ttBolHeader,
            FIRST bf-oe-bolh NO-LOCK
            WHERE ROWID(bf-oe-bolh) EQ ttBolHeader.riBol:      
                    
            ASSIGN
                lcBolHeader     = lcBolHeaderData
                lcConcatBolItem = ""                  
                lFirstBol       = ?
                .                         
                      
            FIND FIRST carrier NO-LOCK
                WHERE carrier.company EQ ttBolHeader.company
                AND carrier.carrier EQ ttBolHeader.carrier
                NO-ERROR.
            FIND FIRST terms NO-LOCK
                WHERE terms.company EQ ttBolHeader.company
                AND terms.t-code  EQ ttBolHeader.terms
                NO-ERROR.
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ttBolHeader.company
                AND cust.cust-no EQ ttBolHeader.customerID
                NO-ERROR.               
            
            FOR EACH ttBolItem
                WHERE ttBolItem.company    EQ ttBolHeader.company
                AND ttBolItem.locationID EQ ttBolHeader.locationID
                AND ttBolItem.bolID      EQ ttBolHeader.bolID,
                FIRST bf-oe-boll NO-LOCK
                WHERE ROWID(bf-oe-boll) EQ ttBolItem.riBolitm :                   
                
                ASSIGN                                     
                    lcBolItem        = lcBolItemData
                    lcBolItemDetail  = lcBolItemDetailData
                    lcBolItemSummary = lcBolItemSummaryData                   
                    .  
                
                lcConcatSetComponent = "".
                /* Set Components */
                IF ttBolItem.lastLineItem THEN
                    FOR EACH fg-set
                        WHERE fg-set.company EQ bf-oe-boll.company
                        AND fg-set.set-no  EQ bf-oe-boll.i-no
                        NO-LOCK,
                      
                        FIRST xitemfg
                        WHERE xitemfg.company EQ bf-oe-boll.company
                        AND xitemfg.i-no    EQ fg-set.part-no
                        NO-LOCK
                      
                        BREAK BY fg-set.set-no:
                      
                        {sys/inc/part-qty.i dPartQty fg-set}
                    
                        ASSIGN                               
                            lcSetComponent = lcSetComponentData
                            .                                
                                                
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "ComponentPartNo", STRING(xitemfg.part-no)).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "ComponentItemNo", fg-set.part-no).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "ComponentItemName", STRING(xitemfg.i-name)).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSetComponent, "ComponentQty", STRING(bf-oe-boll.qty * dPartQty)).                         
                                               
                        lcConcatSetComponent = lcConcatSetComponent + lcSetComponent.
                    
                    END.
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "PrintAssembleComponent", STRING(lPrintAssembleComponent)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "PrintBarCodeByPartNo", STRING(lPrintBarCodeByPartNo)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "PrintDeptNotes", STRING(lPrintDeptNotes)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "BolId", STRING(ttBolItem.bolID)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "firstLineItem", STRING(ttBolItem.firstLineItem)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItem, "lastLineItem", STRING(ttBolItem.lastLineItem)).
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "PrintAssembleComponent", STRING(lPrintAssembleComponent)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "PrintBarCodeByPartNo", STRING(lPrintBarCodeByPartNo)).                                   
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "BolId", STRING(ttBolItem.bolID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "CustomerPart", STRING(ttBolItem.partID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "CustomerPo", STRING(ttBolItem.custPO)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "FGItem", STRING(ttBolItem.itemID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemDescription", STRING(ttBolItem.itemPartDesc)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "BolSummPart", STRING(ttBolItem.bolSummPart)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemName", STRING(ttBolItem.itemName)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemUnit", STRING(ttBolItem.ItemUnit)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemQtyUnit", STRING(ttBolItem.ItemQtyUnit)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemPc", STRING(ttBolItem.ItemPC)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemWeight", STRING(ttBolItem.ItemWeight)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "DeptNotes", STRING(ttBolItem.deptNotes[1])).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "ItemOrder", STRING(ttBolItem.order)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "BolQty", STRING(ttBolItem.bolQty)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "OrdSummaryQty", STRING(ttBolItem.OrdSummaryQty)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "TotalPkgs", STRING(ttBolItem.totalPkgs)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "firstLineItem", STRING(ttBolItem.firstLineItem)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "lastLineItem", STRING(ttBolItem.lastLineItem)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "PartialMulti", STRING(IF bf-oe-boll.partial EQ 0 THEN "" ELSE "1")).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "BolLotNo", STRING(ttBolItem.cLotNo)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemDetail, "Refnum", STRING(ttBolItem.cRefnum)).
                
                lcBolItemDetail = oAttribute:ReplaceAttributes(lcBolItemDetail, BUFFER bf-oe-boll:HANDLE).
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "PrintAssembleComponent", STRING(lPrintAssembleComponent)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "PrintBarCodeByPartNo", STRING(lPrintBarCodeByPartNo)).                                   
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "BolId", STRING(ttBolItem.bolID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "CustomerPart", STRING(ttBolItem.partID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "CustomerPo", STRING(ttBolItem.custPO)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "FGItem", STRING(ttBolItem.itemID)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemDescription", STRING(ttBolItem.itemPartDesc)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "BolSummPart", STRING(ttBolItem.bolSummPart)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemName", STRING(ttBolItem.itemName)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemUnit", STRING(ttBolItem.ItemUnit)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemQtyUnit", STRING(ttBolItem.ItemQtyUnit)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemPc", STRING(ttBolItem.ItemPC)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemWeight", STRING(ttBolItem.ItemWeight)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "DeptNotes", STRING(ttBolItem.deptNotes[1])).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "ItemOrder", STRING(ttBolItem.order)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "BolQty", STRING(ttBolItem.bolQty)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "OrdSummaryQty", STRING(ttBolItem.OrdSummaryQty)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "RefnumRefnumRefnumRefnumRefnumRefnumRefnum", STRING(ttBolItem.Refnum)).                 
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "TotalPkgs", STRING(ttBolItem.totalPkgs)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "firstLineItem", STRING(ttBolItem.firstLineItem)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolItemSummary, "lastLineItem", STRING(ttBolItem.lastLineItem)).
                
                lcBolItemSummary = oAttribute:ReplaceAttributes(lcBolItemSummary, BUFFER bf-oe-boll:HANDLE).
                
                lcConcatSpecInstrctn = "".
                IF lPrintDeptNotes AND ttBolItem.lastLineItem THEN
                    FOR EACH notes NO-LOCK 
                        WHERE notes.rec_key   EQ ttBolItem.cRecKey
                        AND CAN-DO(cPrintDeptCode,notes.note_code)                     
                        BREAK BY notes.note_code:
                    
                        ASSIGN                         
                            cNoteText = notes.note_text
                            . 
                        lcSpecInstrctn = lcSpecInstrctnData.
                    
                        RUN pReplaceNotesText(INPUT-OUTPUT cNoteText).
                    
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSpecInstrctn, "NoteText", cNoteText).
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcSpecInstrctn, "LastNoteText", STRING(LAST(notes.note_code))).
                    
                        lcSpecInstrctn = oAttribute:ReplaceAttributes(lcSpecInstrctn, BUFFER notes:HANDLE).                     
                    
                        lcConcatSpecInstrctn = lcConcatSpecInstrctn + lcSpecInstrctn.
                    END.                                                               
                               
                lcBolItem = REPLACE(lcBolItem, "$SetComponents$", lcConcatSetComponent).
                lcBolItem = REPLACE(lcBolItem, "$BolItemSummary$", lcBolItemSummary).
                lcBolItem = REPLACE(lcBolItem, "$BolItemDetail$", lcBolItemDetail).                  
                lcBolItem = REPLACE(lcBolItem, "$BolItemNotes$", lcConcatSpecInstrctn).
                                                                                                        
                lcBolItem = oAttribute:ReplaceAttributes(lcBolItem, BUFFER bf-oe-boll:HANDLE).
                
                lcConcatBolItem = lcConcatBolItem + lcBolItem. 
                
            END.
            
            lcConcatPrintBarCode = "".
            IF lPrintBarCodeByPartNo THEN
                FOR EACH oe-boll
                    WHERE oe-boll.company    EQ ttBolHeader.company
                    AND oe-boll.loc EQ ttBolHeader.locationID
                    AND oe-boll.bol-no      EQ ttBolHeader.bolID 
                    AND oe-boll.tag <> ""
                    BREAK BY oe-boll.i-no BY oe-boll.tag  :
            
                    lcPrintBarCode = lcPrintBarCodeData.
                    FIND FIRST itemfg OF oe-boll NO-LOCK NO-ERROR.
                    IF FIRST-OF(oe-boll.i-no) OR iLineOfTags >= 5 THEN 
                    DO:                    
                        ASSIGN 
                            iBarLine    = 1
                            iBarcount   = 0
                            iLineOfTags = 0
                            .
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "FirstBarCode", "Yes").                     
                    END.
                    ELSE
                        oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "FirstBarCode", "No"). 
                
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarLine", STRING(iBarline)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarCount", STRING(iBarCount)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintLineOfTags", STRING(iLineOfTags)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarBol", STRING(oe-boll.bol-no)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarPartNo", STRING(itemfg.part-no)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarCustName", STRING(cust.NAME)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "PrintBarTag", STRING(oe-boll.tag)).
               
                    iBarCount = iBarCount + 1.

                    IF iBarCount > 1 OR last-of(oe-boll.i-no) THEN 
                    DO:             

                        ASSIGN 
                            iBarCount   = 0
                            iBarLine    = iBarLine + 2
                            iLineOfTags = iLineOfTags + 1.                                
                    END.          
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcPrintBarCode, "LastBarTag", STRING(LAST-OF(oe-boll.i-no))).
                    lcConcatPrintBarCode = lcConcatPrintBarCode + lcPrintBarCode.
                END.                           

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "FirstBolHeader", STRING(lFirstBol)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "LastCustomer", STRING(lLastCustomer)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "SalesMan", STRING(ttBolHeader.salesMan)).            
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "FreightTerms", STRING(ttBolHeader.frtTerms)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "FreightTermsCAN", STRING(ttBolHeader.frtTermsCAN)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "CustomerPO", STRING(ttBolHeader.poNo)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "JobNumber", STRING(ttBolHeader.JobNo)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "FOB", STRING(ttBolHeader.fob)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "ShipToBroker", STRING(ttBolHeader.shipToBroker)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "ShipName", STRING(ttBolHeader.ShipName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "ShipAddr1", STRING(ttBolHeader.ShipAddr[1])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "ShipAddr2", STRING(ttBolHeader.ShipAddr[2])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "ShipAddr3", STRING(ttBolHeader.ShipAddr3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "CustPhoneNum", STRING(ttBolHeader.CustPhoneNum)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "shipPhone", STRING(ttBolHeader.shipPhone)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "bolPhone", STRING(ttBolHeader.bolPhone)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "shiptoContact", STRING(ttBolHeader.shiptoContact)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyAdd1", STRING(ttBolHeader.companyAdd1)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyAdd2", STRING(ttBolHeader.companyAdd2)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyAdd3", STRING(ttBolHeader.companyAdd3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyAdd4", STRING(ttBolHeader.companyAdd4)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyAdd5", STRING(ttBolHeader.companyAdd5)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyEmail", STRING(ttBolHeader.companyEmail)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "companyName", STRING(ttBolHeader.companyName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "soldtoName", STRING(ttBolHeader.soldtoName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "soldtoAddr1", STRING(ttBolHeader.soldtoAddr[1])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "soldtoAddr2", STRING(ttBolHeader.soldtoAddr[2])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "soldtoAddr3", STRING(ttBolHeader.soldtoAddr3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "totalWeight", STRING(ttBolHeader.totalWeight)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcBolHeader, "totalCases", STRING(ttBolHeader.totalCases)).
                        
            lcBolHeader = REPLACE(lcBolHeader, "$BolItems$", lcConcatBolItem).
            lcBolHeader = REPLACE(lcBolHeader, "$BolBarCode$", lcConcatPrintBarCode).
            
            lcBolHeader = oAttribute:ReplaceAttributes(lcBolHeader, BUFFER bf-oe-bolh:HANDLE).            
            lcBolHeader = oAttribute:ReplaceAttributes(lcBolHeader, BUFFER cust:HANDLE).
            lcBolHeader = oAttribute:ReplaceAttributes(lcBolHeader, BUFFER terms:HANDLE).
            lcBolHeader = oAttribute:ReplaceAttributes(lcBolHeader, BUFFER carrier:HANDLE).
            
            RUN pGetRequestData ("PageHeader", OUTPUT lcPageHeader).
                        
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "FreightTerms", STRING(ttBolHeader.frtTerms)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "FreightTermsCAN", STRING(ttBolHeader.frtTermsCAN)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "CustomerPO", STRING(ttBolHeader.poNo)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "JobNumber", STRING(ttBolHeader.JobNo)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "FOB", STRING(ttBolHeader.fob)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "ShipToBroker", STRING(ttBolHeader.shipToBroker)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "ShipName", STRING(ttBolHeader.ShipName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "ShipAddr1", STRING(ttBolHeader.ShipAddr[1])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "ShipAddr2", STRING(ttBolHeader.ShipAddr[2])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "ShipAddr3", STRING(ttBolHeader.ShipAddr3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "CustPhoneNum", STRING(ttBolHeader.CustPhoneNum)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "shipPhone", STRING(ttBolHeader.shipPhone)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "bolPhone", STRING(ttBolHeader.bolPhone)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "shiptoContact", STRING(ttBolHeader.shiptoContact)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyAdd1", STRING(ttBolHeader.companyAdd1)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyAdd2", STRING(ttBolHeader.companyAdd2)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyAdd3", STRING(ttBolHeader.companyAdd3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyAdd4", STRING(ttBolHeader.companyAdd4)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyAdd5", STRING(ttBolHeader.companyAdd5)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyEmail", STRING(ttBolHeader.companyEmail)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "companyName", STRING(ttBolHeader.companyName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "soldtoName", STRING(ttBolHeader.soldtoName)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "soldtoAddr1", STRING(ttBolHeader.soldtoAddr[1])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "soldtoAddr2", STRING(ttBolHeader.soldtoAddr[2])).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "soldtoAddr3", STRING(ttBolHeader.soldtoAddr3)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "totalWeight", STRING(ttBolHeader.totalWeight)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcPageHeader, "totalCases", STRING(ttBolHeader.totalCases)).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER bf-oe-bolh:HANDLE).            
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER cust:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER terms:HANDLE).
            lcPageHeader = oAttribute:ReplaceAttributes(lcPageHeader, BUFFER carrier:HANDLE).             
                                         
            RUN pInsertPageHeaderFooter (INPUT iNumLinesInPage, INPUT-OUTPUT lcBolHeader, INPUT lcPageHeader, INPUT lcPageFooter).
            
            lcConcatBolHeader = lcConcatBolHeader + lcBolHeader.              
                    
        END.         
    END.

    RUN pGetRequestData(INPUT "BolHeaderGroupHeader", OUTPUT lcBolHeaderGroupHeader).
    RUN pGetRequestData(INPUT "BolHeaderGroupFooter", OUTPUT lcBolHeaderGroupFooter).
    RUN pGetRequestData(INPUT "ReportHeader", OUTPUT lcReportHeader).
    RUN pGetRequestData(INPUT "ReportFooter", OUTPUT lcReportFooter).
        
    ioplcRequestData = REPLACE(ioplcRequestData, "$BolHeaders$", lcConcatBolHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$BolHeaderGroupHeader$", lcBolHeaderGroupHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$BolHeaderGroupFooter$", lcBolHeaderGroupFooter).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportHeader$", lcReportHeader).
    ioplcRequestData = REPLACE(ioplcRequestData, "$ReportFooter$", lcReportFooter).
    ioplcRequestData = REPLACE(ioplcRequestData, "$BoxDesignHeaders$", lcConcatBoxDesignHeader).
    
    RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
    
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BusinessFormLogo", cBusinessFormLogo).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "LogoColor", cLogoColor).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "DisplayCompanyAddress", cDisplayCompanyAddress).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "BolFmtIntegerValue", cBolFmtIntegerValue). 

    ASSIGN   
        opcMessage = ""
        oplSuccess = TRUE
        .
END.        

FINALLY:
    IF VALID-HANDLE (hdBolProcs) THEN
        DELETE PROCEDURE hdBolProcs.
END FINALLY.        


/* **********************  Internal Procedures  *********************** */




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
        AND bf-APIOutboundDetail.parentID      EQ "SendBol"
        NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
        WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Header"
        AND bf-APIOutboundDetail.parentID      EQ "SendBol"
        NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:
        lcHeader = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Header" + "$", lcHeader).
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
        WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Footer"
        AND bf-APIOutboundDetail.parentID      EQ "SendBol"
        NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:
        lcFooter = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Footer" + "$", lcFooter).
    END.    
END PROCEDURE.

PROCEDURE pReplaceNotesText:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcNoteText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTempNote       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIterations     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTextLineLength AS INTEGER   NO-UNDO INIT 80.
    ASSIGN
        iopcNoteText = REPLACE(iopcNoteText, CHR(10) + CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(10), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, "  ", " ")
        .
    
    IF LENGTH(iopcNoteText) GT iTextLineLength THEN 
    DO:
        iIterations = (LENGTH(iopcNoteText) / iTextLineLength) + INT(LENGTH(iopcNoteText) MOD iTextLineLength GT 0).
        DO iIndex = 1 TO iIterations:
            cTempNote = cTempNote + CHR(10) + /*"linefeed" +*/ SUBSTRING(iopcNoteText, iIndex * iTextLineLength - (iTextLineLength - 1), iTextLineLength). 
        END.
    END.
    ELSE
        cTempNote = iopcNoteText.
    
    cTempNote = TRIM(cTempNote, CHR(10)).
    
    iopcNoteText = cTempNote.
END PROCEDURE.
