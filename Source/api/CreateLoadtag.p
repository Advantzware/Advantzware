/*------------------------------------------------------------------------
    File        : api/CreateLoadtag.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : DEVA$!
    Created     : Tue Jan 26 07:33:22 EDT 2021
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
    
    DEFINE VARIABLE lcConcatTagData AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcTagData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iCopies         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cConcatJob      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMiddlesexJob   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMiddlesexPO    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPalletCounter  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cExportFile     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExportTemplate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID          AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdTT         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTTBuffer   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutbound            FOR APIOutbound.
    DEFINE BUFFER bf-tags-APIOutboundDetail FOR APIOutboundDetail.
    
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
        FIND FIRST bf-APIOutbound NO-LOCK
             WHERE bf-APIOutbound.apiOutboundID EQ ipiAPIOutboundID
             NO-ERROR.
        IF NOT AVAILABLE bf-APIOutbound THEN 
        DO:
            ASSIGN
                opcMessage = "No APIOutbound record found"
                oplSuccess = FALSE
                .
            RETURN.        
        END.   
        
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "ExportFile"
             NO-ERROR.
        IF AVAILABLE ttArgs THEN
            cExportFile = ttArgs.argValue.

        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "ExportTemplate"
             NO-ERROR.
        IF AVAILABLE ttArgs THEN
            cExportTemplate = ttArgs.argValue.
        
        FIND FIRST ttArgs
             WHERE ttArgs.argType EQ "ROWID"
               AND ttArgs.argKey  EQ "TTLoadTagHandle"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "Missing TTLoadTag handle"
                oplSuccess = FALSE
                .
            RETURN.                
        END.
        
        hdTT = HANDLE(ttArgs.argValue).
        hdTTBuffer = hdTT:DEFAULT-BUFFER-HANDLE.

        CREATE QUERY hdQuery.
        hdQuery:ADD-BUFFER(hdTTBuffer).
        
        cQueryString = "FOR EACH ttLoadTag"
                     + " " + "WHERE ttLoadTag.exportTemplate EQ '" + cExportTemplate + "'"
                     + " " + "AND ttLoadTag.tagStatus EQ 'Created'"
                     + " " + "AND ttLoadTag.isError EQ FALSE"
                     + " " + "AND ttLoadTag.isSelected EQ TRUE".
        
        IF cExportFile NE "" THEN
            cQueryString = cQueryString + " " + "AND ttLoadTag.exportFile EQ '" + cExportFile + "'".
                         
        hdQuery:QUERY-PREPARE(cQueryString).
        
        hdQuery:QUERY-OPEN().
        
        FIND FIRST bf-tags-APIOutboundDetail NO-LOCK
             WHERE bf-tags-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-tags-APIOutboundDetail.detailID      EQ "ItemTag"
               AND bf-tags-APIOutboundDetail.parentID      EQ bf-APIOutbound.apiID
             NO-ERROR.
        IF AVAILABLE bf-tags-APIOutboundDetail THEN DO:
            REPEAT:
                hdQuery:GET-NEXT ().
                IF hdQuery:QUERY-OFF-END THEN
                    LEAVE.
                
                cJobID = hdTTBuffer:BUFFER-FIELD("jobID"):BUFFER-VALUE.
                
                cConcatJob = IF cJobID EQ "" THEN 
                                 STRING(hdTTBuffer:BUFFER-FIELD("orderID"):BUFFER-VALUE) 
                             ELSE 
                                 hdTTBuffer:BUFFER-FIELD("jobID"):BUFFER-VALUE + "-" + STRING(hdTTBuffer:BUFFER-FIELD("jobID2"):BUFFER-VALUE, "99"). 
                IF iPalletCounter EQ 0 THEN
                    iPalletCounter = hdTTBuffer:BUFFER-FIELD("palletCounter"):BUFFER-VALUE.

                ASSIGN
                    cMiddlesexPO  = SUBSTRING(TRIM(hdTTBuffer:BUFFER-FIELD("jobID"):BUFFER-VALUE),1,6)
                    cMiddlesexJob = IF cMiddlesexJob EQ "" THEN 
                                        "" 
                                    ELSE
                                        "%MX" + FILL("0",6 - LENGTH(TRIM(cMiddlesexJob))) + TRIM(cMiddlesexJob)
                    cMiddlesexPO  = SUBSTRING(TRIM(hdTTBuffer:BUFFER-FIELD("custPONO"):BUFFER-VALUE),1,6)
                    cMiddlesexPO  = IF cMiddlesexPO EQ "" THEN 
                                        "" 
                                    ELSE
                                        "BNJ" + FILL("0",6 - LENGTH(TRIM(cMiddlesexPO))) + TRIM(cMiddlesexPO)
                    .
                                                
                DO iCopies = 1 TO hdTTBuffer:BUFFER-FIELD("printCopies"):BUFFER-VALUE:
                    ASSIGN
                        lcTagData                                             = bf-tags-APIOutboundDetail.data
                        hdTTBuffer:BUFFER-FIELD("palletCounter"):BUFFER-VALUE = iPalletCounter
                        .
                    
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Customer", hdTTBuffer:BUFFER-FIELD("custName"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "OrderNumber", STRING(hdTTBuffer:BUFFER-FIELD("orderID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "BOLID", STRING(hdTTBuffer:BUFFER-FIELD("bolID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "JobNumber", cConcatJob).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemID", CAPS(hdTTBuffer:BUFFER-FIELD("itemID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CustPartNo", hdTTBuffer:BUFFER-FIELD("custPartNo"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CustPoNo", hdTTBuffer:BUFFER-FIELD("custPONo"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "QuantityInSubUnit", STRING(hdTTBuffer:BUFFER-FIELD("quantityInSubUnit"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SubUnitsPerUnit", STRING(hdTTBuffer:BUFFER-FIELD("subUnitsPerUnit"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Total", STRING(hdTTBuffer:BUFFER-FIELD("quantityInUnit"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipCode", hdTTBuffer:BUFFER-FIELD("shipID"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipName", hdTTBuffer:BUFFER-FIELD("shipName"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipAddress1", hdTTBuffer:BUFFER-FIELD("shipAddress1"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipAddress2", hdTTBuffer:BUFFER-FIELD("shipAddress2"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipCity", hdTTBuffer:BUFFER-FIELD("shipCity"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipState", hdTTBuffer:BUFFER-FIELD("shipState"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipCountry", hdTTBuffer:BUFFER-FIELD("shipCountry"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ShipZip", hdTTBuffer:BUFFER-FIELD("shipZip"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldCode", hdTTBuffer:BUFFER-FIELD("soldID"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldName", hdTTBuffer:BUFFER-FIELD("soldName"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldAddress1", hdTTBuffer:BUFFER-FIELD("soldAddress1"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldAddress2", hdTTBuffer:BUFFER-FIELD("soldAddress2"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldCity", hdTTBuffer:BUFFER-FIELD("soldCity"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldState", hdTTBuffer:BUFFER-FIELD("soldState"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldCountry", hdTTBuffer:BUFFER-FIELD("soldCountry"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SoldZip", hdTTBuffer:BUFFER-FIELD("soldZip"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemName", hdTTBuffer:BUFFER-FIELD("itemName"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DueDate", STRING(hdTTBuffer:BUFFER-FIELD("dueDate"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ReleaseDate", STRING(hdTTBuffer:BUFFER-FIELD("relDate"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "UpcNo", STRING(hdTTBuffer:BUFFER-FIELD("upcNo"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Length", STRING(hdTTBuffer:BUFFER-FIELD("boxLen"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Width", STRING(hdTTBuffer:BUFFER-FIELD("boxWid"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Depth", STRING(hdTTBuffer:BUFFER-FIELD("boxDep"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Flute", hdTTBuffer:BUFFER-FIELD("flute"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Test", hdTTBuffer:BUFFER-FIELD("test"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Vendor", hdTTBuffer:BUFFER-FIELD("vendor"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "GrossWeight", STRING(hdTTBuffer:BUFFER-FIELD("grossWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "TareWeight", STRING(hdTTBuffer:BUFFER-FIELD("tareWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "NetWeight", STRING(hdTTBuffer:BUFFER-FIELD("netWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SheetWeight", STRING(hdTTBuffer:BUFFER-FIELD("sheetWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "UOM", hdTTBuffer:BUFFER-FIELD("uom"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Style", hdTTBuffer:BUFFER-FIELD("style"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "StyleDesc", hdTTBuffer:BUFFER-FIELD("styleDesc"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ReleaseLotID", STRING(hdTTBuffer:BUFFER-FIELD("relLotID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "MiddlesexJobNumber", cMiddlesexJob).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "MiddlesexCustPoNo", cMiddlesexPO).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "TagNo", hdTTBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Partial", hdTTBuffer:BUFFER-FIELD("partial"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CaseCode", hdTTBuffer:BUFFER-FIELD("caseNo"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN1", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(1)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN2", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(2)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN3", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(3)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN4", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(4)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN5", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(5)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN6", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(6)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN7", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(7)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SN8", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(8)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PoID", STRING(hdTTBuffer:BUFFER-FIELD("poID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN1", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(9)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN2", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(10)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN3", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(11)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN4", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(12)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN5", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(13)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN6", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(14)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN7", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(15)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN8", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(16)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN9", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(17)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DN10", hdTTBuffer:BUFFER-FIELD("deptNotes"):BUFFER-VALUE(18)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "EstimateID", STRING(hdTTBuffer:BUFFER-FIELD("estID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "OrderDesc1", hdTTBuffer:BUFFER-FIELD("orderDesc1"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "OrderDesc2", hdTTBuffer:BUFFER-FIELD("orderDesc2"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CounterID", SUBSTRING(hdTTBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE, 16, 5)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RfidTag", hdTTBuffer:BUFFER-FIELD("rfidTag"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DueDateJobLine", hdTTBuffer:BUFFER-FIELD("dueDateJobhdr"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "DueDateJob", hdTTBuffer:BUFFER-FIELD("dueDateJob"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "LineID", STRING(hdTTBuffer:BUFFER-FIELD("lineID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "UnitWeight", STRING(hdTTBuffer:BUFFER-FIELD("unitWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PalletWeight", STRING(hdTTBuffer:BUFFER-FIELD("palletWeight"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemDesc1", hdTTBuffer:BUFFER-FIELD("partDscr1"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemDesc2", hdTTBuffer:BUFFER-FIELD("partDscr2"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemDesc3", hdTTBuffer:BUFFER-FIELD("partDscr3"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "ItemLotID", STRING(hdTTBuffer:BUFFER-FIELD("lotID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PalletCode", STRING(hdTTBuffer:BUFFER-FIELD("palletID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PalletId", STRING(hdTTBuffer:BUFFER-FIELD("palletCounter"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "TagCounter", STRING(hdTTBuffer:BUFFER-FIELD("tagCounter"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "TagCountTotal", STRING(hdTTBuffer:BUFFER-FIELD("totalTags"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RN1", hdTTBuffer:BUFFER-FIELD("shipNotes"):BUFFER-VALUE(1)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RN2", hdTTBuffer:BUFFER-FIELD("shipNotes"):BUFFER-VALUE(2)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RN3", hdTTBuffer:BUFFER-FIELD("shipNotes"):BUFFER-VALUE(3)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RN4", hdTTBuffer:BUFFER-FIELD("shipNotes"):BUFFER-VALUE(4)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "WarehouseID", hdTTBuffer:BUFFER-FIELD("warehouseID"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "LocationID", hdTTBuffer:BUFFER-FIELD("locationID"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "JobQuantity", STRING(hdTTBuffer:BUFFER-FIELD("jobQuantity"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "RunShip", STRING(hdTTBuffer:BUFFER-FIELD("runShip"):BUFFER-VALUE, "R&S/WHSE")).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PalletType", STRING(hdTTBuffer:BUFFER-FIELD("itemPalletID"):BUFFER-VALUE)).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "Zone", hdTTBuffer:BUFFER-FIELD("zoneID"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CreatedBy", hdTTBuffer:BUFFER-FIELD("createdUser"):BUFFER-VALUE).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CreateDate", STRING(hdTTBuffer:BUFFER-FIELD("createdDate"):BUFFER-VALUE, "99/99/9999")).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "CreateTime", STRING(hdTTBuffer:BUFFER-FIELD("createdTime"):BUFFER-VALUE, "HH:MM AM")).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PrintDate", STRING(TODAY, "99/99/9999")).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "PrintTime", STRING(TIME, "HH:MM AM")).
                    RUN updateRequestData(INPUT-OUTPUT lcTagData, "SSCC", hdTTBuffer:BUFFER-FIELD("sscc"):BUFFER-VALUE).
                    
                    ASSIGN
                        iPalletCounter                                    = iPalletCounter + 1
                        hdTTBuffer:BUFFER-FIELD("tagStatus"):BUFFER-VALUE = "Printed"
                        .
                    
                    lcConcatTagData = lcConcatTagData + lcTagData. 
                END.
            END.            
        END.

        ioplcRequestData = REPLACE(ioplcRequestData, "$ItemTags$", lcConcatTagData).
        
        COPY-LOB ioplcRequestData TO FILE cExportFile NO-ERROR.
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
        
        IF VALID-HANDLE(hdQuery) THEN DO:
            hdQuery:QUERY-CLOSE().
            DELETE OBJECT hdQuery.
        END.
    END.        
        
