DEFINE TEMP-TABLE ttOrder NO-UNDO
    FIELD orderSeqID      AS INTEGER   LABEL "OrderSeqID"
    FIELD payLoadID       AS CHARACTER LABEL "PayloadID" 
    FIELD fromDomain      AS CHARACTER LABEL "FromDomain"
    FIELD fromIdentity    AS CHARACTER LABEL "FromIdentity"
    FIELD toDomain        AS CHARACTER LABEL "ToDomain"
    FIELD toIdentity      AS CHARACTER LABEL "ToIdentity"
    FIELD senderDomain    AS CHARACTER LABEL "SenderDomain"
    FIELD senderIdentity  AS CHARACTER LABEL "SenderIdentity"
    FIELD sharedSecret    AS CHARACTER LABEL "SharedSecret"
    FIELD userAgent       AS CHARACTER LABEL "UserAgent"
    FIELD company         AS CHARACTER LABEL "Company"
    FIELD warehouseID     AS CHARACTER LABEL "WarehouseID"
    FIELD poID            AS CHARACTER LABEL "CustomerOrderID"
    FIELD orderID         AS INTEGER   LABEL "OrderID"
    FIELD orderDate       AS DATETIME  LABEL "OrderDate"
    FIELD customerID      AS CHARACTER LABEL "CustomerID"
    FIELD totalCost       AS DECIMAL   LABEL "TotalCost"
    FIELD shipToID        AS CHARACTER LABEL "ShipToID"
    FIELD shipToName      AS CHARACTER LABEL "ShipToName"
    FIELD shipToDeliverTo AS CHARACTER LABEL "ShipToDeliverTo"
    FIELD shipToAddress1  AS CHARACTER LABEL "ShipToAddress1"
    FIELD shipToAddress2  AS CHARACTER LABEL "ShipToAddress2"
    FIELD shipToCity      AS CHARACTER LABEL "ShipToCity"
    FIELD shipToState     AS CHARACTER LABEL "ShipToState"
    FIELD shipToZip       AS CHARACTER LABEL "ShipToZip"
    FIELD shipToCountry   AS CHARACTER LABEL "ShipToCountry"
    FIELD shipToPhone     AS CHARACTER LABEL "Phone"
    FIELD shipToAreaCode  AS CHARACTER LABEL "AreaCode"
    FIELD shipToEmail     AS CHARACTER LABEL "ShipToEmail"
    FIELD billToID        AS CHARACTER LABEL "BillToID"
    FIELD billToName      AS CHARACTER LABEL "BillToName"
    FIELD billToDeliverTo AS CHARACTER LABEL "BillToDeliverTo"
    FIELD billToAddress1  AS CHARACTER LABEL "BillToAddress1"
    FIELD billToAddress2  AS CHARACTER LABEL "BillToAddress2"
    FIELD billToCity      AS CHARACTER LABEL "BillToCity"
    FIELD billToState     AS CHARACTER LABEL "BillToState"
    FIELD billToZip       AS CHARACTER LABEL "BillToZip"
    FIELD billToCountry   AS CHARACTER LABEL "BillToCountry"
    FIELD billToEmail     AS CHARACTER LABEL "BillToEmail"
    FIELD freightCost     AS DECIMAL   LABEL "FreightCost"
    FIELD contactName     AS CHARACTER LABEL "ContactName"
    FIELD contactEmail    AS CHARACTER LABEL "ContactEmail"
    FIELD cardNo          AS CHARACTER LABEL "CardNo"
    FIELD cardExpiryDate  AS DATE      LABEL "CardExpiryDate"
    FIELD cardType        AS CHARACTER LABEL "CardType"
    FIELD promiseDate     AS DATE      LABEL "PromiseDate"
    FIELD comments        AS CHARACTER LABEL "Comments"
    FIELD stat            AS CHARACTER LABEL "Status"
    FIELD importType      AS CHARACTER LABEL "ImportType"       /* cXML, Importer, EDI. Default - Importer */
    FIELD action          AS CHARACTER LABEL "Action"           /* Create, Update, Delete. Default - Create */
    INDEX orderSeqID IS PRIMARY UNIQUE orderSeqID
    .

DEFINE TEMP-TABLE ttOrderLine NO-UNDO
    FIELD orderSeqID              AS INTEGER   LABEL "OrderSeqID"
    FIELD payLoadID               AS CHARACTER LABEL "PayLoadID"
    FIELD company                 AS CHARACTER LABEL "Company"
    FIELD poID                    AS CHARACTER LABEL "CustomerOrderID"
    FIELD orderID                 AS INTEGER   LABEL "OrderID"
    FIELD lineNo                  AS INTEGER   LABEL "LineNo"
    FIELD quantity                AS DECIMAL   LABEL "Quantity"
    FIELD dueDate                 AS DATE      LABEL "DueDate"
    FIELD promiseDate             AS DATE      LABEL "PromiseDate"
    FIELD supplierPartID          AS CHARACTER LABEL "SupplierPartID"
    FIELD supplierPartAuxiliaryID AS CHARACTER LABEL "SupplierPartAuxiliaryID"
    FIELD manufacturerPartID      AS CHARACTER LABEL "ManufacturerPartID"
    FIELD unitPrice               AS DECIMAL   LABEL "UnitPrice"
    FIELD itemName                AS CHARACTER LABEL "ItemName"
    FIELD uom                     AS CHARACTER LABEL "Uom"
    FIELD lineCost                AS DECIMAL   LABEL "LineCost"
    FIELD action                  AS CHARACTER LABEL "Action"    /* Create, Update, Delete */
    INDEX orderSeqID orderSeqID
    .    

DEFINE VARIABLE cOrderActionCreate AS CHARACTER NO-UNDO INITIAL "Create".
DEFINE VARIABLE cOrderActionUpdate AS CHARACTER NO-UNDO INITIAL "Update".
DEFINE VARIABLE cOrderActionDelete AS CHARACTER NO-UNDO INITIAL "Delete".

DEFINE VARIABLE cOrderImportTypeImporter    AS CHARACTER NO-UNDO INITIAL "Importer".
DEFINE VARIABLE cOrderImportTypeEDI         AS CHARACTER NO-UNDO INITIAL "EDI".
DEFINE VARIABLE cOrderImportTypeCXMLMonitor AS CHARACTER NO-UNDO INITIAL "cXMLMonitor".
    