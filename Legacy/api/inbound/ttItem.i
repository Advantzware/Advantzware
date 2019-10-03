DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD WarehouseID              AS CHARACTER
    FIELD LocationID               AS CHARACTER
    FIELD PrimaryID                AS CHARACTER
    FIELD InventoryStockID         AS CHARACTER
    FIELD Quantity                 AS DECIMAL
    FIELD ItemType                 AS CHARACTER  
    FIELD StockIDAlias             AS CHARACTER
    FIELD QuantityUOM              AS CHARACTER
    FIELD QuantityPerSubUnit       AS DECIMAL
    FIELD QuantitySubUnitsPerUnit  AS INTEGER
    FIELD QuantityPartial          AS DECIMAL
    .
