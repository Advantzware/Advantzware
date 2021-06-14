DEFINE TEMP-TABLE ttPhysicalBrowseInventory NO-UNDO
    FIELD company          AS CHARACTER
    FIELD inventoryStockID AS CHARACTER
    FIELD tag              AS CHARACTER
    FIELD itemID           AS CHARACTER
    FIELD quantity         AS DECIMAL
    FIELD origQuantity     AS DECIMAL
    FIELD locationID       AS CHARACTER
    FIELD warehouseID      AS CHARACTER
    FIELD location         AS CHARACTER
    FIELD origLocationID   AS CHARACTER
    FIELD origWarehouseID  AS CHARACTER
    FIELD origLocation     AS CHARACTER
    FIELD inventoryStatus  AS CHARACTER
    FIELD itemType         AS CHARACTER
    FIELD customerID       AS CHARACTER
    FIELD lastTransTime    AS DATETIME
    .