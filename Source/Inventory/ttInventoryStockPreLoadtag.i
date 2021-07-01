DEFINE TEMP-TABLE ttInventoryStockPreLoadtag NO-UNDO
    LIKE inventoryStock
    FIELD quantityTotal           AS DECIMAL  
    FIELD quantityTotalRunning    AS DECIMAL 
    FIELD countOfLoadtags         AS INTEGER 
    FIELD countOfLabelsPerLoadtag AS INTEGER
    FIELD labelTemplate           AS CHARACTER 
    FIELD labelProgram            AS CHARACTER 
    .