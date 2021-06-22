DEFINE TEMP-TABLE ttInventoryStockLoadtagWIP NO-UNDO
    LIKE inventoryStock
    FIELD quantityTotal           AS DECIMAL  
    FIELD quantityTotalRunning    AS DECIMAL 
    FIELD countOfLoadtags         AS INTEGER 
    FIELD countOfLabelsPerLoadtag AS INTEGER
    FIELD labelTemplate           AS CHARACTER 
    FIELD labelProgram            AS CHARACTER 
    FIELD countOfLabels           AS INTEGER
    FIELD nextMachineID           AS CHARACTER 
    FIELD nextMachineName         AS CHARACTER
    FIELD orderCustomerID         AS CHARACTER
    FIELD orderCustomerName       AS CHARACTER
    FIELD lastMachineName         AS CHARACTER 
    FIELD rmItemName              AS CHARACTER 
    .   
