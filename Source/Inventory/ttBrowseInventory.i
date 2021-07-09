DEFINE TEMP-TABLE ttBrowseInventory NO-UNDO
    LIKE  inventoryStock
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
    FIELD transactionType         AS CHARACTER
    FIELD locDscr                 AS CHARACTER
    FIELD leadDays                AS INTEGER
    FIELD orderLevel              AS INTEGER
    FIELD orderMax                AS INTEGER
    FIELD orderMin                AS INTEGER
    FIELD quantityOnHand          AS DECIMAL
    FIELD quantityOnOrder         AS DECIMAL
    FIELD quantityAllocated       AS DECIMAL
    FIELD quantityBackOrder       AS DECIMAL
    FIELD quantityAvailable       AS DECIMAL
    .