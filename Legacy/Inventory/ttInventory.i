
/*------------------------------------------------------------------------
    File        : ttInventory.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definition for ttInventoryTag used in LoadtagProcs.p for generating loadtags

    Author(s)   : BV
    Created     : Sun Mar 03 18:38:28 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    
DEFINE {1} TEMP-TABLE ttInventoryStockPreLoadtag
    LIKE inventoryStock
    FIELD quantityTotal AS DECIMAL  
    FIELD quantityTotalRunning AS DECIMAL 
    FIELD countOfLoadtags AS INTEGER 
    FIELD countOfLabelsPerLoadtag AS INTEGER
    FIELD labelTemplate AS CHARACTER 
    FIELD labelProgram AS CHARACTER 
    .   

DEFINE {1} TEMP-TABLE ttInventoryStockLoadtag
     LIKE ttInventoryStockPreLoadtag
     FIELD countOfLabels AS INTEGER
     .    
     
DEFINE {1} TEMP-TABLE ttInventoryStockLoadtagWIP
    LIKE ttInventoryStockLoadtag
    FIELD nextMachineID AS CHARACTER 
    FIELD nextMachineName AS CHARACTER
    FIELD orderCustomerID AS CHARACTER
    FIELD orderCustomerName AS CHARACTER
    FIELD lastMachineName AS CHARACTER 
    FIELD rmItemName AS CHARACTER 
    .

DEFINE {1} TEMP-TABLE ttPrintInventoryStock
    FIELD inventoryStockID AS CHARACTER LABEL "InventoryStockID"
    FIELD stockIDAlias AS CHARACTER	LABEL "WIPTag"
    FIELD wipItemID AS CHARACTER LABEL "WIPItemID"
    FIELD jobNumber AS CHARACTER LABEL "JobNumber"
    FIELD jobRunNumber AS INTEGER LABEL "JobRunNumber"
    FIELD jobID AS CHARACTER LABEL "JobID"
    FIELD jobIDTrimmed AS CHARACTER LABEL "JobIDTrimmed"
    FIELD jobIDFullTrimmed AS CHARACTER LABEL "JobIDFullTrimmed"
    FIELD formNo AS INTEGER LABEL "Form"
    FIELD blankNo AS INTEGER LABEL "Blank"
    FIELD customerID AS CHARACTER LABEL "CustomerID"
    FIELD customerName AS CHARACTER LABEL "CustomerName"
    FIELD machineID AS CHARACTER LABEL "MachineID"
    FIELD machineName AS CHARACTER LABEL "MachineName"
    FIELD passNo AS INTEGER LABEL "Pass"
    FIELD nextMachineID AS CHARACTER LABEL "NextMachineID"
    FIELD nextMachineName AS CHARACTER LABEL "NextMachineName"
    FIELD quantity AS DECIMAL LABEL "Quantity"
    FIELD quantityPerSubUnit AS DECIMAL LABEL "QuantityPerSubUnit"
    FIELD quantitySubUnitsPerUnit AS INTEGER LABEL "QuantitySubUnitsPerUnit"
    FIELD quantityOfSubUnits AS INTEGER LABEL "QuantityOfSubUnits"
    FIELD quantityOfUnits AS INTEGER LABEL "QuantityOfUnits"
    FIELD quantityPartial AS DECIMAL LABEL "QuantityPartial"
    FIELD quantityUOM AS CHARACTER LABEL "QuantityUOM"
    FIELD copyNo AS INTEGER LABEL "CopyNo"
    FIELD copies AS CHARACTER LABEL "Copies"
    FIELD rmItemID AS CHARACTER LABEL "RMItemID"
    FIELD rmItemName AS CHARACTER LABEL "RMItemName"
    FIELD company AS CHARACTER LABEL "Company"
    .

DEFINE {1} TEMP-TABLE ttBrowseInventory
    LIKE ttInventoryStockLoadtagWIP
    .

DEFINE {1} TEMP-TABLE ttPhysicalBrowseInventory
    FIELD company                AS CHARACTER
    FIELD inventoryStockID       AS CHARACTER
    FIELD stockIDAlias           AS CHARACTER
    FIELD itemID                 AS CHARACTER
    FIELD quantity               AS DECIMAL
    FIELD origQuantity           AS DECIMAL
    FIELD locationID             AS CHARACTER
    FIELD warehouseID            AS CHARACTER
    FIELD location               AS CHARACTER
    FIELD origLocationID         AS CHARACTER
    FIELD origWarehouseID        AS CHARACTER
    FIELD origLocation           AS CHARACTER
    FIELD inventoryStatus        AS CHARACTER
    FIELD itemType               AS CHARACTER
    FIELD customerID             AS CHARACTER
    FIELD lastTransTime          AS DATETIME
    .

DEFINE TEMP-TABLE ttInventoryStockDetails NO-UNDO
    LIKE inventoryStock
    .

DEFINE VARIABLE gcStatusStockPreLoadtag    AS CHARACTER INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag       AS CHARACTER INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial       AS CHARACTER INITIAL "Created".
DEFINE VARIABLE gcStatusStockReceived      AS CHARACTER INITIAL "On-Hand".
DEFINE VARIABLE gcStatusStockConsumed      AS CHARACTER INITIAL "Consumed".

DEFINE VARIABLE gcStatusSnapshotNotScanned      AS CHARACTER INITIAL "Not Scanned".
DEFINE VARIABLE gcStatusSnapshotNotScannedConf  AS CHARACTER INITIAL "Not Scanned - Confirmed".
DEFINE VARIABLE gcStatusSnapshotCompleteMatch   AS CHARACTER INITIAL "Complete Match".
DEFINE VARIABLE gcStatusSnapshotLocChange       AS CHARACTER INITIAL "Location Change".
DEFINE VARIABLE gcStatusSnapshotQtyChange       AS CHARACTER INITIAL "Quantity Change".
DEFINE VARIABLE gcStatusSnapshotQtyAndLocChange AS CHARACTER INITIAL "Quantity and Location Change".
DEFINE VARIABLE gcStatusSnapshotTagNotFound     AS CHARACTER INITIAL "Tag Not Found".

DEFINE VARIABLE gcSourceTypeSnapshot       AS CHARACTER INITIAL "Snapshot".

DEFINE VARIABLE gcStatusTransactionInitial AS CHARACTER INITIAL "Pending".
DEFINE VARIABLE gcStatusTransactionPosted  AS CHARACTER INITIAL "Posted".

DEFINE VARIABLE gcTransactionTypeReceive   AS CHARACTER INITIAL "R".
DEFINE VARIABLE gcTransactionTypeTransfer  AS CHARACTER INITIAL "T".
DEFINE VARIABLE gcTransactionTypeConsume   AS CHARACTER INITIAL "I".
DEFINE VARIABLE gcTransactionTypeShip      AS CHARACTER INITIAL "S".
DEFINE VARIABLE gcTransactionTypeCompare   AS CHARACTER INITIAL "C".

DEFINE VARIABLE gcSnapshotTypeCount        AS CHARACTER INITIAL "C". /* Count */
DEFINE VARIABLE gcSnapshotTypeCapture      AS CHARACTER INITIAL "R". /* Report Capture */
DEFINE VARIABLE gcSnapshotTypeArchive      AS CHARACTER INITIAL "A". /* Archive */

DEFINE VARIABLE gcItemTypeWIP              AS CHARACTER INITIAL "WP".
DEFINE VARIABLE gcItemTypeFG               AS CHARACTER INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM               AS CHARACTER INITIAL "RM".

DEFINE VARIABLE gcDBUser                   AS CHARACTER INITIAL "asi".    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
