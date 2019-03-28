
/*------------------------------------------------------------------------
    File        : ttInventoryTag.i
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
    FIELD inventoryStockID AS CHARACTER
    FIELD stockIDAlias AS CHARACTER	
    FIELD wipItemID AS CHARACTER
    FIELD jobNo AS CHARACTER
    FIELD jobID AS CHARACTER
    FIELD jobID2 AS INTEGER
    FIELD formNo AS INTEGER
    FIELD blankNo AS INTEGER
    FIELD customerID AS CHARACTER
    FIELD customerName AS CHARACTER
    FIELD machineID AS CHARACTER
    FIELD machineName AS CHARACTER
    FIELD passNo AS INTEGER
    FIELD nextMachineID AS CHARACTER
    FIELD nextMachineName AS CHARACTER
    FIELD quantity AS DECIMAL
    FIELD quantityPerSubUnit AS DECIMAL
    FIELD quantitySubUnitsPerUnit AS INTEGER
    FIELD quantityOfSubUnits AS INTEGER
    FIELD quantityOfUnits AS INTEGER
    FIELD quantityPartial AS DECIMAL
    FIELD quantityUOM AS CHARACTER
    FIELD copyNo AS INTEGER
    FIELD copies AS CHARACTER
    FIELD rmItemID AS CHARACTER
    FIELD rmItemName AS CHARACTER.

DEFINE VARIABLE gcStatusStockPreLoadtag    AS CHARACTER INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag       AS CHARACTER INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial       AS CHARACTER INITIAL "Initialized".
DEFINE VARIABLE gcStatusStockReceived      AS CHARACTER INITIAL "On-Hand".
DEFINE VARIABLE gcStatusStockConsumed      AS CHARACTER INITIAL "Consumed".

DEFINE VARIABLE gcStatusTransactionInitial AS CHARACTER INITIAL "Pending".
DEFINE VARIABLE gcStatusTransactionPosted  AS CHARACTER INITIAL "Posted".

DEFINE VARIABLE gcTransactionTypeReceive   AS CHARACTER INITIAL "R".
DEFINE VARIABLE gcTransactionTypeTransfer  AS CHARACTER INITIAL "T".
DEFINE VARIABLE gcTransactionTypeConsume   AS CHARACTER INITIAL "I".
DEFINE VARIABLE gcTransactionTypeShip      AS CHARACTER INITIAL "S".

DEFINE VARIABLE gcItemTypeWIP              AS CHARACTER INITIAL "WP".
DEFINE VARIABLE gcItemTypeFG               AS CHARACTER INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM               AS CHARACTER INITIAL "RM".
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
