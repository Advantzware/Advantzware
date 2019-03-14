
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
    
DEFINE {1} TEMP-TABLE InventoryStock
    FIELD rec_key AS CHARACTER
    FIELD InventoryStockID AS CHARACTER
    FIELD Company AS CHARACTER
    FIELD RMItemID AS CHARACTER
    FIELD FGItemID AS CHARACTER
    FIELD WIPItemID AS CHARACTER
    FIELD PrimaryID AS CHARACTER 
    FIELD ItemType AS CHARACTER
    FIELD JobID AS CHARACTER
    FIELD JobID2 AS INTEGER
    FIELD FormNo AS INTEGER
    FIELD BlankNo AS INTEGER
    FIELD PassNo AS INTEGER
    FIELD PoID AS INTEGER
    FIELD PoLine AS INTEGER
    FIELD WarehouseID AS CHARACTER
    FIELD LocationID AS CHARACTER
    FIELD ZoneID AS CHARACTER 
    FIELD Quantity AS DECIMAL
    FIELD QuantityOriginal AS DECIMAL
    FIELD QuantityPerSubUnit AS DECIMAL
    FIELD QuantitySubUnitsPerUnit AS INTEGER
    FIELD QuantityOfSubUnits AS INTEGER
    FIELD QuantityOfUnits AS INTEGER
    FIELD QuantityPartial AS DECIMAL
    FIELD QuantityOfSubUnitsOriginal AS INTEGER
    FIELD QuantityOfUnitsOriginal AS INTEGER
    FIELD QuantityPartialOriginal AS DECIMAL 
    FIELD QuantityUOM AS CHARACTER
    FIELD CostStandardPerUOM AS DECIMAL
    FIELD CostActualPerUOM AS DECIMAL
    FIELD CostDetailID AS INTEGER 
    FIELD CostUOM AS CHARACTER
    FIELD CreatedTime AS DATETIME
    FIELD CreatedBy AS CHARACTER
    FIELD ConsumedTime AS DATETIME
    FIELD ConsumedBy AS CHARACTER 
    FIELD BOLID AS CHARACTER
    FIELD MachineID AS CHARACTER
    FIELD Consumer AS CHARACTER
    FIELD SourceID AS CHARACTER
    FIELD SourceType AS CHARACTER
    FIELD Lot AS CHARACTER
    FIELD CustomerID AS CHARACTER
    FIELD OrderID AS INTEGER
    FIELD OrderLine AS INTEGER
    FIELD ReleaseID AS INTEGER
    FIELD ReleaseLine AS INTEGER
    FIELD InventoryStatus AS CHARACTER /*Loadtag, Posted, Allocated, etc.*/
    FIELD DimEachLen AS DECIMAL
    FIELD DimEachWid AS DECIMAL 
    FIELD DimEachDep AS DECIMAL
    FIELD DimEachUOM AS CHARACTER
    FIELD InventoryStockLen AS DECIMAL
    FIELD InventoryStockWid AS DECIMAL 
    FIELD InventoryStockDep AS DECIMAL
    FIELD InventoryStockUOM AS CHARACTER  
    FIELD BasisWeight AS DECIMAL 
    FIELD BasisWeightUOM AS CHARACTER 
    FIELD WeightTare AS DECIMAL 
    FIELD WeightNet AS DECIMAL
    FIELD WeightTotal AS DECIMAL  
    FIELD WeightUOM AS CHARACTER  
    FIELD StockIDAlias AS CHARACTER
    . 
    
DEFINE {1} TEMP-TABLE ttInventoryStockPreLoadtag
    LIKE InventoryStock
    FIELD QuantityTotal AS DECIMAL  
    FIELD QuantityTotalRunning AS DECIMAL 
    FIELD CountOfLoadtags AS INTEGER 
    FIELD CountOfLabelsPerLoadtag AS INTEGER
    FIELD LabelTemplate AS CHARACTER 
    FIELD LabelProgram AS CHARACTER 
    .   

DEFINE {1} TEMP-TABLE ttInventoryStockLoadtag
     LIKE ttInventoryStockPreLoadtag
     FIELD CountOfLabels AS INTEGER
     .    
     
DEFINE {1} TEMP-TABLE ttInventoryStockLoadtagWIP
    LIKE ttInventoryStockLoadtag
    FIELD NextMachineID AS CHARACTER 
    FIELD NextMachineName AS CHARACTER
    FIELD OrderCustomerID AS CHARACTER
    FIELD OrderCustomerName AS CHARACTER
    FIELD LastMachineName AS CHARACTER 
    FIELD RMItemName AS CHARACTER 
    .
    
DEFINE {1} TEMP-TABLE InventoryStockAlias
    FIELD InventoryStockAliasID AS INTEGER
    FIELD Company AS CHARACTER  
    FIELD InventoryStockID AS CHARACTER
    FIELD UniquePrefix AS CHARACTER 
    FIELD StockIDAlias AS CHARACTER
    .
    
DEFINE {1} TEMP-TABLE InventoryTransaction 
    FIELD rec_key AS CHARACTER
    FIELD InventoryTransactionID AS INTEGER
    FIELD Company AS CHARACTER 
    FIELD InventoryStockID AS CHARACTER
    FIELD StockIDAlias AS CHARACTER
    FIELD TransactionType AS CHARACTER
    FIELD QuantityChange AS DECIMAL 
    FIELD QuantityUOM AS CHARACTER 
    FIELD WarehouseID AS CHARACTER 
    FIELD LocationID AS CHARACTER 
    FIELD CreatedTime AS DATETIME
    FIELD CreatedBy AS CHARACTER
    FIELD ScannedTime AS DATETIME
    FIELD ScannedBy AS CHARACTER
    FIELD PostedTime AS DATETIME
    FIELD PostedBy AS CHARACTER 
    FIELD TransactionTime AS DATETIME
    FIELD TransactionStatus AS CHARACTER
    .
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
