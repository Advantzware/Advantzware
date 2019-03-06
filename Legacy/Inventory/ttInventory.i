
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
    
DEFINE {1} TEMP-TABLE ttInventoryStock
    FIELD rec_key AS CHARACTER
    FIELD InventoryStockID AS INTEGER
    FIELD Company AS CHARACTER
    FIELD RMItemID AS CHARACTER
    FIELD FGItemID AS CHARACTER
    FIELD WIPItemID AS CHARACTER
    FIELD ItemType AS CHARACTER
    FIELD JobNo AS CHARACTER
    FIELD JobNo2 AS INTEGER
    FIELD FormNo AS INTEGER
    FIELD BlankNo AS INTEGER
    FIELD PassNo AS INTEGER
    FIELD PoNo AS INTEGER
    FIELD PoLine AS INTEGER
    FIELD WarehouseID AS CHARACTER
    FIELD LocationID AS CHARACTER
    FIELD Quantity AS DECIMAL
    FIELD QuantityOriginal AS DECIMAL
    FIELD QuantityPerSubUnit AS DECIMAL
    FIELD QuantitySubUnitsPerUnit AS INTEGER
    FIELD QuantityOfSubUnits AS INTEGER
    FIELD QuantityOfUnits AS INTEGER
    FIELD QuantityPartial AS DECIMAL 
    FIELD QuantityUOM AS CHARACTER
    FIELD CostStandardPerUOM AS DECIMAL
    FIELD CostActualPerUOM AS DECIMAL
    FIELD CostUOM AS CHARACTER
    FIELD TimeCreated AS DATETIME
    FIELD CreatedBy AS CHARACTER
    FIELD TimeConsumed AS DATETIME
    FIELD BOL AS CHARACTER
    FIELD MachineID AS CHARACTER
    FIELD ConsumedByType AS CHARACTER
    FIELD LotID AS CHARACTER
    FIELD CustomerID AS CHARACTER
    FIELD OrderNo AS INTEGER
    FIELD OrderLine AS INTEGER
    FIELD ReleaseNo AS INTEGER
    FIELD ReleaseLine AS INTEGER
    FIELD InventoryStatus AS CHARACTER /*Loadtag, Posted, Allocated, etc.*/
    FIELD EachDimLen AS DECIMAL
    FIELD EachDimWid AS DECIMAL 
    FIELD EachDimDep AS DECIMAL
    FIELD EachDimUOM AS CHARACTER  
    FIELD BasisWeight AS DECIMAL 
    FIELD BasisWeightUOM AS CHARACTER 
    FIELD WeightTare AS DECIMAL 
    FIELD WeightNet AS DECIMAL 
    FIELD WeightUOM AS CHARACTER  
    . 
    
DEFINE {1} TEMP-TABLE ttInventoryStockLoadtag
     LIKE ttInventoryStock
     FIELD CountOfLabelsPerLoadtag AS INTEGER 
     
     .
 
DEFINE {1} TEMP-TABLE ttInventoryStockPreLoadtag
    LIKE ttInventoryStock
    FIELD QuantityTotal AS DECIMAL  
    FIELD QuantityTotalRunning AS DECIMAL 
    FIELD CountOfLoadtags AS INTEGER 
    FIELD CountOfLabelsPerLoadtag AS INTEGER
    FIELD LabelTemplate AS CHARACTER 
    FIELD LabelProgram AS CHARACTER 
    .   
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
