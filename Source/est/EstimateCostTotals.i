
/*------------------------------------------------------------------------
    File        : EstimateCostTotals.i
    Purpose     : 

    Syntax      :

    Description : Include file for handling of common summary fields across estCostHeader, estCostForm and estCostItem

    Author(s)   : BV
    Created     : Thu Jul 25 14:34:21 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEFINE PARAMETER BUFFER ipbf-{&TableName}     FOR {&TableName}.
    DEFINE PARAMETER BUFFER ipbf-estCostCategory FOR estCostCategory.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

    
    FIND CURRENT ipbf-{&TableName} EXCLUSIVE-LOCK.
    IF ipbf-estCostCategory.includeInBoardCost THEN 
        ipbf-{&TableName}.costTotalBoard = ipbf-{&TableName}.costTotalBoard + ipdCost.
    IF ipbf-estCostCategory.includeInLaborCost THEN 
        ipbf-{&TableName}.costTotalLabor = ipbf-{&TableName}.costTotalLabor + ipdCost.
    IF ipbf-estCostCategory.includeInVariableOverheadCost THEN 
        ipbf-{&TableName}.costTotalVariableOverhead = ipbf-{&TableName}.costTotalVariableOverhead + ipdCost.
    IF ipbf-estCostCategory.includeInFixedOverheadCost THEN 
        ipbf-{&TableName}.costTotalFixedOverhead = ipbf-{&TableName}.costTotalFixedOverhead + ipdCost.
    IF ipbf-estCostCategory.includeInMaterialCost THEN        
        ipbf-{&TableName}.costTotalMaterial = ipbf-{&TableName}.costTotalMaterial + ipdCost.
    IF ipbf-estCostCategory.includeInFactoryCost THEN 
        ipbf-{&TableName}.costTotalFactory = ipbf-{&TableName}.costTotalFactory + ipdCost.
    IF ipbf-estCostCategory.includeInNonFactoryCost THEN 
        ipbf-{&TableName}.costTotalNonFactory = ipbf-{&TableName}.costTotalNonFactory + ipdCost.
    IF ipbf-estCostCategory.includeInNetProfit THEN 
        ipbf-{&TableName}.netProfit = ipbf-{&TableName}.netProfit + ipdCost.
    ASSIGN 
        ipbf-{&TableName}.costTotalFull = ipbf-{&TableName}.costTotalFactory + ipbf-{&TableName}.costTotalNonFactory                  
        ipbf-{&TableName}.sellPrice = ipbf-{&TableName}.netProfit + ipbf-{&TableName}.costTotalFull
        ipbf-{&TableName}.profitPctGross = (1 - (ipbf-{&TableName}.costTotalFactory / ipbf-{&TableName}.sellPrice)) * 100
        ipbf-{&TableName}.profitPctNet = (1 - (ipbf-{&TableName}.costTotalFull / ipbf-{&TableName}.sellPrice)) * 100
        .
    FIND CURRENT ipbf-{&TableName} NO-LOCK.

/* ***************************  Main Block  *************************** */
