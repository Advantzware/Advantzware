
/*------------------------------------------------------------------------
    File        : EstimateCostTotalReset.i
    Purpose     : 

    Syntax      :

    Description : Include file for reset of common summary fields across estCostHeader, estCostForm and estCostItem

    Author(s)   : BV
    Created     : Thu Aug 23 14:34:21 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEFINE PARAMETER BUFFER ipbf-{&TableName}     FOR {&TableName}.

/* ********************  Preprocessor Definitions  ******************** */

    
    FIND CURRENT ipbf-{&TableName} EXCLUSIVE-LOCK.
    ASSIGN 
        ipbf-{&TableName}.costTotalBoard = 0
        ipbf-{&TableName}.costTotalLabor = 0
        ipbf-{&TableName}.costTotalMaterial = 0
        ipbf-{&TableName}.costTotalFactory = 0 
        ipbf-{&TableName}.costTotalNonFactory = 0 
        ipbf-{&TableName}.netProfit = 0
        .
    FIND CURRENT ipbf-{&TableName} NO-LOCK.

/* ***************************  Main Block  *************************** */
