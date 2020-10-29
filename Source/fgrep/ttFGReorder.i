
/*------------------------------------------------------------------------
    File        : ttFGReorder.i
    Purpose     : Replace IR1 and extend for multi select

    Syntax      :

    Description : Reorder Report Temp-table		

    Author(s)   : BV
    Created     : Sun Aug 30 20:27:02 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGReorder NO-UNDO 
    FIELD company AS CHARACTER 
    FIELD itemID AS CHARACTER
    FIELD itemName AS CHARACTER
    FIELD itemDesc1 AS CHARACTER 
    FIELD itemDesc2 AS CHARACTER 
    FIELD itemDesc3 AS CHARACTER
    FIELD productCategoryID AS CHARACTER 
    FIELD productCategoryDesc AS CHARACTER 
    FIELD quantityOnHand AS DECIMAL 
    FIELD quantityOnOrder AS DECIMAL
    FIELD quantityAllocated AS DECIMAL 
    FIELD quantityAvailable AS DECIMAL 
    FIELD quantityReorderLevel AS DECIMAL 
    FIELD quantityMinOrder AS DECIMAL 
    FIELD quantityMaxOrder AS DECIMAL
    FIELD quantityToOrder AS DECIMAL 
    FIELD quantityToOrderSuggested AS DECIMAL 
    FIELD dateDueDateEarliest AS DATE
    FIELD blankLength AS DECIMAL 
    FIELD blankWidth AS DECIMAL
    FIELD blankDimUom AS CHARACTER 
    FIELD blankArea AS DECIMAL  
    FIELD blankAreaUOM AS CHARACTER
    FIELD isMale AS LOGICAL 
    FIELD compatibilityGroup AS INTEGER
    FIELD multiplier AS INTEGER
    FIELD isSelected AS LOGICAL
    .  
    
DEFINE TEMP-TABLE ttFGReorderSelection NO-UNDO 
    LIKE ttFGReorder
    FIELD quantityCyclesRequired AS DECIMAL
    FIELD quantityCyclesSurplus AS DECIMAL
    FIELD quantityToOrderSurplus AS DECIMAL
    . 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
