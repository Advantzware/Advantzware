
/*------------------------------------------------------------------------
    File        : EstimateProcs.i
    Purpose     : 

    Syntax      :

    Description : Definition file for EstimateProcs (estimate calculation engine)

    Author(s)   : BV
    Created     : Tue Oct 23 11:28:10 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttCalculationErrors
    FIELD cEstNo AS CHARACTER 
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER 
    FIELD iPassNo AS INTEGER
    FIELD cError AS CHARACTER 
    FIELD cMessage AS CHARACTER 
    FIELD lCritical AS LOGICAL
    .
    
DEFINE {1} TEMP-TABLE ttCostTable NO-UNDO
    FIELD dRunQty AS DECIMAL EXTENT 20
    FIELD dRunCost AS DECIMAL EXTENT 20
    FIELD cRunCostUom AS CHARACTER 
    FIELD dSetups AS DECIMAL EXTENT 20
    .



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
