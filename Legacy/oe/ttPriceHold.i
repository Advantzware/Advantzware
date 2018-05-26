
/*------------------------------------------------------------------------
    File        : ttPriceHold.i
    Purpose     : Declare as NEW within primary caller routine that implements the PriceProcs.p

    Syntax      :

    Description : Temp-table definition for ttPriceHold

    Author(s)   : BV
    Created     : Fri May 04 11:23:26 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttPriceHold
    FIELD riLine               AS ROWID 
    FIELD cFGItemID            AS CHARACTER
    FIELD cCustID              AS CHARACTER 
    FIELD cShipID              AS CHARACTER 
    FIELD dQuantity            AS DECIMAL 
    FIELD lMatrixMatch         AS LOGICAL 
    FIELD cMatrixMatch         AS CHARACTER
    FIELD lEffectiveDateTooOld AS LOGICAL 
    FIELD dtEffectiveDate      AS DATE 
    FIELD iQuantityLevel       AS INTEGER
    FIELD lQuantityOutOfRange  AS LOGICAL
    FIELD lQuantityMatch       AS LOGICAL  
    FIELD lPriceHold           AS LOGICAL 
    FIELD cPriceHoldReason     AS CHARACTER
    FIELD cPriceHoldDetail     AS CHARACTER 
    .     

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
