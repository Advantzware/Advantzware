
/*------------------------------------------------------------------------
    File        : ttInputEst.i
    Purpose     : Include file that houses ttInputEstimate

    Syntax      :

    Description : Include file for declaration of shared temp tables for 
                  estimate importing

    Author(s)   : BV
    Created     : Wed Jun 14 22:08:46 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} SHARED TEMP-TABLE ttInputEst
    FIELD riParentEst AS ROWID   /*Can be the row-id of the est table or blank for new estimate*/
    FIELD iEstNo AS INTEGER /*Defaults to auto-incremented*/
    FIELD iFormNo AS INTEGER /*Defaults to 1*/
    FIELD iBlankNo AS INTEGER /*Defaults to 1*/
    FIELD cCompany AS CHARACTER /*Required*/
    FIELD cCustomer AS CHARACTER /*Defaults to Customer X if empty*/
    FIELD cShipTo AS CHARACTER /*Defaults to cCustomer if not provided*/
    FIELD cPartID AS CHARACTER /*Required*/
    FIELD cPartName AS CHARACTER /*Optional*/
    FIELD cPartDescription AS CHARACTER  /*Optional*/
    FIELD cStyle AS CHARACTER /*Required and validated*/
    FIELD cFlute AS CHARACTER /*Requied and validated*/
    FIELD cTest AS CHARACTER /*Required and validated*/
    FIELD cBoard AS CHARACTER /*Can be derived from CFlute and cTest*/
    FIELD cTab AS CHARACTER /*Defaults to In*/
    FIELD cCategory AS CHARACTER /*Required and validated*/
    FIELD dLength AS DECIMAL /*Required*/
    FIELD dWidth AS DECIMAL /*Required*/
    FIELD dDepth AS DECIMAL /*Optional*/
    FIELD dLengthBlank AS DECIMAL /*Optional - If provided will override the calculated blank*/
    FIELD dWidthBlank AS DECIMAL /*Optional - If provided will override the calculated blank*/
    FIELD lPurchased AS LOGICAL /*Defaults to No*/
    FIELD cInkDescription AS CHARACTER /*Optional*/
    FIELD iCountColors AS INTEGER /*Optional - If not provided, derived from provided Ink Codes or 0*/
    FIELD cInkCode LIKE eb.i-code /*Optional - Array of Inks - Validated RM Codes - Defaults to Style Inks or Control Inks if Count is non-zero*/
    FIELD iInkCoverage LIKE eb.i-% /*Optional - Array of Coverages - Defaults to Style % or Control % if not provided for corresponding code*/
    FIELD cCadID AS CHARACTER /*Optional*/
    FIELD cDieID AS CHARACTER /*Optional*/
    FIELD cSalesManID AS CHARACTER /*Defaults to Customer SalesmanID*/
    FIELD iQuantity AS INTEGER /*Required*/
    FIELD iQuantityYield AS INTEGER /*Defaults to iQuantity*/
    FIELD dDieInches AS DECIMAL /*Optional - defaults to 0*/
    FIELD cDesigner AS CHARACTER /*Optional*/    
    FIELD dWidthDie AS DECIMAL  /*Defaults to WidthBlank*/
    FIELD dLengthDie AS DECIMAL /*Defaults to LengthBlank*/
    FIELD cBndlCode AS CHARACTER /*Required*/
    FIELD iUnitCount AS INTEGER /*Required*/
    FIELD iPerPallet AS INTEGER /*Required*/
    FIELD cPallet AS CHARACTER /*Required*/
    FIELD cStockNo AS CHARACTER /*Optional*/
    FIELD cEstType AS CHARACTER /* import form or misc estimate */
    FIELD copy-qty as int extent 20 /* Optional*/ 
    FIELD copy-rel as int extent 20  /* Optional */
    FIELD dWeightPerM as DECIMAL  /* Optional */
    FIELD iStackHeight as INTEGER  /* Optional */
    FIELD iStackCode as CHARACTER  /* Optional */
    FIELD cFreightChargeMethod AS CHARACTER /*Optional - default "P"*/
    FIELD iPartial AS INTEGER /* Optional*/
    FIELD copy-runship as CHARACTER extent 20  /* Optional */
    FIELD cSourceEst AS CHARACTER /* Optional*/
    FIELD dQtyPerSet AS DECIMAL /* Optional*/
    FIELD cSetType AS CHARACTER /* Optional*/  
    FIELD dMSF AS DECIMAL /* Optional*/  
    FIELD dForceFrt AS DECIMAL /* Optional*/
    FIELD cForceFrtUom AS CHARACTER /* Optional*/ 
    FIELD cAddersDscr1 AS CHARACTER /* Optional*/ 
    FIELD cAddersDscr2 AS CHARACTER /* Optional*/
    FIELD cAddersDscr3 AS CHARACTER /* Optional*/
    FIELD cProject AS CHARACTER /* Optional*/
    FIELD iMolds AS INTEGER /* Optional*/
    FIELD dSqFt AS DECIMAL /* Optional*/
    FIELD cFgEstNo AS CHARACTER /* Optional*/
    FIELD lKeyItem AS LOGICAL /* Optional*/
    FIELD quantityToOrderSuggested AS DECIMAL /* Optional*/
    FIELD dCaliper AS DECIMAL /* Optional*/
    FIELD iPalletRestore AS INTEGER /* Optional*/
    FIELD iQtyRestore AS INTEGER /* Optional*/
    .




/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
