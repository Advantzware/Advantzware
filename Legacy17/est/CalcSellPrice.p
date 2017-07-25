
/*------------------------------------------------------------------------
    File        : CalcSellPrice.p
    Purpose     : 

    Syntax      :

    Description : Calculates Sell Price Given a Margin and Commission.  Includes functionality for M calculation.

    Author(s)   : BV
    Created     : Sun Jul 23 17:28:38 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcMarginBasisFromCEControl AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcMarginBasis AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCommissionBasis AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdCostTotalFactory AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE INPUT PARAMETER ipdCostNonFactory AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE INPUT PARAMETER ipdPctCommission AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE INPUT PARAMETER ipdPctMargin AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE INPUT PARAMETER ipdAddToPrice AS DECIMAL DECIMALS 10 NO-UNDO.

DEFINE OUTPUT PARAMETER opdCalculatedPrice AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE OUTPUT PARAMETER opdCalculatedCommission AS DECIMAL DECIMALS 10 NO-UNDO.

DEFINE VARIABLE dEffectivePctMargin AS DECIMAL DECIMALS 10 NO-UNDO.
DEFINE VARIABLE cEffectiveMarginBasis AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/*Initialize the Margin Pct and Basis*/
dEffectivePctMargin = ipdPctMargin.
cEffectiveMarginBasis = ipcMarginBasis.

IF cEffectiveMarginBasis EQ "S" OR ipcMarginBasisFromCEControl EQ "B" THEN  /*REFACTOR Don't see how this gets set by user*/ 
    ASSIGN
        dEffectivePctMargin = (1 - (100 / (100 + dEffectivePctMargin))) * 100  
        cEffectiveMarginBasis = IF cEffectiveMarginBasis EQ "S" THEN "G" ELSE "N".
ELSE
    IF cEffectiveMarginBasis EQ "B" THEN cEffectiveMarginBasis = "N". /*This does not seem correct.  "B" should be treated as "G"?*/

CASE cEffectiveMarginBasis:
    WHEN "G" THEN  /*Gross Margin calc - Use Total Factory Cost only*/
        ASSIGN
            opdCalculatedPrice = ipdCostTotalFactory / (1 - (dEffectivePctMargin / 100)) + ipdAddToPrice
            opdCalculatedCommission = IF ipdPctCommission EQ 0 THEN 0
                ELSE ((opdCalculatedPrice - (IF ipcCommissionBasis EQ "G" THEN ipdCostTotalFactory ELSE 0)) * (ipdPctCommission / 100)).    
    WHEN "F" THEN  /* REFACTOR Do not see "F" as an option - do not understand this option*/
        ASSIGN
            opdCalculatedPrice = (ipdCostTotalFactory / (1 - (dEffectivePctMargin / 100)) + ipdCostNonFactory) + ipdAddToPrice
            opdCalculatedCommission = IF ipdPctCommission EQ 0 THEN 0
                ELSE ((opdCalculatedPrice - (IF ipcCommissionBasis EQ "G" THEN ipdCostTotalFactory ELSE 0)) * (ipdPctCommission / 100)).    

    WHEN "N" THEN /*Net Margin - calculate price based on margin/markup of which will include the Non Factory cost ex. commission*/
        DO:
            IF ipcCommissionBasis EQ "G" AND ipdPctCommission NE 0 THEN
                opdCalculatedPrice = (((1 / (ipdPctCommission / 100)) * (ipdCostTotalFactory + ipdCostNonFactory)) - ipdCostTotalFactory) /
                    (((1 / (ipdPctCommission / 100)) * (1 - (dEffectivePctMargin / 100))) - 1) + ipdAddToPrice.

            ELSE
                opdCalculatedPrice = (ipdCostTotalFactory + ipdCostNonFactory) / (1 - ((dEffectivePctMargin + ipdPctCommission) / 100)) + ipdAddToPrice.

            opdCalculatedCommission = opdCalculatedPrice - ipdCostTotalFactory - ipdCostNonFactory - (opdCalculatedPrice * (dEffectivePctMargin / 100)).
        END.

END CASE.
