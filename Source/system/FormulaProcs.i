
/*------------------------------------------------------------------------
    File        : FormulaProcs.i
    Purpose     : For defining temp-tables across procedure interface

    Syntax      :

    Description : Temp-tables for FormulaProcs

    Author(s)   : BV
    Created     : Sun Dec 22 12:23:40 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPanel
    FIELD cPanelType            AS CHARACTER
    FIELD iPanelNum             AS INTEGER 
    FIELD cPanelFormula         AS CHARACTER 
    FIELD dScoringAllowance     AS DECIMAL
    FIELD cScoreType            AS CHARACTER
    FIELD dPanelSize            AS DECIMAL
    FIELD dPanelSizeFromFormula AS DECIMAL
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
