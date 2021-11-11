
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
    FIELD lAddAllowanceToSize   AS LOGICAL
    .
    
DEFINE TEMP-TABLE ttScoreLine NO-UNDO
    FIELD PanelType AS CHARACTER
    FIELD IsTotal   AS LOGICAL
    FIELD ScoreLine AS CHARACTER.
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
