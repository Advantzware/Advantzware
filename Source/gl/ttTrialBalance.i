
/*------------------------------------------------------------------------
    File        : ttTrialBalance.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Mar 30 09:12:43 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTrialBalance NO-UNDO
    FIELD accountID         AS CHARACTER 
    FIELD accountName       AS CHARACTER 
    FIELD accountType       AS CHARACTER 
    FIELD accountGroup      AS CHARACTER 
    FIELD amountPTD         AS DECIMAL 
    FIELD amountYTD         AS DECIMAL
    FIELD amountYTDOpen     AS DECIMAL 
    FIELD amountYTDFYE      AS DECIMAL
    FIELD isBalanceSheet    AS LOGICAL 
    FIELD isIncomeStatement AS LOGICAL 
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
