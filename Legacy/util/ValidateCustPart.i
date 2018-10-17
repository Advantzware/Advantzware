
/*------------------------------------------------------------------------
    File        : Validate.i
    Purpose     : Reduces code repetition for basic validation query

    Syntax      :

    Description : Controls the main query and return value for various types of validations.
Requires &ValidationTable  &ValidationField &ValidationMessage

    Author(s)   : BV
    Created     : Tue Mar 13 18:46:22 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcInputValuePart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcInputValueCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplRequire AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplIsValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
&IF DEFINED(ValidateTable) = 0 &THEN 
&GLOBAL-DEFINE ValidateTable itemfg
&GLOBAL-DEFINE ValidateField1 part-no
&GLOBAL-DEFINE ValidateField2 cust-no
&GLOBAL-DEFINE ValidateMessage "Cust Part #"
&ENDIF

/* ***************************  Main Block  *************************** */

ASSIGN 
    cMessage = "{&ValidateMessage}"
    oplIsValid = YES
    .
   
IF iplRequire AND ipcInputValuePart EQ "" THEN 
    ASSIGN 
        oplIsValid = NO
        opcMessage = cMessage + " is required."
        .
IF oplIsValid THEN DO:
    FIND FIRST {&ValidateTable} NO-LOCK 
        WHERE {&ValidateTable}.company EQ ipcCompany
            AND  {&ValidateTable}.{&ValidateField1} EQ ipcInputValuePart
            AND {&ValidateTable}.{&ValidateField1} NE ""
            AND  ({&ValidateTable}.{&ValidateField2} EQ ipcInputValueCust OR {&ValidateTable}.i-code  EQ "S")
        NO-ERROR.
    IF NOT AVAILABLE {&ValidateTable} THEN 
        ASSIGN 
            oplIsValid = NO 
            opcMessage = cMessage + " is not valid."
            .
END.
