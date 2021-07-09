
/*------------------------------------------------------------------------
    File        : ValidateEstFormBlank.i
    Purpose     : Reduces code repetition for basic validation query.  3 Criteria.

    Syntax      :

    Description : Controls the main query and return value for various types of validations.
Requires &ValidationTable  &ValidationField &ValidationMessage

    Author(s)   : 
    Created     : Thu May 06 18:46:22 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcInputValue1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcInputValue2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcInputValue3 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iplRequireValue1 AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplIsValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
&IF DEFINED(ValidateTable) = 0 &THEN 
&GLOBAL-DEFINE ValidateTable shipto
&GLOBAL-DEFINE ValidateField1 shipto
&GLOBAL-DEFINE ValidateField2 0
&GLOBAL-DEFINE ValidateField3 0
&GLOBAL-DEFINE ValidateMessage "ShipToID"
&ENDIF

/* ***************************  Main Block  *************************** */

ASSIGN 
    cMessage   = "{&ValidateMessage}"
    oplIsValid = YES
    .
    
IF iplRequireValue1 AND ipcInputValue1 EQ "" THEN 
    ASSIGN 
        oplIsValid = NO
        opcMessage = cMessage + " is required."
        .
IF oplIsValid THEN 
DO:
    FIND FIRST {&ValidateTable} NO-LOCK 
        WHERE {&ValidateTable}.company EQ ipcCompany
        AND {&ValidateTable}.{&ValidateField1} EQ FILL(" ",8 - LENGTH(TRIM(ipcInputValue1))) + TRIM(ipcInputValue1) 
        AND {&ValidateTable}.{&ValidateField2} EQ ipcInputValue2
        AND {&ValidateTable}.{&ValidateField3} EQ ipcInputValue3    
        NO-ERROR.
    IF NOT AVAILABLE {&ValidateTable} THEN 
        ASSIGN 
            oplIsValid = NO 
            opcMessage = cMessage + " is not valid."
            .    
END.