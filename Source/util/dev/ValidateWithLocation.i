
/*------------------------------------------------------------------------
    File        : ValidateWithLocation.i
    Purpose     : Reduces code repetition for basic validation query
                  Includes Location match as required criteria

    Syntax      :

    Description : Controls the main query and return value for various types of validations.
Requires &ValidationTable  &ValidationField &ValidationMessage

    Author(s)   : BV
    Created     : Tue Mar 13 18:46:22 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT PARAMETER ipcInputValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplRequire AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplIsValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

ASSIGN 
    cMessage = {&ValidateMessage}
    oplIsValid = YES
    .
    
IF iplRequire AND ipcInputValue EQ "" THEN 
    ASSIGN 
        oplIsValid = NO
        opcMessage = cMessage + " is a required."
        .
IF oplIsValid THEN DO:
    FIND FIRST {&ValidateTable} NO-LOCK 
        WHERE {&ValidateTable}.company EQ ipcCompany
        AND {&ValidateTable}.loc EQ ipcLocation
        AND {&ValidateTable}.{&ValidateField} EQ ipcInputValue
        NO-ERROR.
    IF NOT AVAILABLE {&ValidateTable} THEN 
        ASSIGN 
            oplIsValid = NO 
            opcMessage = cMessage + " is not valid."
            .
END.