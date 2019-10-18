
/*------------------------------------------------------------------------
    File        : Validate.i
    Purpose     : Reduces code repetition for basic validation query

    Syntax      :

    Description : Controls the main query and return value for various types of validations.
Requires &ValidationTable  &ValidationField &ValidationMessage

    Author(s)   : Sewa Singh
    Created     : Tue Oct 15 18:46:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcInputValue AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iplRequire AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplIsValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
&IF DEFINED(ValidateTable) = 0 &THEN 
&GLOBAL-DEFINE ValidateTable po-ord
&GLOBAL-DEFINE ValidateField po-no
&GLOBAL-DEFINE ValidateMessage "Po Number"
&ENDIF

/* ***************************  Main Block  *************************** */

ASSIGN 
    cMessage = "{&ValidateMessage}"
    oplIsValid = YES
    .
    
IF iplRequire AND ipcInputValue EQ 0 THEN 
    ASSIGN 
        oplIsValid = NO
        opcMessage = cMessage + " is required."
        .
IF oplIsValid THEN DO:
    FIND FIRST {&ValidateTable} NO-LOCK 
        WHERE {&ValidateTable}.company EQ ipcCompany
            AND  {&ValidateTable}.{&ValidateField} EQ ipcInputValue
        NO-ERROR.
    IF NOT AVAILABLE {&ValidateTable} THEN 
        ASSIGN 
            oplIsValid = NO 
            opcMessage = cMessage + " is not valid."
            .
END.