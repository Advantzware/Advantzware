
/*------------------------------------------------------------------------
    File        : packCodeOverride.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jun 20 17:23:38 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustNo AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcStyle AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcPackCode AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
opcPackCode = "".    

IF ipcCustNo GT "" THEN 
    FIND FIRST cust NO-LOCK WHERE cust.company = ipcCompany 
        AND cust.cust-no = ipcCustNo
        NO-ERROR.
IF AVAIL cust AND cust.case-bundle <> "" THEN DO: 
    opcPackCode = cust.case-bundle.

END.
ELSE  
DO:
    IF ipcStyle GT "" THEN 
        FIND FIRST style NO-LOCK WHERE style.company EQ ipcCompany
            AND style.style EQ ipcStyle
            NO-ERROR.

    IF AVAILABLE style AND style.spare-char-5 GT "" THEN 
        opcPackCode = style.spare-char-5.

END.
