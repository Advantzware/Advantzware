
/*------------------------------------------------------------------------
    File        : GetCERouteFromStyle.p
    Purpose     : 

    Syntax      :

    Description : Given a company and Style, it will lookup the config settings to return a machine code to use in Layout Tab

    Author(s)   : BV
    Created     : Thu Dec 06 13:15:44 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcMachine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-style FOR style.

/* ***************************  Main Block  *************************** */

RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CERouteFromStyle",
                      INPUT "L",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cReturn,
                      OUTPUT lFound).
IF lFound AND cReturn EQ "Yes" THEN DO:
    
    FIND FIRST bf-style NO-LOCK 
        WHERE bf-style.company EQ ipcCompany
        AND bf-style.style EQ ipcStyle
        NO-ERROR.
    IF AVAILABLE bf-style THEN 
        opcMachine = bf-style.m-code[1].
END. 
    

/* ********************  Preprocessor Definitions  ******************** */

