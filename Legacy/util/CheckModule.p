
/*------------------------------------------------------------------------
    File        : CheckModule.p
    Purpose     : Should replace need for chk-mod.p

    Syntax      : RUN CheckModule.p (INPUT "ASI", INPUT "Module", INPUT YES /*prompt if no access*/, OUTPUT lAccess). 

    Description : Returns whether the caller has access to the passed in module.  
        Can prompt the user or not prompt.  Will always return a logical output parameter.	

    Author(s)   : BV
    Created     : Sun Jan 07 19:48:23 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipcDBName  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcModule  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplMessage AS LOGICAL   NO-UNDO.

DEFINE OUTPUT PARAMETER oplAccess  AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdmin        AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*Trim & symbol from module name since could be launched from main menu*/
IF INDEX(ipcModule,"&") GT 0 THEN
    ipcModule = REPLACE(ipcModule,"&","").

/*Initialize access*/
oplAccess = YES.

IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
IF VALID-HANDLE(hPgmMstrSecur) THEN
RUN epCanAccess IN hPgmMstrSecur (
    "util/CheckModule.p",
    "SuperAdmin",
    OUTPUT lAdmin 
    ).
IF lAdmin THEN RETURN.

FIND FIRST module NO-LOCK
     WHERE module.db-name EQ ipcDBName
       AND module.module  EQ ipcModule
     NO-ERROR.
IF NOT AVAILABLE module THEN 
    FIND FIRST module NO-LOCK 
        WHERE module.module EQ ipcModule
        NO-ERROR.
IF NOT AVAILABLE module THEN DO:
    FIND FIRST prgrms NO-LOCK 
        WHERE prgrms.prgmname EQ ipcModule
        NO-ERROR.
    IF AVAILABLE prgrms THEN 
        FIND FIRST module NO-LOCK 
            WHERE module.module EQ prgrms.prgm_ver  /*note this should be changed to use a new module linker field*/
            NO-ERROR.
END.
IF NOT AVAILABLE module THEN 
    ASSIGN 
        oplAccess = YES  /*Set to yes until we get Product List Organized*/
        cMessage = 'Module "' + ipcModule + '" does not exist'
        .
IF AVAILABLE module THEN  DO:
    IF oplAccess AND NOT module.is-used  THEN
        ASSIGN 
            oplAccess = NO
            cMessage = 'Module "' + ipcModule + '" is not activated.'
            .
    IF oplAccess AND module.expire-date LT TODAY THEN
        ASSIGN
            oplAccess = NO
            cMessage = 'Access to Module "' +  ipcModule + '" is expired.'
            .    
END.
IF NOT oplAccess AND iplMessage THEN 
    RUN pMessage (INPUT ipcModule, INPUT cMessage) . 


/* **********************  Internal Procedures  *********************** */

PROCEDURE pMessage:
/*------------------------------------------------------------------------------
 Purpose: Displays message for Module Access    
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcModule AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcMessage AS CHARACTER NO-UNDO.

 MESSAGE "System License Error for" "~"" + ipcModule + "~"!" SKIP(1)
    ipcMessage SKIP 
    "Contact Advantzware (215.369.7800) for assistance."
    VIEW-AS ALERT-BOX ERROR TITLE "System License".

END PROCEDURE.

    