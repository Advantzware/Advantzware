
/*------------------------------------------------------------------------
    File        : checkExpiredLicense.p
    Purpose     : 

    Syntax      :

    Description : Check for and warn of an expired license.

    Author(s)   : 
    Created     : Tue Jun 21 20:31:59 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE lModulesExpired AS LOGICAL NO-UNDO.
DEFINE VARIABLE iExpiredDays    AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
lModulesExpired = FALSE.
FOR EACH module NO-LOCK:
        
    IF module.expire-date LE TODAY + 7 THEN
        ASSIGN lModulesExpired = TRUE
            iExpiredDays    = module.expire-date - TODAY.                             
               
END.
    
IF lModulesExpired EQ TRUE THEN 
DO:
    
    MESSAGE "Warning: One or more of your Advantzware licenses will expire in " iExpiredDays " days." SKIP 
            "Please contact Advantzware at 215-369-7800 to obtain a new license key."
        VIEW-AS ALERT-BOX TITLE "License Expiring".                 
        
END.
