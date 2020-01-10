/*------------------------------------------------------------------------
    File        : checkExpiredLicense.p
    Description : Check for and warn of an expired license.
    Author(s)   : 
    Created     : Tue Jun 21 20:31:59 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE lModulesExpired AS LOGICAL NO-UNDO.
DEFINE VARIABLE iExpiredDays    AS INTEGER NO-UNDO INITIAL 360.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
lModulesExpired = FALSE.
FOR EACH module WHERE module.expire-date LE TODAY + 7:
    ASSIGN 
        lModulesExpired = TRUE
        iExpiredDays    = IF module.expire-date - TODAY LT iExpiredDays THEN module.expire-date - TODAY ELSE iExpiredDays.
END.
    
IF lModulesExpired EQ TRUE THEN MESSAGE 
    "Warning: One or more of your Advantzware licenses will expire in " iExpiredDays " days." SKIP 
    "Please contact Advantzware at 215-369-7800 to obtain a new license key."
    VIEW-AS ALERT-BOX TITLE "License Expiring".                 
