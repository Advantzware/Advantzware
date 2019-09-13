/*------------------------------------------------------------------------
    File        : util/chk-mod2.p
    Purpose     : Tests module license, displays error and returns error
    Syntax      : RUN util/chk-mod2.p (INPUT module.module, INPUT module.dscr, INPUT message)
    Description :  
    Author(s)   : UNK
    Created     : UNK
    Notes       : Modified 9/13/19 - MYT - source cleanup 
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-module-code AS CHARACTER NO-UNDO. /* button label*/
DEFINE INPUT PARAMETER ip-module-dscr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-message AS CHARACTER NO-UNDO.

ASSIGN 
    ip-module-code = REPLACE(ip-module-code,"&","").

FIND FIRST module NO-LOCK WHERE 
    module.module = ip-module-code 
    NO-ERROR.
IF NOT AVAIL module THEN FIND FIRST module NO-LOCK WHERE 
    module.dscr = ip-module-dscr
    NO-ERROR. 

IF AVAILABLE module 
AND NOT module.is-used
THEN DO:
    MESSAGE 
        "You are not licensed to run this module/feature." SKIP
        "Contact ASI Support for further assistance." SKIP 
        "Module: " + module.module + " (" + module.dscr + ")." SKIP 
        ip-message 
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
ELSE IF AVAILABLE module 
AND module.expire-date < TODAY THEN DO:
    MESSAGE 
        "Your license for this module/feature has expired." SKIP
        "Contact ASI Support for further assistance." SKIP 
        "Module: " + module.module + " (" + module.dscr + ")." SKIP 
        ip-message 
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
ELSE IF NOT AVAILABLE module THEN DO:
    MESSAGE 
        "System error: request for license check on unknown module." SKIP 
        "Please forward this error to ASI support if possible." SKIP 
        "Module: " + ip-module-code + " (" + ip-module-dscr + ")." SKIP 
        "Program: util/chk-mod2.p" SKIP 
         ip-message 
         VIEW-AS ALERT-BOX WARNING.
     RETURN.
 END.
 
        