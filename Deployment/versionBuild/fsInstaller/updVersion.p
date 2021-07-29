DEF INPUT PARAMETER cObjCodeLocn AS CHAR NO-UNDO.
DEF VAR cString AS CHAR NO-UNDO.
DEF VAR cTextVersion AS CHAR NO-UNDO.
DEF VAR dVersion AS DEC NO-UNDO.
DEF VAR cTextDate AS CHAR NO-UNDO.

IF SEARCH(cObjCodeLocn + "pt/version.txt") <> ? THEN DO:
    INPUT FROM VALUE(cObjCodeLocn + "pt/version.txt").
    IMPORT UNFORMATTED cString.
    
    ASSIGN
        cTextVersion = IF SUBSTRING(cString,10,1) <> " " THEN 
                        SUBSTRING(cString,6,7) ELSE
                        "10.3000"
        dVersion = DECIMAL(cTextVersion)
        cTextDate = SUBSTRING(cString,20,8).
        
    FIND LAST z_version
        NO-ERROR.
    
    IF AVAIL z_version THEN DO:
        IF z_version.version < dVersion THEN DO:
            CREATE z_version.
            ASSIGN
                z_version.version = dVersion
                z_version.asofdate = cTextDate
                z_version.update-date = TODAY.
        END.
        ELSE IF z_version.version = dVersion THEN ASSIGN
                z_version.asofdate = cTextDate
                z_version.update-date = TODAY.
        ELSE MESSAGE "There is a discrepancy in your version" SKIP
                     "information.  Please contact Foresight" SKIP
                     "Support for assistance."
                     VIEW-AS ALERT-BOX ERROR.            
    END.
    ELSE MESSAGE "No version record available. Please" SKIP
                 "contact Foresight Support for help."
                 VIEW-AS ALERT-BOX ERROR.
END.
