DEFINE INPUT  PARAMETER  ipcDatabase  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opiUserCount AS INTEGER INITIAL 0 NO-UNDO.
DEFINE OUTPUT PARAMETER  opiUniqueUsers AS INTEGER INITIAL 0 NO-UNDO.
DEFINE TEMP-TABLE ttUsers
FIELD usrName AS CHARACTER 
INDEX i1 usrName.

opiUserCount = 0.
CASE ipcDatabase:
    WHEN "ASI" THEN 
        DO:
            FOR EACH ASI._connect NO-LOCK WHERE ASI._connect._connect-type = "SELF" OR ASI._connect._connect-type= "REMC"  :
                DISPLAY asi._connect._connect-name.
                FIND FIRST ttUsers WHERE ttUsers.usrName EQ asi._connect._connect-name
                  NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttUsers THEN DO:
                    CREATE ttUsers.
                    ASSIGN ttUsers.usrName = asi._connect._connect-name.
                END.
                opiUserCount = opiUserCount + 1.
            END.
        END.
    WHEN "ASI" THEN 
        DO:
            FOR EACH ASI._Connect NO-LOCK WHERE ASI._Connect._connect-type = "SELF" OR ASI._Connect._connect-type= "REMC"  :
                opiUserCount = opiUserCount + 1.
                FIND FIRST ttUsers WHERE ttUsers.usrName EQ ASI._Connect._connect-name
                  NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttUsers THEN DO:
                    CREATE ttUsers.
                    ASSIGN ttUsers.usrName = ASI._Connect._connect-name.
                END.
            END.
        END.
END CASE.
opiUniqueUsers = 0.
FOR EACH ttUsers:
    opiUniqueUsers = opiUniqueUsers + 1.
END.
