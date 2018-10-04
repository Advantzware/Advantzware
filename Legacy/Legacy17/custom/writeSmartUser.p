/*------------------------------------------------------------------------
    File        : create-users.p
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH.

DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipcUserName AS CHARACTER NO-UNDO .

DEFINE VARIABLE oUsers AS UserDatasetModel NO-UNDO .

/* ***************************  Main Block  *************************** */
FIND FIRST SmartLanguage NO-LOCK WHERE SmartLanguage.LanguageIsoCode EQ "EN-US":U.

oUsers = UserDatasetModel:FromUsername (ipcUserID) .
IF NOT oUsers:SmartUser:Available THEN DO:
    oUsers:TrackingChanges = TRUE .
    oUsers:SmartUser:Create() .
    ASSIGN
        oUsers:SmartUser:UserName     = ipcUserID 
        oUsers:SmartUser:LanguageGuid = SmartLanguage.LanguageGuid
        .    
/*    oUsers:SaveChanges() .*/
END. /* not oUsers */
oUsers:TrackingChanges = TRUE .
ASSIGN 
    oUsers:SmartUser:UserFullName            = ipcUserName
    oUsers:SmartUser:UserPasswordChangedDate = NOW
    .
oUsers:SaveChanges() .

FIND FIRST SmartGroup NO-LOCK WHERE SmartGroup.GroupName EQ "User":U NO-ERROR .
IF AVAILABLE SmartGroup THEN 
RUN createSmartUserGroup (SmartGroup.GroupGuid) .

IF CAN-DO("ASI,Admin,NoSweat", ipcUserID) THEN DO:  
    FIND FIRST SmartGroup NO-LOCK WHERE SmartGroup.GroupName EQ "Admin":U NO-ERROR .
    IF AVAILABLE SmartGroup THEN 
    RUN createSmartUserGroup (SmartGroup.GroupGuid) .
END.

PROCEDURE createSmartUserGroup:
    DEFINE INPUT PARAMETER ipcGuid AS CHARACTER NO-UNDO .
    
    IF NOT CAN-FIND(FIRST SmartUserGroup
                    WHERE SmartUserGroup.UserGuid  EQ oUsers:SmartUser:UserGuid
                      AND SmartUserGroup.GroupGuid EQ ipcGuid) THEN DO:
        CREATE SmartUserGroup.
        ASSIGN
            SmartUserGroup.UserGuid  = oUsers:SmartUser:UserGuid
            SmartUserGroup.GroupGuid = ipcGuid
            .
    END. /* if not */
END PROCEDURE.    
