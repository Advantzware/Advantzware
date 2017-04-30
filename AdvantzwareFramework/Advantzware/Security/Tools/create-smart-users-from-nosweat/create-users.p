/*------------------------------------------------------------------------
    File        : create-users.p
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH.

DEFINE VARIABLE oUsers    AS UserDatasetModel NO-UNDO .
DEFINE VARIABLE adminGuid AS CHARACTER        NO-UNDO .
DEFINE VARIABLE userGuid  AS CHARACTER        NO-UNDO .

/* ***************************  Main Block  *************************** */
FIND FIRST SmartLanguage NO-LOCK WHERE SmartLanguage.LanguageIsoCode EQ "EN-US":U.
FIND FIRST SmartGroup NO-LOCK WHERE SmartGroup.GroupName EQ "Admin":U.
adminGuid = SmartGroup.GroupGuid.
FIND FIRST SmartGroup NO-LOCK WHERE SmartGroup.GroupName EQ "User":U.
userGuid = SmartGroup.GroupGuid.

FOR EACH SmartUser:
    DELETE SmartUser.
END. /* each smartuser */

FOR EACH users NO-LOCK:    
    oUsers = UserDatasetModel:FromUsername (users.user_id) .
    IF NOT oUsers:SmartUser:Available THEN DO:
        oUsers:TrackingChanges = TRUE .    
        oUsers:SmartUser:Create() .
        ASSIGN
            oUsers:SmartUser:UserName                = users.user_id 
            oUsers:SmartUser:UserFullName            = users.user_name
            oUsers:SmartUser:LanguageGuid            = SmartLanguage.LanguageGuid
            oUsers:SmartUser:UserPasswordChangedDate = NOW
            .    
        oUsers:SaveChanges() .
    END. /* not oUsers */
    IF NOT CAN-FIND(FIRST SmartUserGroup
                    WHERE SmartUserGroup.UserGuid  EQ oUsers:SmartUser:UserGuid
                      AND SmartUserGroup.GroupGuid EQ userGuid) THEN DO:
        CREATE SmartUserGroup.
        ASSIGN
            SmartUserGroup.UserGuid  = oUsers:SmartUser:UserGuid
            SmartUserGroup.GroupGuid = userGuid
            .
    END. /* if not */
    IF CAN-DO("ASI,Admin,NoSweat", users.user_id) AND
       NOT CAN-FIND(FIRST SmartUserGroup
                    WHERE SmartUserGroup.UserGuid  EQ oUsers:SmartUser:UserGuid
                      AND SmartUserGroup.GroupGuid EQ adminGuid) THEN DO:
        CREATE SmartUserGroup.
        ASSIGN
            SmartUserGroup.UserGuid  = oUsers:SmartUser:UserGuid
            SmartUserGroup.GroupGuid = adminGuid
            .
    END. /* if not */
END. /* each users */
