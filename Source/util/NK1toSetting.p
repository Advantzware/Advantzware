/* NK1toSetting.p - rstark - 11.4.2021 */

DEFINE VARIABLE companyContext      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iScopeID            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingTypeID      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSettingValue       AS CHARACTER NO-UNDO.
DEFINE VARIABLE nk1DataType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE nk1Name             AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingCategoryTags AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDataType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDefaultValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDescription  AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingValidValues  AS CHARACTER NO-UNDO.

IF USERID("ASI") EQ "NoSweat" THEN
INPUT FROM VALUE(SEARCH("Documentation\NK1 to Setting Type Conversion.csv")) NO-ECHO.
ELSE
INPUT FROM VALUE(SEARCH("N:\Documentation\NK1 to Setting Type Conversion.csv")) NO-ECHO.
IMPORT ^. // header line
REPEAT:
    IMPORT DELIMITER ","
        nk1Name
        nk1DataType
        settingName
        settingDataType
        settingDescription
        settingValidValues
        settingDefaultValue
        companyContext
        settingCategoryTags
        .
    FIND FIRST settingType NO-LOCK
         WHERE settingType.settingName EQ settingName
         NO-ERROR.
    IF NOT AVAILABLE settingType THEN DO:
        DO WHILE TRUE:
            iSettingTypeID = NEXT-VALUE(settingTypeID_seq).
            IF NOT CAN-FIND(FIRST settingType
                            WHERE settingType.settingTypeID EQ iSettingTypeID) THEN
            LEAVE.
        END. // do while
        CREATE settingType.
        ASSIGN
            settingType.settingTypeID = iSettingTypeID
            settingType.settingName   = settingName
            settingType.description   = settingDescription
            settingType.dataType      = settingDataType
            settingType.validValues   = settingValidValues
            settingType.defaultValue  = settingDefaultValue
            settingType.categoryTags  = settingCategoryTags
            .
    END. // not can-find
    IF companyContext THEN
    FOR EACH sys-ctrl NO-LOCK
        WHERE sys-ctrl.name EQ nk1Name
        :
        CASE nk1DataType:
            WHEN "Character" THEN
            cSettingValue = sys-ctrl.char-fld.
            WHEN "Date" THEN
            cSettingValue = STRING(sys-ctrl.date-fld,"99/99/9999").
            WHEN "Decimal" THEN
            cSettingValue = STRING(sys-ctrl.dec-fld).
            WHEN "Integer" THEN
            cSettingValue = STRING(sys-ctrl.int-fld).
            WHEN "Logical" THEN
            cSettingValue = STRING(sys-ctrl.log-fld,"YES/NO").
            WHEN "Description" THEN
            cSettingValue = sys-ctrl.descrip.
        END CASE.
        // convert when integer and/or decimal fields used as logicals
        IF nk1DataType NE settingDataType AND settingDataType EQ "Logical" THEN
        cSettingValue = STRING(cSettingValue EQ "1","YES/NO").
        // if value same as default, don't create unique setting
        IF cSettingValue EQ settingType.defaultValue THEN NEXT.
        FIND FIRST scope NO-LOCK
             WHERE scope.scopeTable  EQ "Company"
               AND scope.scopeField1 EQ sys-ctrl.company
             NO-ERROR.
        IF NOT AVAILABLE scope THEN DO:
            DO WHILE TRUE:
                iScopeID = NEXT-VALUE(scopeID_seq).
                IF NOT CAN-FIND(FIRST scope
                                WHERE scope.scopeID EQ iScopeID) THEN
                LEAVE.
            END. // do while
            CREATE scope.
            ASSIGN
                scope.scopeID     = iScopeID
                scope.scopeTable  = "Company"
                scope.scopeField1 = sys-ctrl.company
                .
        END. // if not avail
        iScopeID = scope.scopeID.
        FIND FIRST setting EXCLUSIVE-LOCK
             WHERE setting.settingTypeID EQ settingType.settingTypeID
               AND setting.settingName   EQ settingType.settingName
               AND setting.scopeID       EQ iScopeID
             NO-ERROR.
        IF NOT AVAILABLE setting THEN DO:
            DO WHILE TRUE:
                iSettingID = NEXT-VALUE(settingID_seq).
                IF NOT CAN-FIND(FIRST setting
                                WHERE setting.settingID EQ iSettingID) THEN
                LEAVE.
            END. // do while
            CREATE setting.
            ASSIGN
                setting.settingTypeID = settingType.settingTypeID
                setting.settingID     = iSettingID
                setting.settingName   = settingType.settingName
                setting.description   = settingType.description
                setting.scopeID       = iScopeID
                .
        END. // not avail
        setting.settingValue = cSettingValue.
    END. // each sys-ctrl
END. // repeat
INPUT CLOSE.
