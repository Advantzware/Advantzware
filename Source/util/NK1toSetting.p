/* NK1toSetting.p - rstark - 11.4.2021 */

DEFINE VARIABLE companyContext      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iScopeIDCompany     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSettingTypeID      AS INTEGER   NO-UNDO.
DEFINE VARIABLE nk1DataType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE nk1Name             AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDataType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDefaultValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingDescription  AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingValidValues  AS CHARACTER NO-UNDO.

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
            .
    END. // not can-find
    IF companyContext THEN
    FOR EACH sys-ctrl NO-LOCK
        WHERE sys-ctrl.name EQ nk1Name
        :
        FIND FIRST scope NO-LOCK
             WHERE scope.scopeTable  EQ "Company"
               AND scope.scopeField1 EQ sys-ctrl.company
             NO-ERROR.
        IF NOT AVAILABLE scope THEN NEXT.
        iScopeIDCompany = scope.scopeID.
        FIND FIRST setting EXCLUSIVE-LOCK
             WHERE setting.settingTypeID EQ settingType.settingTypeID
               AND setting.settingName   EQ settingType.settingName
               AND setting.scopeID       EQ iScopeIDCompany
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
                setting.scopeID       = iScopeIDCompany
                .
        END. // not avail
        CASE nk1DataType:
            WHEN "Character" THEN
            setting.settingValue = sys-ctrl.char-fld.
            WHEN "Date" THEN
            setting.settingValue = STRING(sys-ctrl.date-fld,"99/99/9999").
            WHEN "Decimal" THEN
            setting.settingValue = STRING(sys-ctrl.dec-fld).
            WHEN "Integer" THEN
            setting.settingValue = STRING(sys-ctrl.int-fld).
            WHEN "Logical" THEN
            setting.settingValue = STRING(sys-ctrl.log-fld,"YES/NO").
            WHEN "Description" THEN
            setting.settingValue = sys-ctrl.descrip.
        END CASE.
        // convert when integer and/or decimal fields used as logicals
        IF nk1DataType NE settingDataType AND settingDataType EQ "Logical" THEN
        setting.settingValue = STRING(setting.settingValue EQ "1","YES/NO").
    END. // each sys-ctrl
END. // repeat
INPUT CLOSE.
