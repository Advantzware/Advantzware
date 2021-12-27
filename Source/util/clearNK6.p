// clearNK6.p

DEFINE VARIABLE nk1Name     AS CHARACTER NO-UNDO.
DEFINE VARIABLE settingName AS CHARACTER NO-UNDO.

INPUT FROM VALUE(SEARCH("Documentation/NK1toSetting.csv")) NO-ECHO.
IMPORT ^. // header line
REPEAT:
    IMPORT DELIMITER ","
        nk1Name
        ^
        settingName
        .
    FOR EACH sys-ctrl EXCLUSIVE-LOCK
        WHERE sys-ctrl.name     EQ nk1Name
          AND sys-ctrl.isActive EQ NO
        :
        sys-ctrl.isActive = YES.
    END.
    FIND FIRST settingType EXCLUSIVE-LOCK
         WHERE settingType.settingName EQ settingName
         NO-ERROR.
    IF NOT AVAILABLE settingType THEN NEXT.
    DELETE settingType.
END.
INPUT CLOSE.
