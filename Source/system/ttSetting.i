DEFINE {1} TEMP-TABLE ttSetting NO-UNDO 
    FIELD rec_key         AS CHARACTER LABEL "Record Key"
    FIELD settingTypeID   AS INT64     LABEL "Setting Type ID"
    FIELD settingID       AS INT64     LABEL "Setting ID"
    FIELD settingName     AS CHARACTER LABEL "Name"
    FIELD description     AS CHARACTER LABEL "Description"
    FIELD settingTypeDesc AS CHARACTER LABEL "Description"
    FIELD settingValue    AS CHARACTER LABEL "Value"
    FIELD hasContext      AS LOGICAL   LABEL "Has Context?"
    FIELD categoryTags    AS CHARACTER LABEL "Category Tags"
    FIELD securityLevel   AS INTEGER   LABEL "Security Level"
    FIELD validValues     AS CHARACTER LABEL "Valid Values"
    FIELD defaultValue    AS CHARACTER LABEL "Default Value"
    FIELD isPassword      AS LOGICAL   LABEL "Is Password?"
    FIELD dataType        AS CHARACTER LABEL "Data Type"
    FIELD validValueMin   AS CHARACTER LABEL "Minimum Valid Value"
    FIELD validValueMax   AS CHARACTER LABEL "Maximum Valid Value"
    FIELD programID       AS CHARACTER LABEL "Program"
    FIELD scopeID         AS INT64     LABEL "Scope ID"
    FIELD inactive        AS LOGICAL   LABEL "Status" FORMAT "Inactive/Active"
    FIELD settingUser     AS CHARACTER LABEL "User"
    FIELD scopeTable      AS CHARACTER LABEL "Scope Type"
    FIELD scopeField1     AS CHARACTER LABEL "Scope Field 1"
    FIELD scopeField2     AS CHARACTER LABEL "Scope Field 2"
    FIELD scopeField3     AS CHARACTER LABEL "Scope Field 3"
    FIELD recordSource    AS CHARACTER 
    FIELD allData         AS CHARACTER
    INDEX settingID IS PRIMARY settingID
    INDEX settingType settingTypeID settingID settingUser scopeTable scopeField1 scopeField2 scopeField3
    INDEX recordSource recordSource
    .
