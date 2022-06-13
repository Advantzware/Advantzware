DEFINE {1} TEMP-TABLE ttSetting NO-UNDO 
    FIELD rec_key           AS CHARACTER FORMAT "X(26)"           LABEL "Record Key"
    FIELD settingTypeID     AS INT64     FORMAT ">>>>>>>>>9"      LABEL "Setting Type ID"
    FIELD settingID         AS INT64     FORMAT ">>>>>>>>>9"      LABEL "Setting ID"
    FIELD settingName       AS CHARACTER FORMAT "X(30)"           LABEL "Name"
    FIELD description       AS CHARACTER FORMAT "X(200)"          LABEL "Description"
    FIELD settingTypeDesc   AS CHARACTER FORMAT "X(200)"          LABEL "Description"
    FIELD settingValue      AS CHARACTER FORMAT "X(100)"          LABEL "Value"
    FIELD hasContext        AS LOGICAL   FORMAT "TRUE/FALSE"      LABEL "Has Context?"
    FIELD categoryTags      AS CHARACTER FORMAT "X(1000)"         LABEL "Category Tags"
    FIELD securityLevel     AS INTEGER   FORMAT ">>>>>>>>>9"      LABEL "Security Level"
    FIELD validValues       AS CHARACTER FORMAT "X(30)"           LABEL "Valid Values"
    FIELD defaultValue      AS CHARACTER FORMAT "X(30)"           LABEL "Default Value"
    FIELD isPassword        AS LOGICAL   FORMAT "TRUE/FALSE"      LABEL "Is Password?"
    FIELD dataType          AS CHARACTER FORMAT "X(30)"           LABEL "Data Type"
    FIELD validValueMin     AS CHARACTER FORMAT "X(30)"           LABEL "Minimum Valid Value"
    FIELD validValueMax     AS CHARACTER FORMAT "X(30)"           LABEL "Maximum Valid Value"
    FIELD programID         AS CHARACTER FORMAT "X(30)"           LABEL "Program"
    FIELD scopeID           AS INT64     FORMAT ">>>>>>>>>9"      LABEL "Scope ID"
    FIELD inactive          AS LOGICAL   FORMAT "Inactive/Active" LABEL "Status"
    FIELD settingUser       AS CHARACTER FORMAT "X(30)"           LABEL "User"
    FIELD scopeTable        AS CHARACTER FORMAT "X(30)"           LABEL "Scope Type"
    FIELD scopeField1       AS CHARACTER FORMAT "X(30)"           LABEL "Scope Field 1"
    FIELD scopeField2       AS CHARACTER FORMAT "X(30)"           LABEL "Scope Field 2"
    FIELD scopeField3       AS CHARACTER FORMAT "X(30)"           LABEL "Scope Field 3"
    FIELD isUserGroup       AS LOGICAL   FORMAT "TRUE/FALSE"      LABEL "User Group?"
    FIELD isUserGroupMember AS LOGICAL   FORMAT "TRUE/FALSE"      LABEL "Is current user a member or user group?" 
    FIELD recordSource      AS CHARACTER FORMAT "X(30)"
    FIELD priorityID        AS INTEGER   FORMAT ">>>9"
    FIELD allData           AS CHARACTER
    INDEX settingID IS PRIMARY settingID
    INDEX settingOrder settingName priorityID DESCENDING inactive programID DESCENDING settingUser DESCENDING isUserGroup   
    INDEX settingType settingTypeID settingID settingUser scopeTable scopeField1 scopeField2 scopeField3
    INDEX recordSource recordSource
    .
DEFINE TEMP-TABLE ttSettingUsage NO-UNDO LIKE ttSetting
    FIELD stackTrace AS CHARACTER
    INDEX settingName settingTypeID settingName scopeTable scopeField1 scopeField2 scopeField3
    .
