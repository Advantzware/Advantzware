// ttAuditTable.i - rstark - 3.7.2022

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD auditTable        AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD description       AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD expireDays        AS INTEGER   FORMAT ">>>9"  LABEL "Expire"
    FIELD expireDaysDefault AS INTEGER   FORMAT ">>>9"  LABEL "Default"
    FIELD audit             AS LOGICAL   EXTENT 4
    FIELD auditDefault      AS LOGICAL   EXTENT 4
        INDEX ttTable IS PRIMARY auditTable
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD auditTable   AS CHARACTER
    FIELD auditField   AS CHARACTER FORMAT "x(30)" LABEL "Field"
    FIELD description  AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD audit        AS LOGICAL                  LABEL "Update"
    FIELD auditDefault AS LOGICAL                  LABEL "Default"
        INDEX ttField IS PRIMARY 
            auditTable
            auditField
            .
