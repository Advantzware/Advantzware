/* ttAudit.i - used in AOA/param/AuditHist.w & system/Audit.w */

DEFINE TEMP-TABLE ttAuditDBTable NO-UNDO
    FIELD AuditDB    AS CHARACTER
    FIELD AuditTable AS CHARACTER
    FIELD TableName  AS CHARACTER 
        INDEX ttAudit IS PRIMARY AuditDB AuditTable
        .
DEFINE TEMP-TABLE ttAudit NO-UNDO
    FIELD AuditDB    AS CHARACTER
    FIELD AuditTable AS CHARACTER
    FIELD TableName  AS CHARACTER
    FIELD AuditField AS CHARACTER
    FIELD FieldName  AS CHARACTER
    FIELD DBTable    AS LOGICAL
        INDEX ttAudit IS PRIMARY AuditDB AuditTable AuditField
        .
