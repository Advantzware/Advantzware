/* ttAudit.i - used in AOA/param/AuditHist.w & system/Audit.w */

DEFINE TEMP-TABLE ttUser NO-UNDO
    FIELD AuditUser AS CHARACTER
        INDEX AuditUser IS PRIMARY AuditUser
        .
DEFINE TEMP-TABLE ttAudit NO-UNDO
    FIELD AuditDB    AS CHARACTER
    FIELD AuditTable AS CHARACTER
    FIELD AuditField AS CHARACTER
        INDEX ttAudit IS PRIMARY AuditDB AuditTable AuditField
        .
