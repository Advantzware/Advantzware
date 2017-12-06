/* ttAuditHistory.i */

/* Audit History.rpa */
DEFINE TEMP-TABLE ttAuditHistory NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD AuditID          LIKE AuditHdr.AuditID
    FIELD AuditType        LIKE AuditHdr.AuditType
    FIELD AuditDateTime    LIKE AuditHdr.AuditDateTime
    FIELD AuditUser        LIKE AuditHdr.AuditUser
    FIELD AuditDB          LIKE AuditHdr.AuditDB
    FIELD AuditTable       LIKE AuditHdr.AuditTable
    FIELD AuditField       LIKE AuditDtl.AuditField
    FIELD AuditExtent      LIKE AuditDtl.AuditExtent
    FIELD AuditIdxField    LIKE AuditDtl.AuditIdxField
    FIELD AuditKey         LIKE AuditHdr.AuditKey
    FIELD AuditBeforeValue LIKE AuditDtl.AuditBeforeValue
    FIELD AuditAfterValue  LIKE AuditDtl.AuditAfterValue
    .
