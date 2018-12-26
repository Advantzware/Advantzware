/* taskEmail.p - rstark - 12.14.2018 */

/* prowin.exe spawned by tasker.w monitor */

DEFINE VARIABLE cAttachment AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecKey     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSession    AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAuditID    AS INTEGER   NO-UNDO.

RUN system\session.p PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).

ASSIGN
    cSubject    = ENTRY(1,SESSION:PARAMETER,"+")
    cBody       = ENTRY(2,SESSION:PARAMETER,"+")
    cAttachment = ENTRY(3,SESSION:PARAMETER,"+")
    cRecipients = ENTRY(4,SESSION:PARAMETER,"+")
    cRecKey     = ENTRY(5,SESSION:PARAMETER,"+")
    .
RUN spSendEmail (cSubject, cBody, cAttachment, cRecipients).
RUN spCreateAuditHdr ("TASK", "ASI", "TaskEmail", cRecKey, OUTPUT iAuditID).
RUN spCreateAuditDtl (iAuditID, "Subject",    0, cSubject,    "", NO).
RUN spCreateAuditDtl (iAuditID, "Body",       0, cBody,       "", NO).
RUN spCreateAuditDtl (iAuditID, "Attachment", 0, cAttachment, "", NO).
RUN spCreateAuditDtl (iAuditID, "Recipients", 0, cRecipients, "", NO).
    
QUIT.
