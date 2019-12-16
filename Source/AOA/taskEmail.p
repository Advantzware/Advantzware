/* taskEmail.p - rstark - 12.14.2018 */

/* prowin.exe spawned by tasker.w monitor */

DEFINE VARIABLE cAttachment          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecipients          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecKey              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject             AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSession             AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAuditID             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSystemEmailConfigID AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.

ASSIGN
    PROPATH     = ENTRY(1,SESSION:PARAMETER,"+")
    cSubject    = ENTRY(2,SESSION:PARAMETER,"+")
    cBody       = ENTRY(3,SESSION:PARAMETER,"+")
    cAttachment = ENTRY(4,SESSION:PARAMETER,"+")
    cRecipients = ENTRY(5,SESSION:PARAMETER,"+")
    cRecKey     = ENTRY(6,SESSION:PARAMETER,"+")
    /* 
      The "emailConfig" table has a record with configID=1 - this record has 
      the same data which "Config" table had. The ticket# 53885 was used for the conversion. 
      The "Config" table is no longer used for the purpose of sending system emails
     */
    iSystemEmailConfigID = 1 
    .
RUN system/session.p PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).

RUN spSendEmail (
    iSystemEmailConfigID, /* emailConfig.ConfigID */
    cRecipients,          /* Override for Email RecipientsinTo */
    "",                   /* Override for Email RecipientsinReplyTo */
    "",                   /* Override for Email RecipientsinCC */
    "",                   /* Override for Email RecipientsinBCC */
    cSubject,             /* Override for Email Subject */
    cBody,                /* Override for Email Body */
    cAttachment,          /* Email Attachment */
    OUTPUT lSuccess,      /* Email success or not */
    OUTPUT cMessage       /* Reason for failure in case email is not sent */
    ).
RUN spCreateAuditHdr ("TASK", "ASI", "TaskEmail", cRecKey, OUTPUT iAuditID).
RUN spCreateAuditDtl (iAuditID, "Subject",    0, cSubject,    "", NO).
RUN spCreateAuditDtl (iAuditID, "Body",       0, cBody,       "", NO).
RUN spCreateAuditDtl (iAuditID, "Attachment", 0, cAttachment, "", NO).
RUN spCreateAuditDtl (iAuditID, "Recipients", 0, cRecipients, "", NO).

DELETE PROCEDURE hSession.

QUIT.
