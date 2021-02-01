/* taskEmail.p - rstark - 12.14.2018 */

DEFINE INPUT PARAMETER ipcSubject    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBody       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAttachment AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecipients AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKey     AS CHARACTER NO-UNDO.

DEFINE VARIABLE hSession             AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAuditID             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSystemEmailConfigID AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.

RUN spSendEmail (
    iSystemEmailConfigID, /* emailConfig.ConfigID */
    ipcRecipients,        /* Override for Email RecipientsinTo */
    "",                   /* Override for Email RecipientsinReplyTo */
    "",                   /* Override for Email RecipientsinCC */
    "",                   /* Override for Email RecipientsinBCC */
    ipcSubject,           /* Override for Email Subject */
    ipcBody,              /* Override for Email Body */
    ipcAttachment,        /* Email Attachment */
    OUTPUT lSuccess,      /* Email success or not */
    OUTPUT cMessage       /* Reason for failure in case email is not sent */
    ).
RUN spCreateAuditHdr ("TASK", "ASI", "TaskEmail", ipcRecKey, OUTPUT iAuditID).
RUN spCreateAuditDtl (iAuditID, "Subject",    0, ipcSubject,    "", NO).
RUN spCreateAuditDtl (iAuditID, "Body",       0, ipcBody,       "", NO).
RUN spCreateAuditDtl (iAuditID, "Attachment", 0, ipcAttachment, "", NO).
RUN spCreateAuditDtl (iAuditID, "Recipients", 0, ipcRecipients, "", NO).
IF lSuccess EQ NO THEN
RUN spCreateAuditDtl (iAuditID, "Error", 0, cMessage, "", NO).
