/*------------------------------------------------------------------------
    File        : api\inbound\CreateBillOfLading.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Mon Sep 16 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opiBOLID     AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdOrderProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidReleaseID  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
  
/* This will eventually move to setsession approach */
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
  
DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.
  
g_company=ipcCompany.
  
RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  
RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).
{sys/inc/var.i "new shared"}

RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.

ASSIGN
    oplSuccess      = YES
    lValidReleaseID = YES 
    lValidCompany   = YES
    .

/* Validates company */
lValidCompany = CAN-FIND(FIRST company NO-LOCK
                         WHERE company.company EQ ipcCompany).
        
IF NOT lValidCompany THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company"
        oplSuccess = NO
        .
    
    RETURN.
END.

/* Validates releaseID */
lValidReleaseID = CAN-FIND(FIRST oe-relh NO-LOCK
                           WHERE oe-relh.company  EQ ipcCompany
                             AND oe-relh.release# EQ ipiReleaseID).
        
IF NOT lValidReleaseID THEN DO:
    ASSIGN 
        opcMessage = "Invalid ReleaseID"
        oplSuccess = NO
        .
    
    RETURN.
END.

IF NOT oplSuccess THEN
    RETURN.

/* Checks whether release is already posted or not */
FIND FIRST oe-relh NO-LOCK
     WHERE oe-relh.company  EQ ipcCompany
       AND oe-relh.release# EQ ipiReleaseID
       AND NOT oe-relh.posted  
       AND oe-relh.printed
       NO-ERROR.

IF NOT AVAILABLE oe-relh THEN DO:
    ASSIGN 
        opcMessage = "Release is not available for posting"
        oplSuccess = NO
        .

    RETURN.

END.

/* Posts releases & Creates BOL */ 
RUN OrderProcsPostReleases IN hdOrderProcs (
    INPUT  ipcCompany,
    INPUT  ipiReleaseID,
    INPUT  FALSE, /* Create Lines from ssrelbol table */
    INPUT  ipcUserName,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT opiBOLID
    ).

IF oplSuccess AND 
   NOT oe-relh.posted THEN DO:
    ASSIGN 
        opcMessage = "Release is not posted"
        oplSuccess = NO
        .

    RETURN.
END.   
      
DELETE PROCEDURE hdOrderProcs.
