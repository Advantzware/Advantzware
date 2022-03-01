
/*------------------------------------------------------------------------
    File        : ProcessOutboundRequest.p
    Purpose     : 

    Syntax      :

    Description : Procedure to make an outbound call

    Author(s)   : DEVA$!
    Created     : Fri Jul 23 04:59:43 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.    
DEFINE INPUT  PARAMETER ipcScopeID          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcScopeType        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO. 
   
DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
    INPUT  ipcCompany,
    INPUT  ipcLocation,
    INPUT  ipcAPIID,
    INPUT  ipcScopeID,
    INPUT  ipcScopeType,
    INPUT  ipcTriggerID,
    INPUT  ipcTableList,
    INPUT  ipcROWIDList,
    INPUT  ipcPrimaryID,
    INPUT  ipcEventDescription,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ) NO-ERROR.
        
DELETE PROCEDURE hdOutboundProcs.