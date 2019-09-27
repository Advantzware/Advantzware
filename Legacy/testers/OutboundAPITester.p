
/*------------------------------------------------------------------------
    File        : testers/OutboundAPITester.p
    Purpose     : Outbound API Tester

    Syntax      :

    Description : Outbound API Tester

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 11 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cAPIID            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClientID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTriggerID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cParentProgram    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEventID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPrimaryID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEventDescription AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
FIND LAST oe-relh NO-LOCK NO-ERROR.

ASSIGN
    cParentProgram    = PROGRAM-NAME(1)
    cAPIID            = "SendRelease"
    cClientID         = ""
    cTriggerID        = "PrintRelease"
    cCompany          = "001"
    cLocation         = "MAIN"
    cPrimaryID        = STRING(oe-relh.release#)
    cEventDescription = "Print Release"
    .

RUN api\PrepareAndCallOutboundRequest.p (
    INPUT  cCompany,                /* Company Code (Mandatory) */
    INPUT  cLocation,               /* Location Code (Mandatory) */
    INPUT  cAPIID,                  /* API ID (Mandatory) */
    INPUT  cClientID,               /* Client ID (Optional) - Pass empty in case to make request for all clients */
    INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
    INPUT  "oe-relh",               /* Comma separated list of table names for which data being sent (Mandatory) */
    INPUT  STRING(ROWID(oe-relh)),  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
    INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */   
    INPUT  cEventDescription,       /* Event's description (Optional) */
    INPUT  FALSE,                   /* Re-trigger flag (Mandatory) - Pass TRUE to re-trigger an Outbound Event, Pass FALSE for new API call */
    OUTPUT iEventID,                /* Outbound Event ID generated */
    OUTPUT lSuccess,                /* Success/Failure flag */
    OUTPUT cMessage                 /* Status message */
    ).
  

MESSAGE "Message:" cMessage SKIP
        "Success:" lSuccess SKIP
    VIEW-AS ALERT-BOX.    
