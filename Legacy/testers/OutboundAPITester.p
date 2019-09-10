
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

DEFINE VARIABLE cAPIID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClientID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTriggerID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEventID       AS INTEGER   NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
ASSIGN
    cParentProgram = PROGRAM-NAME(1)
    cAPIID         = "SendRelease"
    cClientID      = ""
    cTriggerID     = "PrintRelease"
    cCompany       = "001"
    cLocation      = "MAIN"
    .

FIND LAST oe-relh NO-LOCK NO-ERROR.

RUN api\PrepareAndCallOutboundRequest.p (
    cCompany,
    cLocation,
    cAPIID,
    cClientID,
    cTriggerID,
    "oe-relh",
    STRING(ROWID(oe-relh)),
    FALSE,
    OUTPUT iEventID,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).
  

MESSAGE "Message:" cMessage SKIP
        "Success:" lSuccess SKIP
    VIEW-AS ALERT-BOX.    
