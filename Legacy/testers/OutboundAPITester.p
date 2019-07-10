
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
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

{api/ttArgs.i}

/* ***************************  Main Block  *************************** */
ASSIGN
    cParentProgram = PROGRAM-NAME(1)
    cAPIID         = "SendCustomer"
    .

FIND FIRST Cust NO-LOCK NO-ERROR.
CREATE ttArgs.
ASSIGN
    ttArgs.argType = "ROWID"
    ttArgs.argKey  = "cust"
    ttArgs.argValue = STRING(ROWID(cust))
    .
    
RUN api/PrepareOutboundRequest.p (
    INPUT TABLE ttArgs,
    cAPIId,    
    OUTPUT lcRequestData,
    OUTPUT cMessage,
    OUTPUT lSuccess
    ).

IF NOT lSuccess THEN DO:
   RUN api/CreateAPIOutboundEvent.p (
       INPUT cAPIID,
       INPUT lcRequestData,
       INPUT "",
       INPUT cParentProgram,
       INPUT lSuccess,
       INPUT cMessage,
       INPUT NOW
       ).    
END.
ELSE    
    RUN api/CallOutBoundAPI.p (
        cAPIId,
        lcRequestData,
        cParentProgram,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
MESSAGE "Message:" cMessage SKIP
        "Success:" lSuccess
VIEW-AS ALERT-BOX.    
