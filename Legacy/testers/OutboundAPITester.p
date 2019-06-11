
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

DEFINE VARIABLE cAPIID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
cAPIID = "AddPurchaseOrder".

RUN api/CallOutBoundAPI.p (
    cAPIId,
    OUTPUT cMessage,
    OUTPUT lSuccess
    ).
    
MESSAGE "Message:" cMessage SKIP
        "Success:" lSuccess
VIEW-AS ALERT-BOX.    
